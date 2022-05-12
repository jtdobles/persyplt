open Ast
open Sast
open Buildtbl

let check (statements, functions) = 

let symbol_table = get_decls (statements, functions) in
let global_scope = fst symbol_table in
let function_scopes = snd symbol_table in

let rec check_return slst ret = match slst with 
    Return _ :: _ -> if ret != Void then true else raise(Failure "Function of type Nah should not have a return statement") 
  | s :: ss -> check_return ss ret 
  | [] -> if ret = Void then true else raise (Failure "Function has an empty body at the highest level but returns (?)") in 

let check_expr_scope scop = function 
    DecAssign(ty, s, _) -> add_symbol_to_tbl s ty scop 
  | _ -> scop in 

let rec check_stmt_scope scop = function 
    Expr(e) -> check_expr_scope scop e 
  | Dec(ty, s) -> add_symbol_to_tbl s ty scop
  | While(p, _, _) -> check_expr_scope scop p
  | If(p, _, _) -> check_expr_scope scop p
  | BBlock(sl) -> List.fold_left (fun m f -> check_stmt_scope m f) scop sl
  | _ -> scop in

let ret_func = function 
    Function(e) -> e 
  | e           -> e 
  | _           -> raise (Failure "function return type is flawed")
in

let check_bool e = 
  match e with
      (ty, l) -> if ty = Bool then (ty, l) else raise (Failure "value must be a bool")
in

let check_declassign ty1 s ex1 = 
  match ex1 with
    | (ty2, _) when ty1 = ty2 -> (ty1, SDecAssign(ty1, s, ex1)) 
    | (ty2, _) -> raise (Failure ("lvalue " ^ string_of_typ ty1 ^ " not equal to rvalue " ^ string_of_typ ty2)) 
in

let check_assign lvaluet rvaluet  =
  if lvaluet = rvaluet then lvaluet else raise (Failure "wrong assignment")  
in

let rec check_expr scop sscop e = match e with
    IntLit l -> (Int, SIntLit l)
  | BoolLit l -> (Bool, SBoolLit l) 
  | FloatLit l -> (Float, SFloatLit l)
  | StringLit l -> (String, SStringLit l)
  | Noexpr -> (Void, SNoexpr)
  | Id l -> (toi scop l, SId l)
  | Binop(e1, op, e2) as e -> 
    let (t1, e1') = check_expr scop sscop e1 
    and (t2, e2') = check_expr scop sscop e2 in
    let same = t1 = t2 in
    let ty = match op with
        Add | Sub | Mult | Div | Mod when same && t1 = Int   -> Int
      | Add | Sub | Mult | Div | Mod when same && t1 = Float -> Float
      | Equal | Neq                  when same               -> Bool
      | Less | Leq | Greater | Geq   when same && (t1 = Int || t1 = Float) -> Bool
      | And | Or                     when same && t1 = Bool -> Bool
      | _ -> raise (Failure ("illegal binary operator " ^
                    string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                    string_of_typ t2 ^ " in " ^ string_of_expr e))
    in (ty, SBinop((t1, e1'), op, (t2, e2')))
  | Unop(uop, e) as ex -> 
    let (t, e') = check_expr scop sscop e in
    let ty = match uop with
        Neg | Incr | Decr when t = Int || t = Float -> t
      | Not when t = Bool -> Bool
      | _ -> raise (Failure ("illegal unary operator " ^ 
                            string_of_uop uop ^ string_of_typ t ^
                            " in " ^ string_of_expr ex))
    in (ty, SUnop(uop, (t, e')))
  | Assign(s, e) -> 
    let lt = toi scop s 
    and (rt, e') = check_expr scop sscop e in
    (check_assign lt rt, SAssign(s, (rt, e')))

  | OpAssign(s, op, e) -> 
    let (t, e1) = check_expr scop sscop e in 
    if t = (toi scop s) then (t, SOpAssign(s, op, (t, e1))) else raise (Failure "types not the same") 

  | DecAssign(ty, l, expr1) -> (match ty, l, expr1 with
    | _ -> check_declassign ty l (check_expr scop sscop expr1)
    )
  | Call(fname, args) -> 
    let eval_list = List.map (check_expr scop sscop) args in 
    let key_func = key_string fname eval_list in  
    let fd = StringMap.find key_func function_scopes in
    let param_length = StringMap.cardinal fd.formals.variables in
    if List.length args != param_length then
    raise (Failure ("expecting " ^ string_of_int param_length ^ 
              " arguments in function call" ))
    else let check_call (_, ft) e = 
    let (et, e') = check_expr scop sscop e in 
    (check_assign ft et, e')
    in 
    let args' = List.map2 check_call (StringMap.bindings fd.formals.variables) args
    in (ret_func fd.ret_typ, SCall(fname, args')) 
  | _  -> raise (Failure "expression is not an expression")  
in

let rec check_stmt scop loopscop s =
  let new_scope = {
    variables = StringMap.empty;
    parent = Some(scop);
  } in match s with 
  Expr e -> SExpr (check_expr scop loopscop e) 
  | If(p, b1, b2) as i -> 
    let scop = get_expr_decls scop p in
    let pred = check_bool (check_expr scop loopscop p) 
    and t = check_stmt scop loopscop b1
    and f = check_stmt scop loopscop b2 in SIf(pred, t, f) 
  | While(p, s, inc) -> 
      let scop = get_expr_decls scop p in
      let pred = check_bool (check_expr scop loopscop p)
      and loop = check_stmt scop true s in SWhile(pred, loop, check_stmt scop loopscop inc) 
  | Cont e -> if loopscop then SCont (check_expr scop loopscop e) else raise (Failure "cont not in a loop")  
  | Exit e -> if loopscop then SExit (check_expr scop loopscop e) else raise (Failure "exit not in a loop") 
  | Return _ -> raise (Failure "return outside a function")
  | Block sl -> 
    let rec check_stmt_list bscop = function
        [Return _ as s] -> [check_stmt bscop loopscop s]
      | Return _ :: _   -> raise (Failure "nothing may follow a return")
      | s :: ss         -> check_stmt bscop loopscop s :: check_stmt_list bscop ss 
      | [] -> []
    in SBlock(check_stmt_list (List.fold_left (fun m f -> check_stmt_scope m f) new_scope sl) sl)
  | BBlock sl -> 
    SBlock(List.map (check_stmt scop loopscop) sl)
    | Dec(ty, l) -> SDec(ty, l)
  | _ as s -> raise (Failure ("statement " ^ string_of_stmt s ^ " is not a statement"))
in

let rec check_stmt_func scop loopscop ret = 
  let new_scope = {
    variables = StringMap.empty;
    parent = Some(scop);
  } in function 
    Expr e -> SExpr (check_expr scop loopscop e) 
  | If(p, b1, b2) -> SIf(check_bool (check_expr scop loopscop p), check_stmt_func scop loopscop ret b1,check_stmt_func scop loopscop ret b2)
  | While(p, s, inc) -> 
    SWhile(check_bool (check_expr scop loopscop p), check_stmt_func new_scope true ret s, check_stmt scop loopscop inc)
    | Return e -> let (t, e') = check_expr scop loopscop e in 
    if t = ret then SReturn (t, e') 
    else raise (
    Failure ("return gives " ^ string_of_typ t ^ " expected " ^
      string_of_typ ret ^ " in " ^ string_of_expr e)) 
  | Cont e -> if loopscop then SCont (check_expr scop loopscop e) else raise (Failure "cont not in aloop")  
  | Exit e -> if loopscop then SExit (check_expr scop loopscop e) else raise (Failure "exit not in a loop") 
  | Block sl -> 
    let rec check_stmt_list bscop = function
        [Return _ as s] -> [check_stmt_func bscop loopscop ret s]
      | Return _ :: _   -> raise (Failure "nothing may follow a return")
      | s :: ss         -> check_stmt_func bscop loopscop ret s :: check_stmt_list bscop ss
      | []              -> []
    in SBlock(check_stmt_list (List.fold_left (fun m f -> check_stmt_scope m f) new_scope sl) sl)
  | BBlock sl -> SBlock(List.map (check_stmt_func scop loopscop ret) sl)
  | Dec(ty, l) -> SDec(ty, l)
  | _ as s -> raise (Failure ("statement " ^ string_of_stmt s ^ " is not a statement"))
in

let check_func (fd : func_decl) = 
  if check_return fd.fstmts (ret_func fd.typ) then 
    let key_func = key_string fd.fname fd.formals in 
      let current_function = StringMap.find key_func function_scopes in 
      { styp = ret_func fd.typ;
        sfname = fd.fname;
        sformals = fd.formals;
        sfstmts = match check_stmt_func current_function.locals false (ret_func fd.typ) (Block fd.fstmts) with
	          SBlock(sl) -> sl
          | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
    else raise (Failure "there is not return statement at the highest level of the function")
in 

(* SAST of semantically-check statements and functions *)
let sstatements = List.map (check_stmt global_scope false) statements in
let sfuncs = List.map check_func functions in

(* def implicit main function if it's not defined; main takes int like C *)
let rec has_main sfuncs = match sfuncs with
    [] -> false
  | sfd :: _ when sfd.sfname = "main" && sfd.styp = Int -> true
  | sfd :: _ when sfd.sfname = "main" -> raise (Failure ("Error: function main must have eturn type int, not type " ^ string_of_typ sfd.styp))
  | _ :: tail -> has_main tail in

let updated_sfuncs = if has_main sfuncs then sfuncs else
  { styp = Int;
    sfname = "main";
    sformals = [];
    sfstmts = List.rev sstatements;
  } :: sfuncs in

(sstatements, updated_sfuncs)