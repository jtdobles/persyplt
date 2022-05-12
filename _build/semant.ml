(* Semantic checking for the D compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

let check(structs, functions) = 
  let add_struct map sd = 
    let dup_err = "found duplicated struct"
    and flag_err err = raise (Failure err)
    and st = sd.stname
    in match sd with
        _ when StringMap.mem sd.stname map -> flag_err dup_err
      | _ -> StringMap.add st sd map
  in
  let check_struct st =
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name (ty, name, 0) m) StringMap.empty st.members
    in
      {
        sstname = st.stname;
        smembers = st.members;
      }
  in

  let add_func map fd =
    let std_lib_err = "function " ^ fd.fname ^ " cannot be redefined"
    and dup_err = "function already exists: " ^ fd.fname
    and flag_err err = raise (Failure err)
    and n = fd.fname
    in match fd with
      _ when StringMap.mem n map -> flag_err dup_err
    | _ -> StringMap.add n fd map
  in

  let std_lib_funcs = List.fold_left add_func StringMap.empty [
    {rtyp = Void; fname = "printi";  formals = [(Int, "args")]; fstmts = []};
    {rtyp = Void; fname = "printf";  formals = [(Float, "args")]; fstmts = []};
    {rtyp = Void; fname = "prints";  formals = [(String, "args")]; fstmts = []};
    {rtyp = Int;  fname = "leng";    formals = [(Array(Int), "args")]; fstmts = []};
  ]
  in

  let function_decls = List.fold_left add_func std_lib_funcs functions in

  let find_func fn =
    try StringMap.find fn function_decls
    with Not_found -> raise (Failure("No such function exist " ^ fn))
  in
  (* main func decl checker *)
  let _ = find_func "main" in

  let check_function func =
    let add_var map (tp, name, len) =
      let dup_err = "duplicate variable found: " ^ name in
      match (tp, name) with 
        _ when StringMap.mem name map -> raise (Failure dup_err)
      | _ -> StringMap.add name (tp, name, len) map
    in
    
    let find_var map name =
      try StringMap.find name map
      with Not_found -> raise( Failure("No such variable exists: " ^ name))
    in

    let check_assign lvalt rvalt err =
      if lvalt = rvalt then lvalt else raise (Failure err)
    in

    let type_of_identifier s symbols =
      let (ty, _, _) = try StringMap.find s symbols 
                       with Not_found -> raise( Failure("No such ID exists: " ^ s))
      in ty
    in

    let rec check_expr map e = match e with
      Noexpr(ty) ->    (ty, SNoexpr(ty), map)
      | Not(e) as notEx-> let (t, e', map') = check_expr map e in
        if t != Bool then 
          raise(Failure("bool expr not found in " ^ string_of_expr notEx))
        else (Bool, SNot((t, e')), map')
      | IntLit l ->    (Int, SIntLit l, map)
      | FloatLit l ->  (Float, SFloatLit l, map)
      | StringLit l -> (String, SStringLit l, map)
      | BoolLit l ->   (Bool, SBoolLit l, map)
      | ArrayLit(l) ->
        let array_body = List.map (check_expr map) l in
        let array_type, _, _ = List.nth array_body 0 in
            (Array array_type, SArrayLit(List.map (fun (t, sx, _) -> (t, sx)) array_body), map)
      | Id s -> (type_of_identifier s map, SId s, map)
      | Assign(v, e) ->
        let lt, vname, map1 = find_name v map "assignment err" in
        let rt, ex, map2 = check_expr map1 e in
        (check_assign lt rt "data type missmatch", SAssign((lt, vname), (rt, ex)), map2)

      | OpAssign(v, op, e) ->
        let lt, vname, map1 = find_name v map "assignment err" in
        let rt, ex, map2 = check_expr map1 e in
        (check_assign lt rt "data type missmatch", SOpAssign((lt, vname), (rt, ex)), map2)

      | ArrayAssign(v, i, e) ->
        let strName = match v with
          Id i -> i
          | _ -> raise(Failure("Invalid array identifier: " ^ string_of_expr v))
        in
        let lt, vname, map1 = find_name v map "assignment err" in
        let rt, ex, map2 = check_expr map1 e in
        let it, ix, map3 = check_expr map2 i in
        let (idx_typ, sidx, _) = check_expr map i in
          let _ = match sidx with
            SIntLit l ->
              let (_, _, arsize) = StringMap.find strName map in
              if l >= arsize && arsize != 0 then raise(Failure("Array Index out ouf bound: " ^ string_of_int l))
              else l
            | _ -> 0
          in
        let elem_type = (match lt with
            Array(t) -> t
            | _ -> raise(Failure("got type: " ^ string_of_typ lt))
            )
        in
        (check_assign elem_type rt "array type missmatch", SArrayAssign((lt, vname), (it, ix), (rt, ex)), map3)

      | ArrayIndex(arname, idx) ->
        let strName = match arname with
          Id i -> i
          | _ -> raise( Failure("Invalid array identifier: " ^ string_of_expr arname))
        in
        let (typ, sid, map1) = check_expr map arname in
        let (idx_typ, sidx, map2) = check_expr map1 idx in
        let _ = match sidx with
          SIntLit l ->
            let (_, _, size) = StringMap.find strName map in
            if l >= size && size != 0 then raise(Failure("Array Index out ouf bound: " ^ string_of_int l))
            else l
          | _ -> 0
        in

        let elem_type = match typ with
          Array(t) -> t
          | _ -> raise(Failure("Unexpected type: " ^ string_of_typ typ))
        in
        (elem_type, SArrayIndex((typ, sid), (idx_typ, sidx)), map2)
      | StructAssign(v, m, e) ->
        let strName = match v with
          Id i -> i
          | _ -> raise(Failure("Invalid struct identifier: " ^ string_of_expr v)) 
        in
        let lt, vname, map1 = find_name v map "assignment err" in
        let rt, ex, map2 = check_expr map1 e in
      (check_assign Ast.Int Ast.Int "struct type missmatch", SStructAssign((lt, vname), m, (rt, ex)), map2)
      | StructUse(v, m) ->
        let strName = match v with
          Id i -> i
          | _ -> raise(Failure("Invalid struct identifier: " ^ string_of_expr v))
        in
        let lt, vname, map1 = find_name v map "assignment err" in
      (Int, SStructUse((lt, vname), m), map1)
      (*| Unop(uop, e) as ex ->
          let (t, e') = check_expr map e in
          let ty = match uop with
            Incr | Decr when t = Int || t = Float -> t
            | _ -> raise(Failure("illegal unary op " ^ string_of_uop uop ^ string_of_typ t ^ " in " ^ string_of_expr ex)) 
          in 
          (ty, SUnop(uop, (t, e')), map')*)
      | Binop(e1, op, e2) as ex ->
        let (t1, e1', map') = check_expr map e1 in
        let (t2, e2', map'') = check_expr map' e2 in
        let same = t1 = t2 in
        let ty = match t1 with 
          _ -> match op with
            Add | Sub | Mult | Div | Mod when same && t1 = Int -> Int
            | Add | Sub | Mult | Div     when same && t1 = Float -> Float
            | Add                        when same && t1 = String -> String
            | Eq | Neq                   when same -> Bool
            | Lt | Leq | Gt | Geq        when same && (t1 = Int || t1 = Float) -> Bool
            | And | Or                   when same && t1 = Bool -> Bool
            | _ -> raise (Failure ("Illegal binary operator " ^ string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^ string_of_typ t2 ^ " in " ^ string_of_expr ex))
        in
        (ty, SBinop((t1, e1'), op, (t2, e2')), map'')
      | Call(fname, args) as call ->
          let fd = find_func fname in
          let param_len = List.length fd.formals in
          if List.length args != param_len then
            raise(Failure("expecting to have " ^ string_of_int param_len ^ " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e =
            let (et, e', map') = check_expr map e in
            let arg_err = "Illegal argument: " ^ string_of_typ et ^ "; expect to have " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in
            (check_assign ft ft arg_err, e')
          in 
          let args' = List.map2 check_call fd.formals args in 
          (fd.rtyp, SCall(fname, args'), map)
    and find_name (name) map err = match name with
          Id _ -> check_expr map name
          | _ -> raise(Failure("find_name err"))
    in
    let check_bool map e =
      let (t', e', map') = check_expr map e
      and err = "Expected boolran expression in " ^ string_of_expr e 
      in
      if Bool != Bool then raise(Failure err) else (t', e')
    in
    let rec check_stmt map st = match st with
      | Block sl ->
        let rec check_stmt_list map sl = match sl with
          [] -> ([], map)
          | [Return _ as s] -> ([fst (check_stmt map s)], map)
          | Return _ :: _ -> raise(Failure "empty return")
          | Block sl :: ss -> check_stmt_list map (sl @ ss)
          | s :: ss -> let cs, m' = check_stmt map s in
                      let csl, m'' = check_stmt_list m' ss in
                      (cs :: csl, m'')
        in
        (SBlock(fst (check_stmt_list map sl)), map)
      | VarDecl(tp, id, e) ->
        let (rty, sexpr, map') = check_expr map e in
        let arg_err = "illegal argument found" in
        let len = match e with
          Ast.ArrayLit art -> List.length art
          | _ -> 0 
        in
        let nmap = add_var map' (tp, id, len) in
        let r_expr = (rty, sexpr) in
        (SVarDecl(tp, id, r_expr), nmap)
      | ArrayDecl(t, id, e1, e) ->
        let(ty', e1', _) = check_expr map e1 in
          if ty' != Ast.Int then raise(Failure("Integer is expected but program received: " ^ string_of_typ t))
          else
            let len = match e1 with
              Ast.IntLit t -> t
            in
        let nmap = add_var map (t, id, len) in
        let (t2, sx2, map') = check_expr map e in
        let r2 = (t2, sx2) in
        (SArrayDecl(t, id, (ty', e1'), r2), nmap)
      | If(cond, s1, s2) -> let sthen, _ = check_stmt map s1 in
                            let slese, _ = check_stmt map s2 in
                            (SIf(check_bool map cond, sthen, slese), map)
      | For(e1, e2, e3, stmtlst) -> let (st1, m') = check_stmt map e1 in
                                    let (ty3, sx3, m'') = check_expr m' e3 in
                                    SFor(st1, check_bool m'' e2, (ty3, sx3), fst (check_stmt m'' stmtlst)), m''
      | While(cond, stmtlst) -> SWhile(check_bool map cond, fst (check_stmt map stmtlst)), map
      (*| Switch(cond, stmtlst) -> SSwitch(check_bool map cond, fst (check_stmt map stmtlst)), map
      | Case(cond, stmtlst) -> SSwitch(check_bool map cond, fst (check_stmt map stmtlst)), map
      | Default(cond, stmtlst) -> SDefault(check_bool map cond, fst (check_stmt map stmtlst))*)
      | Expr e -> let (ty, sexpr, nmap) = check_expr map e in 
                  (SExpr (ty, sexpr), nmap)
      | Return e -> let (t, e', map') = check_expr map e in
                    if t = func.rtyp then (SReturn (t, e'), map')
                    else raise(Failure("return type expected to be " ^ string_of_typ func.rtyp ^ "but " ^ string_of_typ t ^ " was given; error in " ^ string_of_expr e))
      | _ -> raise(Failure("Statement match failed"))
    in
    
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name (ty, name, 0) m) StringMap.empty func.formals
    in
      {
        srtyp = func.rtyp;
        sfname = func.fname;
        sformals = func.formals;
        sfstmts = match fst (check_stmt symbols (Block(func.fstmts))) with
          SBlock(sl) -> sl
          | _ -> let block_err = "Block building failed" in
          raise(Failure block_err)
      }
  in (* check_func in *)
  let ssfuncs = List.map check_function functions in
  let sstructs = List.map check_struct structs in
  (sstructs, ssfuncs)