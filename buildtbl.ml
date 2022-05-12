
open Ast

module StringMap = Map.Make(String)

(* scoping check table *)
type scope_table = {
  variables : typ StringMap.t;
  parent : scope_table option;
}

let rec string_of_scope scope = 
  let print_bind name ty str = ("(" ^ name ^ " " ^ (string_of_typ ty) ^ ")\n" ^ str) in
  let this_scope = StringMap.fold (fun k t s -> print_bind k t s) scope.variables "" in 
  let parent_scope = match scope.parent with
      Some(parent) -> string_of_scope parent
    | None -> ""
  in "{" ^ this_scope ^ "}\n|\nV\n{" ^ parent_scope ^ "}"  


(* function declarations table *)
type func_table = {
  formals : scope_table;
  locals : scope_table;
  ret_typ : typ;
}

let illegal_func_names = ["printn"; "printi"; "prints"; "printb"; "printf"; ]

let rec toi scope s =
  if StringMap.mem s scope.variables then
    StringMap.find s scope.variables 
  else match scope.parent with
      Some(parent) -> toi parent s 
    | _ -> raise (Failure ("Variable " ^ s ^ " not found"))

let rec string_of_params params = match params with
    (typ, _) :: [] -> string_of_typ typ
  | (typ, _) ::  p -> string_of_typ typ ^ ", " ^ string_of_params p
  | _              -> "" 
and key_string name params = name ^ " (" ^ string_of_params params ^ ")"

let rec string_of_params_built_in params = match params with
  | typ :: []   -> string_of_typ typ 
  | typ :: p    -> string_of_typ typ ^ ", " ^ string_of_params_built_in p
  | _     -> ""
  and key_string_built_in_functions name params = name ^ " (" ^ string_of_params_built_in params ^ ")"

let rec is_valid_dec name scope = 
  if StringMap.mem name scope.variables then
    raise (Failure ("Error: variable " ^ name ^ " is already defined"))
  else match scope.parent with
      Some(parent) -> is_valid_dec name parent
    | _ -> true

let add_symbol name ty scope = match ty with
    Void -> raise (Failure ("Error: variable " ^ name ^ " declared with type Void"))
  | _  -> if is_valid_dec name scope then {
      variables = StringMap.add name ty scope.variables;
      parent = scope.parent;
    } else scope 

  let add_symbol_to_tbl name ty scope = match ty with
    Void -> raise (Failure ("Error: variable " ^ name ^ " declared with type void"))
  | _  ->  {
      variables = StringMap.add name ty scope.variables;
      parent = scope.parent;
    } 

let get_bind_decs scope bind = 
  let ty, name = bind in add_symbol name ty scope

let rec get_expr_decls scope expr = 
  match expr with
    Binop(e1, _, e2) -> 
    let expr_list = [e1; e2] in List.fold_left get_expr_decls scope expr_list
  | Unop(_, e) -> get_expr_decls scope e
  | Assign(_, e) -> get_expr_decls scope e
  | OpAssign(_, _, e) -> get_expr_decls scope e
  | DecAssign(ty, name, e) -> 
    let updated_scope = get_expr_decls scope e in add_symbol name ty updated_scope
  | Call(_, expr_list) -> List.fold_left get_expr_decls scope expr_list
  | _ -> scope

let rec get_stmt_decs scope stmt =
  let new_scope = {
    variables = StringMap.empty;
    parent = Some(scope);
  } in match stmt with
  Block(s_list) -> 
    let _ = List.fold_left get_stmt_decs new_scope s_list in scope
  | BBlock(s_list) -> 
    let _ = List.fold_left get_stmt_decs new_scope s_list in scope
  | Expr(e) -> get_expr_decls scope e
  | Dec(ty, name) -> add_symbol name ty scope
  | If(cond, then_s, else_s) -> 
    let cond_scope = get_expr_decls new_scope cond in
    let _ = (get_stmt_decs cond_scope then_s, get_stmt_decs cond_scope else_s) in scope
  | While(e, s, _) -> 
    let while_scope = get_expr_decls new_scope e in 
    let _ = get_stmt_decs while_scope s in scope
  | _ -> scope

let get_vars scope s_list = List.fold_left get_stmt_decs scope s_list

let valid_func_name fd map = 
  let rec unused_name name illegals = match illegals with
      [] -> ()
    | illegal_name :: _ when name = illegal_name ->
      raise (Failure ("Error: illegal function name " ^ name))
    | _ :: tail -> unused_name name tail
  in let _ = unused_name fd.fname illegal_func_names in
  let key = key_string fd.fname fd.formals in
  if StringMap.mem key map then 
    raise (Failure("Error: function " ^ fd.fname ^ " is already defined with formal arguments (" ^ 
                   (string_of_params fd.formals) ^ ")"))
  else key

  let build_func_table global_scope (fd : func_decl) map = 
    let key = valid_func_name fd map in
    let formals_scope = List.fold_left get_bind_decs {
        variables = StringMap.empty;
        parent = None;
      } fd.formals in
    let updated_scope = { 
      variables = formals_scope.variables;
      parent = Some(global_scope); } in
    let locals_scope = get_vars {
        variables = StringMap.empty;
        parent = Some(updated_scope);
      } fd.fstmts in StringMap.add key {
      formals = updated_scope;
      locals = locals_scope;
      ret_typ = fd.typ;
    } map

let get_decls (s_list, f_list) = 
  let globals = get_vars {
      variables = StringMap.empty;
      parent = None;
    } (List.rev s_list) in

  (* D's built-in functions *)
  let built_in_funcs = 
    let build_built_in_func_table map (name, param_typ, typ) = 
      let args = List.fold_left 
          (fun m f -> let param_name = ("p" ^ string_of_int (StringMap.cardinal m.variables)) in add_symbol param_name f m) {
          variables = StringMap.empty;
          parent = None;
        } param_typ in
      let key = (key_string_built_in_functions name param_typ)
      in StringMap.add key {
        formals = args; 
        locals = {
          variables = StringMap.empty;
          parent = None;
        };
        ret_typ = typ;
      } map
    in List.fold_left build_built_in_func_table StringMap.empty [
      ("printn", [], Void);
      ("printi", [Int], Void);
      ("prints", [String], Void);
      ("printb", [Bool], Void);
      ("printf", [Float], Void);

      ("ftoi", [Float], Int);
      ("btoi", [Bool], Int);
      ("stoi", [String], Int);
      ("itof", [Int], Float);
      ("stof", [String], Float);
      ("itos", [Int], String);
      ("ftos", [Float], String);
      ("btos", [Bool], String);
    ] 
  in

let get_funcs f_list = 
  List.fold_left (fun m f -> build_func_table globals f m) built_in_funcs f_list

in (globals, get_funcs f_list)
