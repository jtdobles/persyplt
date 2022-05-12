module L = Llvm
module A = Ast
open Sast

exception Error of string

module StringMap = Map.Make(String)

let translate (_, functions) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
  we will generate code *)
  let the_module = L.create_module context "D" in

  (* define llytype variables *)
  let i64_t      = L.i64_type    context
  and i32_t      = L.i32_type    context
  and i16_t      = L.i16_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.float_type  context
  and double_t   = L.double_type context
  and void_t     = L.void_type   context in
  let str_t      = L.pointer_type i8_t
  in

  let rec ltype_of_typ = function
      A.Int         -> i32_t
    | A.Bool        -> i1_t
    | A.Void        -> void_t
    | A.Float       -> float_t
    | A.String      -> str_t
    | A.Function(_) -> raise (Error "Function lltype not implemented")
  in

  let rec lvalue_of_typ typ = function
    A.Int | A.Bool | A.Void -> L.const_int (ltype_of_typ typ) 0
    | A.Float               -> L.const_float (ltype_of_typ typ) 0.0
    | A.String              -> L.const_pointer_null (ltype_of_typ typ)
    | A.Function(_)         -> raise (Error "Function lltype not implemented")
  in

  (* datatype cast funcs *)
  let verify_params params func = 
    if List.length params != 1 then
      raise (Failure ("Error: " ^ func ^ "() requires one argument"))
    else List.hd params
  in

  let to_int params = 
    match verify_params params "int" with 
      (_, SFloatLit(f)) -> (A.Int, SIntLit(int_of_float f))
      | (_, SBoolLit(true)) -> (A.Int, SIntLit(1))
      | (_, SBoolLit(false)) -> (A.Int, SIntLit(0))
      | (_, SStringLit(s)) -> (
          try (A.Int, SIntLit(int_of_string s))
          with Failure _ -> raise (Failure ("Error: string \"" ^ s ^ "\" cannot be cast to int")))
  in

  let to_float params = 
    match verify_params params "float" with 
      (_, SIntLit(i)) -> (A.Float, SFloatLit(float_of_int i))
      | (_, SStringLit(s)) -> (
        try (A.Int, SFloatLit(float_of_string s))
        with Failure _ -> raise (Failure ("Error: string \"" ^ s ^ "\" cannot be cast to float")))
  in
  
  let to_string params = 
    match verify_params params "string" with 
      (_, SIntLit(i)) -> (A.String, SStringLit(string_of_int i))
      | (_, SFloatLit(f)) -> (A.String, SStringLit(string_of_float f))
      | (_, SBoolLit(true)) -> (A.String, SStringLit("true"))
      | (_, SBoolLit(false)) -> (A.String, SStringLit("false"))
  in
    
  let printf_t : L.lltype = 
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
    L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  let build_function_body fdecl =
    let (the_function, _) = try StringMap.find fdecl.sfname function_decls 
      with Not_found -> raise (Error "function definition not found")
    in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "int_fmt_str" builder
    and float_format_str = L.build_global_stringptr "%g\n" "float_fmt_str" builder
    and str_format_str = L.build_global_stringptr "%s\n" "str_fmt_str" builder
    in

    (* track vars llvalue *)
    let local_vars:(string, L.llvalue) Hashtbl.t = Hashtbl.create 50 in

    (* add formals to local_vars map *)
    let add_formal (t, n) p = 
      L.set_value_name n p;
      let local = L.build_alloca (ltype_of_typ t) n builder in
      ignore (L.build_store p local builder);
      Hashtbl.add local_vars n local;
    in

    (* add formal bindings and their values to local_vars *)
    let _ = List.iter2 add_formal fdecl.sformals (Array.to_list (L.params the_function)) in

    (* lookup table for local vars *)
    let lookup n = try Hashtbl.find local_vars n
      with Not_found -> raise (Error ("variable " ^ n ^ " not found in locals map"))
    in

    (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
     match L.block_terminator (L.insertion_block builder) with
       Some _ -> ()
     | None -> ignore (instr builder) in

    (* build_expr *)
    let rec expr builder ((e_type, e) : sexpr) = match e with
      SIntLit(num)      -> L.const_int (ltype_of_typ A.Int) num
      | SBoolLit(b)     -> L.const_int (ltype_of_typ A.Bool) (if b then 1 else 0)
      | SFloatLit(fl)   -> L.const_float (ltype_of_typ A.Float) fl
      | SStringLit(str) -> L.build_global_stringptr str "" builder
      | SId s -> L.build_load (lookup s) s builder

      | SBinop ((A.Float,_ ) as e1, op, e2) ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
        (match op with 
           A.Add     -> L.build_fadd
         | A.Sub     -> L.build_fsub
         | A.Mult    -> L.build_fmul
         | A.Div     -> L.build_fdiv 
         | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
         | A.Neq     -> L.build_fcmp L.Fcmp.One
         | A.Less    -> L.build_fcmp L.Fcmp.Olt
         | A.Leq     -> L.build_fcmp L.Fcmp.Ole
         | A.Greater -> L.build_fcmp L.Fcmp.Ogt
         | A.Geq     -> L.build_fcmp L.Fcmp.Oge
         | A.Mod     -> L.build_frem
         | A.And | A.Or  ->
           raise (Error "Invalid expressions for Binary operator between floats, semant should have stopped this")
        ) e1' e2' "tmp" builder

      | SBinop (e1, op, e2) ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
        (match op with
           A.Add     -> L.build_add
         | A.Sub     -> L.build_sub
         | A.Mult    -> L.build_mul
         | A.Div     -> L.build_sdiv
         | A.And     -> L.build_and
         | A.Or      -> L.build_or
         | A.Equal   -> L.build_icmp L.Icmp.Eq
         | A.Neq     -> L.build_icmp L.Icmp.Ne
         | A.Less    -> L.build_icmp L.Icmp.Slt
         | A.Leq     -> L.build_icmp L.Icmp.Sle
         | A.Greater -> L.build_icmp L.Icmp.Sgt
         | A.Geq     -> L.build_icmp L.Icmp.Sge
         | A.Mod     -> L.build_srem
        ) e1' e2' "tmp" builder

      | SUnop(op, ((t, var) as e)) -> 
        let e' = expr builder e in
        (match op with
           A.Neg when t = A.Float -> L.build_fneg e' "tmp" builder
         | A.Neg                  -> L.build_neg e' "tmp" builder
         | A.Not                  -> L.build_not e' "tmp" builder
         | A.Incr -> 
           let added = L.build_nsw_add e' (L.const_int (ltype_of_typ t) 1) (L.value_name e') builder in 
           let var_name vr = (match vr with
               | SId(s)  -> s 
               | _       -> raise (Error "Incr should only be used with variables."))
           in
           L.build_store added (lookup (var_name var)) builder
  
         | A.Decr ->
           let added = L.build_nsw_add e' (L.const_int (ltype_of_typ t) (-1)) (L.value_name e') builder in 
           let var_name vr = (match vr with
               | SId(s)  -> s 
               | _       -> raise (Error "Decr should only be used with variables."))
           in
           L.build_store added (lookup (var_name var)) builder
        )

      | SAssign (s, e) -> 
        let e' = expr builder e in
        ignore(L.build_store e' (lookup s) builder); e'

      | SOpAssign(v, o, e) ->
        let value = expr builder (A.Void, SBinop( (A.Void, SId(v)), o, e)) in
        ignore(L.build_store value (lookup v) builder); value

      | SDecAssign(t, s, e) -> 
        let local_var = L.build_alloca (ltype_of_typ t) s builder in
        Hashtbl.add local_vars s local_var;
        let e' = expr builder e in
        ignore(L.build_store e' (lookup s) builder); e'


      | SCall("printn", []) -> let newline = expr builder (String, SStringLit("")) in 
        L.build_call printf_func [| str_format_str ; newline |] "printf" builder

      | SCall("printi", [params]) -> let print_value = expr builder params in
        L.build_call printf_func [| int_format_str ; print_value |] "printf" builder

      | SCall("printf", [params]) -> let print_value = L.build_fpext (expr builder params) double_t "ext" builder in 
        L.build_call printf_func [| float_format_str ; print_value |] "printf" builder
            
      | SCall("prints", [params]) -> let print_value = expr builder params in
            L.build_call printf_func [| str_format_str ; print_value |] "printf" builder

      | SCall("printb", [e]) -> let print_value = expr builder e
      in L.build_call printf_func [| int_format_str ; print_value |] "printf" builder

      | SCall("ftoi", params) -> expr builder (to_int params)
      | SCall("btoi", params) -> expr builder (to_int params)
      | SCall("stoi", params) -> expr builder (to_int params)
      | SCall("itof", params) -> expr builder (to_float params)
      | SCall("stof", params) -> expr builder (to_float params)
      | SCall("itos", params) -> expr builder (to_string params)
      | SCall("ftos", params) -> expr builder (to_string params)
      | SCall("btos", params) -> expr builder (to_string params)

      (* SCall for user defined functions *)
      | SCall (f, args)           -> 
        let (fdef, fdecl) = try StringMap.find f function_decls 
          with Not_found -> raise (Error "User defined function call not found")
        in
        let llargs = List.rev (List.map (expr builder) (List.rev args)) in
        let result = (match fdecl.styp with 
              A.Void -> ""
            | _ -> f ^ "_result") in
        L.build_call fdef (Array.of_list llargs) result builder

      | SNoexpr                   -> L.const_int i32_t 0
      | _ -> raise (Error "Expression match not implemented")
    in

    let rec stmt builder = function
      | SBlock sl                               -> List.fold_left stmt builder sl
      | SExpr e                                 -> ignore(expr builder e); builder
      | SDec (t, n)                             -> 
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in Hashtbl.add local_vars n local_var; builder
      | SReturn e -> ignore(match fdecl.styp with
            A.Void -> L.build_ret_void builder 
          | _ -> L.build_ret (expr builder e) builder
          );
        builder

      | SIf (predicate, then_stmt, else_stmt)   -> 
        let bool_val = expr builder predicate in
        let merge_bb = L.append_block context "merge" the_function in
        let build_br_merge = L.build_br merge_bb in (* partial function *)

        let then_bb = L.append_block context "then" the_function in
        add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
          build_br_merge;

        let else_bb = L.append_block context "else" the_function in
        add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
          build_br_merge;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context merge_bb

      | SWhile (predicate, body, increment) -> 
        let rec loop_stmt loop_bb exit_bb builder = (function
              SBlock(sl) -> List.fold_left (fun b s -> loop_stmt loop_bb exit_bb b s) builder sl
            | SIf (predicate, then_stmt, else_stmt)   -> 
              let bool_val = expr builder predicate in
              let merge_bb = L.append_block context "merge" the_function in
              let build_br_merge = L.build_br merge_bb in (* partial function *)

              let then_bb = L.append_block context "then" the_function in
              add_terminal (loop_stmt loop_bb exit_bb (L.builder_at_end context then_bb) then_stmt)
                build_br_merge;

              let else_bb = L.append_block context "else" the_function in
              add_terminal (loop_stmt loop_bb exit_bb (L.builder_at_end context else_bb) else_stmt)
                build_br_merge;

              ignore(L.build_cond_br bool_val then_bb else_bb builder);
              L.builder_at_end context merge_bb
            | SCont _ -> 
              let cont_bb = L.append_block context "cont" the_function in 
              ignore (L.build_br cont_bb builder);
              let cont_builder = (L.builder_at_end context cont_bb) in
              add_terminal (loop_stmt loop_bb exit_bb cont_builder increment) (L.build_br loop_bb);
              builder 
            | SExit _ -> ignore(L.build_br exit_bb builder); builder
            | _ as e -> stmt builder e) in

        let pred_bb = L.append_block context "while" the_function 
        and merge_bb = L.append_block context "merge" the_function in

        ignore(L.build_br pred_bb builder);
        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (loop_stmt pred_bb merge_bb (L.builder_at_end context body_bb) body)
          (L.build_br pred_bb);

        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = expr pred_builder predicate in

        ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
        L.builder_at_end context merge_bb
      | SCont _     -> raise (Failure "Error: cont occurs outside of loop")
      | SExit _    -> raise (Failure "Error: exit occurs outside of loop")
      | _           -> raise (Error "Statement match for stmt builder not implemented")
    in 

    let builder = stmt builder (SBlock fdecl.sfstmts) in

    add_terminal builder (match fdecl.styp with
          A.Void -> L.build_ret_void
        | A.Float -> L.build_ret (L.const_float float_t 0.0)
        | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  let _ = List.map build_function_body functions in
  the_module