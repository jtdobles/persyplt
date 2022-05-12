(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (structs, functions) =
  let context      = L.global_context () in
  let report_err e = raise (Failure e) in 

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "D" in

  (* Get types from the context *)
  let i32_t         = L.i32_type    context
    and i8_t        = L.i8_type     context
    and i1_t        = L.i1_type     context 
    and float_t     = L.double_type context
    and char_t      = L.i8_type     context
    and void_t      = L.void_type   context 
    and struct_t st = L.named_struct_type context st
  in

  (* Named struct types; array, string *)
  let array_t = fun (llvm_type) -> L.struct_type context [| L.pointer_type llvm_type; i32_t; i32_t|] in
  let string_t = L.struct_type context [| L.pointer_type char_t|] in
  let i32OF = L.const_int (L.i32_type context) in

  (* LLVM type of non-primitive types *)
  let rec ltype_of_struct_members = function
      A.Struct n -> struct_t n
    | A.Int   -> i32_t
    | A.Float -> float_t
    | A.String -> string_t
    | A.Bool  -> i1_t
  in

  (* Struct declaration *)
  let structs_decls =
    let struct_decl map stdecl = 
      let name = stdecl.sstname
        and mem_typs = Array.of_list (List.map (fun (t,_) ->
          ltype_of_struct_members t) stdecl.smembers) 
      in
      let stype = L.struct_type context mem_typs in
      StringMap.add name (stype, stdecl.smembers) map 
    in
    List.fold_left struct_decl StringMap.empty structs
  in

  let lookup_struc st = 
    try StringMap.find st structs_decls
    with Not_found -> raise(Failure("There's no such struct available")) 
  in

  (* LLVM types of primitive types *)
  let rec ltype_of_typ = function
      A.Int       -> i32_t
    | A.Float     -> float_t
    | A.String    -> string_t
    | A.Bool      -> i1_t
    | A.Void      -> void_t
    | A.Array ar  -> array_t (ltype_of_typ ar)
    | A.Struct st -> fst (lookup_struc st)
  in

  (* LLVM type of array elements *)
  let rec ltype_of_array_element = function
    A.Array ar -> ltype_of_typ ar
  in

  (* printf *)
  let printf_t = L.var_arg_function_type i32_t [| (L.pointer_type char_t) |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls =
    let function_decl map fdecl =
      let name = fdecl.sfname
        and formal_types = Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.sformals) 
      in
      let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) map 
    in
    List.fold_left function_decl StringMap.empty functions 
  in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    let char_format_str = L.build_global_stringptr "%s\n" "" builder
      and int_format_str = L.build_global_stringptr "%d\n" "" builder 
      and float_format_str = L.build_global_stringptr "%f\n" "" builder
      and boolean_format_str = L.build_global_stringptr "%s\n" "" builder
    in
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n (local, A.Void) m
      in
      List.fold_left2 add_formal StringMap.empty fdecl.sformals
        (Array.to_list (L.params the_function))
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup map n = match StringMap.find_opt n map with
      Some (v, _) -> v
      | None -> try fst (StringMap.find n local_vars) with
                Not_found -> report_err("Local var " ^ n ^ " not found")
    in

    (* Construct code for an expression; return its value *)
    let rec build_expr map builder ((_, e) : sexpr) = match e with
      SNoexpr(t) -> (match t with
        A.Int -> L.const_int i32_t 0
        | A.Float -> L.const_float float_t 0.0
        | A.String -> 
          let alloc = L.build_alloca string_t "alloc" builder in
          let global_str = L.build_global_string "" "global_str" builder in
          let str = L.build_bitcast global_str (L.pointer_type i8_t) "str_cast" builder in
          let str_loc = L.build_struct_gep alloc 0 "str_cast_loc" builder in
          let _ = L.build_store str str_loc builder in
          L.build_load alloc "" builder
        | A.Bool -> L.const_int i1_t 1
        | A.Array _ ->
          let llvm_typ = ltype_of_typ (fst (List.hd [])) in
          let ty = array_t llvm_typ in
          let alloc = L.build_alloca ty "alloc" builder in
          let data_loca = L.build_struct_gep alloc 0 "data_loca" builder in
          let len_loc = L.build_struct_gep alloc 1 "" builder in
          let len = 0 in
          let cap = len * 2 in
          let data_loc = L.build_array_alloca  llvm_typ (i32OF cap) "data_loc" builder in
          let _ = L.build_store data_loc data_loca builder in
          let _ = L.build_store (i32OF len) len_loc builder in
          L.build_load alloc "val" builder), map, builder

      | SNot (e) ->
        let (e', _, _) = build_expr map builder e in
        L.build_not e' "not op" builder, map, builder

      | SIntLit i -> L.const_int i32_t i, map, builder

      | SFloatLit fl -> L.const_float float_t fl, map, builder

      | SStringLit s ->
        let alloc = L.build_alloca string_t "alloc" builder in
        let global_str = L.build_global_string s "global_str" builder in
        let str = L.build_bitcast global_str (L.pointer_type i8_t) "str_cast" builder in
        let str_loc = L.build_struct_gep alloc 0 "str_cast_loc" builder in
        let _ = L.build_store str str_loc builder in
        let value = L.build_load alloc "" builder in
        (value, map, builder)

      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0), map, builder

      | SArrayLit ar ->
        let llvm_typ = ltype_of_typ (fst (List.hd ar)) in
        let ty = array_t llvm_typ in
        let alloc = L.build_alloca ty "alloc" builder in
        let data_loca = L.build_struct_gep alloc 0 "data_loca" builder in
        let len_loc = L.build_struct_gep alloc 1 "" builder in
        let len = List.length ar in
        let cap = len * 2 in
        let data_loc = L.build_array_alloca llvm_typ (i32OF cap) "data_loc" builder in
        let array_itertool (acc, builder) ex =
          let value, m', builder = build_expr map builder ex in
          let item_loc = L.build_gep data_loc [| i32OF acc |] "item_loc" builder in
          let _ = L.build_store value item_loc builder in
          (acc+1, builder)
        in
        let _, builder = List.fold_left array_itertool (0, builder) ar in
        let _ = L.build_store data_loc data_loca builder in
        let _ = L.build_store (i32OF len) len_loc builder in
        let value = L.build_load alloc "value" builder in
        (value, map, builder)

      | SId si -> L.build_load (lookup map si) si builder, map, builder

      | SAssign (v,e) ->
        let (e1, map1, builder) = build_expr map builder e in
          (match (snd v) with
            SId si ->
              ignore(L.build_store e1 (lookup map si) builder); e1, map1, builder
          )

      | SOpAssign(v, o, e) -> 
        let (e1, map1, builder) = build_expr map builder e in
        (match (snd v) with
          SId si ->
            ignore(L.build_store e1 (lookup map si) builder); e1, map1, builder
        )
      | SArrayAssign (v, i, e) ->
        let rval, m', builder = build_expr map builder e in
        let name = match snd v with SId si -> si in
        let nam_addr = lookup map name in
        let data_loca = L.build_struct_gep nam_addr 0 "" builder in
        let data_loc = L.build_load data_loca "" builder in
        let ival, _, builder = build_expr map builder i in
        let addr = L.build_gep data_loc [| ival |] "" builder in
        let _ = L.build_store rval addr builder in
        (rval, m', builder)

      | SArrayIndex (id, idx) ->
        let name = match snd id with 
          SId si -> si
          | _ -> "can't index non-id value"
        in
        let nam_addr = lookup map name in
        let data_loca = L.build_struct_gep nam_addr 0 "" builder in
        let data_loc = L.build_load data_loca "" builder in
        let ival, _, builder = build_expr map builder idx in
        let idx_addr = L.build_gep data_loc [| ival |] "" builder in
        let value = L.build_load idx_addr "" builder in
        (value, map, builder)

      | SStructAssign (v, m, e) ->
        let rval, map1, builder = build_expr map builder e in
        let name = match snd v with SId si -> si in
        let nam_addr = lookup map1 name in
        let struct_name = (match snd (StringMap.find name map1) with A.Struct i -> i) in
        let mname = (match m with A.Id i -> i) in
        let members = snd (lookup_struc struct_name) in
          let rec get_idx n lst i = match lst with
            | [] -> raise(Failure("Struct member not found"))
            | hd :: tl -> if (hd=n) then i else get_idx n tl (i+1)
          in
          let idx = (get_idx mname (List.map (fun (_, nm) -> nm) members) 0) in
        let ptr = L.build_struct_gep nam_addr idx ("struct_ptr") builder in 
        let _ = L.build_store rval ptr builder in
        (rval, map1, builder)

      | SStructUse (v, m) ->
        let name = match snd v with SId si -> si in
        let nam_addr = lookup map name in
        let struct_name = (match snd (StringMap.find name map) with A.Struct i -> i) in
        let mname = (match m with A.Id i -> i) in
        let members = snd (lookup_struc struct_name) in
          let rec get_idx n lst i = match lst with
            | [] -> raise(Failure("Struct member not found"))
            | hd :: tl -> if (hd=n) then i else get_idx n tl (i+1)
          in
          let idx = (get_idx mname (List.map (fun (_, nn) -> nn) members) 0) in
        let ptr = L.build_struct_gep nam_addr idx ("struct_ptr") builder in
        let value = L.build_load ptr "member_val" builder in
        (value, map, builder)
        
      (*| SUnop(op, e) ->
        let (e', _, _) = build_expr map builder e in
        (match op with
          A.Incr ->
            let added = L.build_nsw_add e' (L.const_int (ltype_of_typ t) 1) (L.value_name e') builder in
            let vname vr = (match vr with
              SId si -> si
              | _ -> raise(Failure("Increment should only be used with variables"))
            ) in
            L.build_store added (lookup (vname v)) builder
          | A.Decr ->
            let added = L.build_nsw_add e' (L.const_int (ltype_of_typ t) (-1)) (L.value_name e') builder in 
            let vname vr = (match vr with
                | SId(si)  -> si
                | _       -> raise (Error "Decrement should only be used with variables"))
            in
            L.build_store added (lookup (vname v)) builder
        ) e' "unary op" builder, map, builder*)

      | SBinop ((A.Float, _) as e1, op, e2) -> 
        let (e1', _, _) = build_expr map builder e1
          and (e2', _, _) = build_expr map builder e2
        in
        (match op with
          A.Add    -> L.build_fadd
          | A.Sub  -> L.build_fsub
          | A.Mult -> L.build_fmul
          | A.Div  -> L.build_fdiv
          | A.Eq   -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq  -> L.build_fcmp L.Fcmp.One
          | A.Lt   -> L.build_fcmp L.Fcmp.Olt
          | A.Leq  -> L.build_fcmp L.Fcmp.Ole
          | A.Gt   -> L.build_fcmp L.Fcmp.Ogt
          | A.Geq  -> L.build_fcmp L.Fcmp.Oge
          | A.And | A.Or -> 
            raise(Failure("semant error; float detected on and/or op"))
        ) e1' e2' "float op" builder, map, builder
      | SBinop (e1, A.Mod, e2) ->
        let (e1', _, _) = build_expr map builder e1
          and (e2', _, _) = build_expr map builder e2 
        in
        L.const_srem e1' e2', map, builder

      | SBinop (e1, op, e2) ->
        let (e1', _, _) = build_expr map builder e1
          and (e2', _, _) = build_expr map builder e2 
        in
        (match op with
          A.Add       -> L.build_add
          | A.Sub     -> L.build_sub
          | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
          | A.Eq      -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Lt      -> L.build_icmp L.Icmp.Slt
          | A.Leq     -> L.build_icmp L.Icmp.Sle
          | A.Gt      -> L.build_icmp L.Icmp.Sgt
          | A.Geq     -> L.build_icmp L.Icmp.Sge
        ) e1' e2' "int op" builder, map, builder


      | SCall ("printf", [e]) ->
        let e', _, builder = build_expr map builder e in 
        L.build_call printf_func [| float_format_str ; e' |] "printf" builder, map, builder
      | SCall ("printi", [e]) ->
        let e', _, builder = build_expr map builder e in 
        L.build_call printf_func [| int_format_str ; e' |] "printi" builder, map, builder
      | SCall ("prints", [e]) ->
        let e', _, builder = build_expr map builder e in 
        L.build_call printf_func [| char_format_str ; e' |] "prints" builder, map, builder
      
      | SCall ("leng", [e]) ->
        let arr_addr = (match e with _, SId si -> lookup map si) in
        let len_loc = L.build_struct_gep arr_addr 1 "" builder in
        let value = L.build_load len_loc "" builder in
        value, map, builder
      | SCall ("leng", [e]) ->
        let (e', _, builder) = build_expr map builder e in
        let length = L.array_length (L.type_of e') in
        L.const_int i32_t length, map, builder

      | SCall (fn, args) ->
        let (fdef, fdecl) = StringMap.find fn function_decls in
        let llargs = List.map (fun(a,b,c) -> a) (List.rev (List.map (build_expr map builder) (List.rev args))) in
        let res = (match fdecl.srtyp with
                    A.Void -> ""
                    | _ -> fn ^ "_res")
        in
        L.build_call fdef (Array.of_list llargs) res builder, map, builder
    in
  

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
        | None -> ignore (instr builder)
    in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    let rec build_stmt map builder s = match s with
      SBlock sl ->
        let b, _= List.fold_left (fun (b, m) s -> build_stmt m b s) (builder, map) sl in
        (b, map)
      | SVarDecl(ty, st, rex) -> 
        (match ty with
          A.Struct s ->
            let ltyp = ltype_of_typ ty in
            let st_addr = L.build_alloca ltyp st builder in
            let m' = StringMap.add st (st_addr, ty) map in
          (builder, m')
          | _ ->
            let ltyp = ltype_of_typ ty in
            let st_addr = L.build_alloca ltyp st builder in
            let rval, m', builder = build_expr map builder rex in
            let m'' = StringMap.add st (st_addr, A.Void) m' in
            let _ = L.build_store rval st_addr builder in
          (builder, m'')
        )
      | SArrayDecl(t, v, e1, e) ->
        let llvm_typ = ltype_of_typ t in
        let addr = L.build_alloca llvm_typ v builder in
        let alloc = L.build_alloca llvm_typ "alloc" builder in
        let data_loca = L.build_struct_gep alloc 0 "data_loca" builder in
        let len = (match e1 with _, SIntLit i -> i) in
        let len_loc = L.build_struct_gep alloc 1 "" builder in
        let cap = len * 2 in
        let data_loc = L.build_array_alloca (ltype_of_array_element t) (i32OF cap) "data_loc" builder in
        let noexpr_val = 
          (match t with
            Array Ast.Int      -> L.const_int i32_t 0
            | Array Ast.Float  -> L.const_float float_t 0.0
            | Array Ast.String -> 
              let alloc = L.build_alloca string_t "alloc" builder in
              let global_str = L.build_global_string "" "global_str" builder in
              let str = L.build_bitcast global_str (L.pointer_type i8_t) "str_cast" builder in
              let str_loc = L.build_struct_gep alloc 0 "str_cast_loc" builder in
              let _ = L.build_store str str_loc builder in
              L.build_load alloc "" builder
            | Array Ast.Bool   -> L.const_int i1_t 1
          )
        in
        let rec stdo (acc, builder) = 
          let item_loc = L.build_gep data_loc [|i32OF acc |] "item_loc" builder in
          let _ = L.build_store noexpr_val item_loc builder in
          if acc < len then stdo (acc + 1, builder) else acc, builder
        in
        let _, builder = stdo (0, builder) in
        let m' = StringMap.add v (addr, A.Void) map in
        let _ = L.build_store data_loc data_loca builder in
        let _ = L.build_store (i32OF len) len_loc builder in
        let value = L.build_load alloc "value" builder in
        let vl = lookup m' v in
        let _ = L.build_store value vl builder in
        (builder, m')
      | SIf (e, s1, s2) ->
        let bool_val, m', builder = build_expr map builder e in
        let merge_blocks = L.append_block context "merge" the_function in
        let merge_build_br = L.build_br merge_blocks in (* partial fn *)
        let then_block = L.append_block context "then" the_function in
        let then_builder, m'' = build_stmt m' (L.builder_at_end context then_block) s1 in
        add_terminal then_builder merge_build_br;
        let else_block = L.append_block context  "else" the_function in
        let else_builder, m'' = build_stmt m' (L.builder_at_end context else_block) s2 in
        add_terminal else_builder merge_build_br;

        ignore(L.build_cond_br bool_val then_block else_block builder);
        L.builder_at_end context merge_blocks, m'
      | SFor (e1, e2, e3, stmtlst) -> build_stmt map builder (SBlock [ e1 ; SWhile(e2, SBlock [stmtlst ; SExpr e3]) ] )
      | SWhile(cond, stmtlst) ->
        let pred_block = L.append_block context "while" the_function in
        ignore(L.build_br pred_block builder);
        let body_block = L.append_block context "while_body" the_function in
        let body_block_br, m' = build_stmt map (L.builder_at_end context body_block) stmtlst in
        add_terminal body_block_br (L.build_br pred_block);
        let pred_builder = L.builder_at_end context pred_block in
        let bool_val, _, _ = build_expr m' pred_builder cond in
        let merge_blocks = L.append_block context "merge" the_function in
        ignore(L.build_cond_br bool_val body_block merge_blocks pred_builder);
        L.builder_at_end context merge_blocks, m'
      | SExpr e -> ignore(build_expr map builder e); builder, map
      | SReturn e -> 
          ignore(match fdecl.srtyp with
            A.Void -> L.build_ret_void builder
            | _ -> let e', _, _ = (build_expr map builder e) in
            L.build_ret e' builder);
            builder, map
      | _ -> report_err "No implementation"
    in

    (* Build the code for each statement in the function *)
    let builder,_ = build_stmt StringMap.empty builder (SBlock fdecl.sfstmts) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.srtyp with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
  