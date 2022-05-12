(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SIntLit of int
  | SBoolLit of bool
  | SFloatLit of float
  | SStringLit of string

  | SId of string
  | SUnop of uop * sexpr
  | SBinop of sexpr * op * sexpr
  
  | SAssign of string * sexpr
  | SDecAssign of typ * string * sexpr
  | SOpAssign of string * op * sexpr
  
  | SCall of string * sexpr list
  | SNoexpr

type sstmt =
  SBlock of sstmt list
  | SExpr of sexpr
  | SDec of typ * string
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt * sstmt
  | SCont of sexpr
  | SExit of sexpr

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    sfstmts : sstmt list;
  }

type sprogram = sstmt list * sfunc_decl list

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SIntLit(l) -> string_of_int l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SFloatLit(l) -> string_of_float l
  | SStringLit(s) -> "\"" ^ s ^ "\""

  | SId(s) -> s
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2

  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SOpAssign(v, o, e) -> v ^ " " ^ string_of_op o ^ "= " ^ string_of_sexpr e
  | SDecAssign(t, v, e) -> string_of_typ t ^ " " ^ v ^ " = " ^ string_of_sexpr e

  | SCall(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"

  | SNoexpr -> "") 
    ^ ")"	

let rec string_of_sstmt = function
    SBlock(stmts) -> "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SDec(t, v) -> string_of_typ t ^ " " ^ v ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf(e, s, SBlock([])) -> "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s, _) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SCont(expr) -> "cont " ^ string_of_sexpr expr ^ ";\n"
  | SExit(expr) -> "exit " ^ string_of_sexpr expr ^ ";\n"

let string_of_sfdecl fdecl =
    string_of_typ fdecl.styp ^ " " 
    ^ fdecl.sfname ^ "(" 
    ^ String.concat ", " (List.map snd fdecl.sformals)
    ^ ")\n{\n"
    ^ String.concat "" (List.map string_of_sstmt fdecl.sfstmts)
    ^ "}\n"
  
let string_of_sprogram (sts, funcs) =
    String.concat "" (List.map string_of_sfdecl funcs) ^ "\n" ^
    String.concat "\n" (List.map string_of_sstmt (List.rev sts))