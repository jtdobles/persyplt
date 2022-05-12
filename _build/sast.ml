(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
  SNoexpr of typ
  | SNot of sexpr
  | SIntLit of int
  | SFloatLit of float
  | SStringLit of string
  | SBoolLit of bool
  | SArrayLit of sexpr list
  | SId of string
  | SVar of string
  | SAssign of sexpr * sexpr
  | SOpAssign of string * op * sexpr
  | SArrayAssign of sexpr * sexpr * sexpr
  | SArrayIndex of sexpr * sexpr
  | SStructAssign of sexpr * expr * sexpr
  | SStructUse of sexpr * expr
  | SUnop of uop * sexpr
  | SBinop of sexpr * op * sexpr
  | SCall of string * sexpr list

type sstmt =
    SBlock of sstmt list
  | SVarDecl of typ * string * sexpr
  | SArrayDecl of typ * string * sexpr * sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sstmt * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SSwitch of sexpr * sstmt
  | SCase of sexpr * sstmt
  | SDefault of sexpr * sstmt
  | SPrint of sexpr
  | SExpr of sexpr
  | SReturn of sexpr

type sstruct_decl = {
  sstname: string;
  smembers: bind list;
}

(* func_def: ret_typ fname formals locals body *)
type sfunc_decl = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  sfstmts: sstmt list;
}

type sprogram = sstruct_decl list * sfunc_decl list

(* Pretty-printing functions *)
let rec string_of_sexpr (se:sexpr) = match snd se with 
  SNoexpr(t) -> ""
  | SIntLit(i) -> string_of_int i
  | SFloatLit(fl) -> string_of_float fl
  | SStringLit(s) -> s
  | SArrayLit(l) -> "[" ^ (String.concat ", " (List.map string_of_sexpr l)) ^ "]"
  | SId(s) -> s
  | SAssign(v, e) -> string_of_sexpr v ^ " = " ^ string_of_sexpr e
  | SOpAssign(v, o, e) -> v ^ " " ^ string_of_op o ^ "= " ^ string_of_sexpr e
  | SArrayAssign(v, i, e) -> string_of_sexpr v ^  "[" ^ string_of_sexpr i ^ "]"^" = " ^ string_of_sexpr e
  | SArrayIndex(v, i) -> string_of_sexpr v ^ "[" ^ string_of_sexpr i ^ "]"
  (*| SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e*)
  | SCall(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | _ -> "Sexpr not found"

let rec string_of_sstmt = function
  SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SVarDecl(t, s1, e1) -> string_of_typ t ^" " ^ s1 ^ " = " ^ string_of_sexpr e1 ^ ";\n"
  | SArrayDecl(t, v, e1, e) -> string_of_typ t ^ " " ^ v ^  "[" ^ string_of_sexpr e1 ^ "];\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
    "for (" ^ string_of_sstmt e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^ string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr

let string_of_sstructs stdecl = 
  "struct " ^ stdecl.sstname ^ "{\n" ^
  String.concat "\n" (List.map snd stdecl.smembers) ^
  "\n};\n"

let string_of_svdecl = function
  VarDecl(t, id, Noexpr(ty)) -> string_of_typ t ^ " " ^ id
  | VarDecl(t, id, e) -> string_of_typ t ^ " " ^ id ^ " = "

let string_of_sfdecl fdecl =
  string_of_typ fdecl.srtyp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sfstmts) ^
  "}\n"

let string_of_sprogram (structs, funcs) =
  "\n\nSementically checked program: \n\n" ^
  String.concat "" (List.map string_of_sstructs structs) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)