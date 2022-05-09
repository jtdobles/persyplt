type op = Add | Sub | Mult | Div | Mod | Eq | Neq | Lt | Leq | Gt | Geq | And | Or
type typ = Int | Float | String | Bool | Void | Array of typ | Struct of string 
type assignment = Assign 

type expr =
  Noexpr of typ
  | Not of expr
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | BoolLit of bool
  | ArrayLit of expr list
  | Id of string
  | Var of string
  | Assign of expr * expr
  | ArrayAssign of expr * expr * expr
  | ArrayIndex of expr * expr
  | StructAssign of expr * expr * expr
  | StructUse of expr * expr
  | Binop of expr * op * expr
  | Call of string * expr list

(* var binding *)
type bind = typ * string

type stmt = 
  Block of stmt list
  | VarDecl of typ * string * expr
  | ArrayDecl of typ * string * expr * expr
  | If of expr * stmt * stmt
  | For of stmt * expr * expr * stmt
  | While of expr * stmt
  | Print of expr
  | Expr of expr
  | Return of expr
  (* TODO: switch/case/default *)


type struct_decl = {
  stname: string;
  members: bind list;
}

(* func_decl *)
type func_decl = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  fstmts: stmt list;
}

type program = struct_decl list * func_decl list

let rec string_of_typ = function
  Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Bool -> "bool"
  | Void -> "void"
  | Array(t) -> string_of_typ(t) ^ " array"
  | Struct(t) -> t
  (* TODO: tuple, void implementation *)

(* Pretty-printing functions *)
let string_of_op = function
  Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Leq -> "<="
  | Gt -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let rec string_of_expr = function
  Noexpr(t) -> ""
  | Not(v) -> "!" ^ string_of_expr v
  | IntLit(i) -> string_of_int i
  | FloatLit(fl) -> string_of_float fl
  | StringLit(s) -> s
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | ArrayLit(l) -> "[" ^ (String.concat ", " (List.map string_of_expr l)) ^ "]"
  | Id(s) -> s
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | ArrayAssign(v, i, e) -> string_of_expr v ^ "[" ^ string_of_expr i ^ "]" ^ " = " ^ string_of_expr e
  | ArrayIndex(v, i) -> string_of_expr v ^ "[" ^ string_of_expr i ^ "]"
  | StructAssign(v, m, e) -> string_of_expr v ^ "." ^ string_of_expr m ^ " = " ^ string_of_expr e ^ ";"
  | StructUse(v, m) -> string_of_expr v ^ "." ^ string_of_expr m
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | _ -> "no expr available"

let rec string_of_stmt = function
  Block(stmts) ->
  "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | VarDecl(t, s1, Noexpr(ty)) -> string_of_typ t ^ " " ^ s1 ^ ";\n"
  | VarDecl(t, s1, e1) -> string_of_typ t ^ " " ^ s1 ^ " = " ^ string_of_expr e1 ^ ";\n"
  | ArrayDecl(t, v, e, Noexpr(ty)) -> string_of_typ t ^ " " ^ v ^ "[" ^ string_of_expr e ^ "];\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) -> "for (" ^ string_of_stmt e1 ^ "; " ^ string_of_expr e2 ^ "; " ^ string_of_expr e3 ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  (*| Switch(e, s) -> "switch (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | Case(e, s) -> "case :" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | Default(s) -> "default: " ^ string_of_stmt s*)
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | _ -> "no matching stmt"
  (* TODO: switch/case/default, tuple *)

let string_of_structs stdecl =
  "struct " ^ stdecl.stname ^ "{\n" ^ String.concat "" (List.map snd stdecl.members) ^ "};\n"

let string_of_vdecl = function
  VarDecl(t, id, Noexpr(ty)) -> string_of_typ t ^ " " ^ id ^ ";\n"
  | VarDecl(t, id, e) -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr e ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.fstmts) ^
  "}\n"

let string_of_program (structs, funcs) =
  String.concat "" (List.map string_of_structs structs) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)