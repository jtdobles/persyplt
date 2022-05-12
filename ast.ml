type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not | Incr | Decr

type typ = 
    Int 
  | Bool 
  | Void
  | Float
  | String
  | Function of typ

type bind = typ * string

type expr =
    IntLit of int
  | BoolLit of bool
  | FloatLit of float
  | StringLit of string

  | Id of string
  | Unop of uop * expr
  | Binop of expr * op * expr

  | Assign of string * expr
  | OpAssign of string * op * expr
  | DecAssign of typ * string * expr
  | Call of string * expr list

  | Noexpr

type stmt =
  Block of stmt list
  | BBlock of stmt list
  | Expr of expr
  | Dec of typ * string
  | Return of expr
  | If of expr * stmt * stmt
  | While of expr * stmt * stmt
  | Cont of expr
  | Exit of expr

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    fstmts : stmt list;
  }

type program = stmt list * func_decl list

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"
  | Incr -> "++"
  | Decr -> "--"

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let rec string_of_typ = function
  Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
  | Float -> "float"
  | String -> "string"
  | Function(t) -> string_of_typ t ^ " fn"

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | FloatLit(l) -> string_of_float l
  | StringLit(s) -> "\"" ^ s ^ "\""
  | Noexpr -> ""

  | Id(s) -> s
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2

  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | OpAssign(v, o, e) -> v ^ " " ^ string_of_op o ^ "= " ^ string_of_expr e
  | DecAssign(t, v, e) -> string_of_typ t ^ " " ^ v ^ " = " ^ string_of_expr e
  
  | Call(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"

let rec string_of_stmt = function
  Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | BBlock(stmts) -> "\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "\n" 
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Dec(t, v) -> string_of_typ t ^ " " ^ v ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | Cont(expr) -> "cont " ^ string_of_expr expr ^ ";\n";
  | Exit(expr) -> "exit " ^ string_of_expr expr ^ ";\n";
  | While(e, s, _) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

  let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

  let string_of_fdecl fdecl =
    string_of_typ fdecl.typ ^ " " 
    ^ fdecl.fname 
    ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) 
    ^ ")\n{\n" 
    ^ String.concat "" (List.map string_of_stmt fdecl.fstmts) 
    ^ "}\n"
  
  let string_of_program (sts, funcs) =
    (* Do we reverse to pretty-print? Is upside down stuff an indicator of the AST *)
    String.concat "" (List.map string_of_fdecl funcs) ^ "\n" ^
    String.concat "\n" (List.map string_of_stmt (List.rev sts))