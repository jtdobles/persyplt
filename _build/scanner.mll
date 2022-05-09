(* Ocamllex scanner for D *)

{ open Dparse }

let digit = ['0'-'9']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Multiline comments *)
| "//" 	   { singlecomment lexbuf}      (* Single line comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| '!'  		 { NOT }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| '>'  		 { GT }
| ">=" 		 { GEQ }
| "&&"     { AND }
| "||"     { OR }
| '='      { ASSIGN }
| ';'      { SEMI }
| '.'      { DOT }
| ','  		 { COMMA }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "true"   { BOOL_L(true) }
| "false"  { BOOL_L(false) }
| "int"    { INT }
| "float"  { FLOAT }
| "string" { STRING} 
| "bool"   { BOOL }
| "null"   { NULL }
| "void"   { VOID }
| "ar"     { ARRAY }
| "struct" { STRUCT }
(*| "switch" { SWITCH }
| "case"   { CASE }
| "default" { DEFAULT }*)
| ['-']?digit+ as lxm { INT_L(int_of_string lxm) }
| ['-']?digit+['.']digit+ as lxm {FLOAT_L(float_of_string lxm)}
| "\'" [^''']+ "\'" as lxm { STRING_L(lxm) }
| ['a'-'z''_' ]+ as lxm { ID(lxm) }
| ['A'-'Z']['a'-'z' 'A'-'Z']* as structLit { STRUCT_ID(structLit) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and singlecomment = parse
  "\n" { token lexbuf }
| _    { singlecomment lexbuf }
