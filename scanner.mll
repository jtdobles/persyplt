(* Ocamllex scanner for D, adapted from that of the MicroC compiler. Ref: https://github.com/cwabbott0/microc-llvm *)

{ open Dparse }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| "//" 	   { singlecomment lexbuf}      (* Single line comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }
| '='      { ASSIGN }
| "+="     { PLUS_ASSIGN }
| "-="     { MINUS_ASSIGN }
| "*="     { TIMES_ASSIGN }
| "/="     { DIVIDE_ASSIGN } 
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "string" { STRING }
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }
| '.'      { DOT }
| "fn"     { FUNC }
| "cont"   { CONT }
| "exit"   { EXIT }
| ['0'-'9']+['.']['0'-'9']+ as lxm { FLOATLIT(float_of_string lxm) }
| ['0'-'9']+ as lxm { INTLIT(int_of_string lxm) }
| ['\"'](['\x20'-'\x21' '\x23' - '\x7E']* as lxm)['\"'] { STRLIT(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and singlecomment = parse
  "\n" { token lexbuf }
| _    { singlecomment lexbuf }
