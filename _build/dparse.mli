type token =
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | NOT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | PLUS_ASSIGN
  | MINUS_ASSIGN
  | TIMES_ASSIGN
  | DIVIDE_ASSIGN
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | ASSIGN
  | SEMI
  | DOT
  | COMMA
  | IF
  | ELSE
  | FOR
  | WHILE
  | RETURN
  | PRINTF
  | TRUE
  | FALSE
  | INT
  | FLOAT
  | STRING
  | BOOL
  | NULL
  | VOID
  | ARRAY
  | STRUCT
  | MAIN
  | INT_L of (int)
  | FLOAT_L of (float)
  | ID of (string)
  | STRING_L of (string)
  | STRUCT_ID of (string)
  | BOOL_L of (bool)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
