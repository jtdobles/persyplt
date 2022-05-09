/* Ocamlyacc parser for D */

%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token NOT
%token PLUS MINUS TIMES DIVIDE MOD
%token EQ NEQ LT LEQ GT GEQ
%token AND OR
%token ASSIGN SEMI DOT COMMA
%token IF ELSE FOR WHILE
%token RETURN PRINTF
/* TODO: SWITCH CASE DEFAULT TUPLE */

%token TRUE FALSE
%token INT FLOAT STRING BOOL NULL VOID
%token ARRAY STRUCT
%token MAIN
%token <int> INT_L
%token <float> FLOAT_L
%token <string> ID STRING_L
%token <string> STRUCT_ID
%token <bool> BOOL_L
%token EOF

%left LBRACKET RBRACKET
%left LPAREN RPAREN
%left SEMI
%right ASSIGN
%left AND OR
%left EQ NEQ LT LEQ GT GEQ
%left PLUS MINUS TIMES DIVIDE MOD
%right NOT

%start program
%type <Ast.program> program

%%

/* add function declarations*/
program:
  { [], [] }
  | program struct_decl { ($2 :: fst $1), snd $1 }
  | program func_decl { fst $1, ($2 :: snd $1) }

struct_decl:
  STRUCT STRUCT_ID LBRACE struct_stmt_list RBRACE SEMI
  {{ 
    stname = $2;
    members = List.rev $4;
  }}

func_decl:
  dtype ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
  {{
    rtyp = $1;
    fname = $2;
    formals = $4;
    fstmts = List.rev $7;
  }}

struct_stmt_list:
  /*nothing*/                   { [] }
  | struct_stmt_list struc_stmt { $2 :: $1 }

struc_stmt:
  dtype ID SEMI { ($1, $2) }

formals_opt:
  /*nothing*/    { [] }
  | formals_list { $1 }

formals_list:
    dtype ID                    { [($1, $2)] }
  | formals_list COMMA dtype ID { ($3, $4) :: $1 }

vname:
  ID { Id($1) }

stmt_list:
  /* nothing */     { [] }
  | stmt_list stmt  { $2 :: $1 }

stmt:
    expr SEMI                                  { Expr $1 }
  | LBRACE stmt_list RBRACE                    { Block(List.rev $2) }
  | dtype ID SEMI                              { VarDecl($1, $2, Noexpr($1)) }
  | dtype ID ASSIGN expr SEMI                  { VarDecl($1, $2, $4) }
  | dtype ID LBRACKET expr RBRACKET SEMI       { ArrayDecl($1, $2, $4, Noexpr($1))}
  | IF LPAREN expr RPAREN stmt                 { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt       { If($3, $5, $7) }
  | FOR LPAREN stmt expr SEMI expr RPAREN stmt { For($3, $4, $6, $8) }
  | WHILE LPAREN expr RPAREN stmt              { While ($3, $5) }
  | RETURN expr SEMI                           { Return $2 }

expr:
  LPAREN expr RPAREN                      { $2 }
  | NOT expr                              { Not($2) }
  | INT_L                                 { IntLit($1) }
  | FLOAT_L                               { FloatLit($1) }
  | STRING_L                              { StringLit($1) }
  | BOOL_L                                { BoolLit($1) }
  | LBRACKET array_opt RBRACKET           { ArrayLit(List.rev $2) }
  | ID                                    { Id($1) }
  | vname ASSIGN expr                     { Assign($1, $3) }
  | ID LBRACKET expr RBRACKET ASSIGN expr { ArrayAssign(Id($1), $3, $6) }
  | ID LBRACKET expr RBRACKET             {ArrayIndex(Id($1), $3)}
  | ID DOT ID                             { StructUse(Id($1), Id($3)) }
  | ID DOT ID ASSIGN expr                 { StructAssign(Id($1), Id($3), $5) }
  | ID LPAREN args_opt RPAREN             { Call ($1, $3) }
  | expr PLUS  expr                       { Binop($1, Add, $3) }
  | expr MINUS  expr                      { Binop($1, Sub, $3) }
  | expr TIMES  expr                      { Binop($1, Mult, $3) }
  | expr DIVIDE expr                      { Binop($1, Div, $3) }
  | expr MOD expr                         { Binop($1, Mod, $3) }
  | expr EQ  expr                         { Binop($1, Eq, $3) }  
  | expr NEQ  expr                        { Binop($1, Neq, $3) }
  | expr LT  expr                         { Binop($1, Lt, $3) }
  | expr LEQ  expr                        { Binop($1, Leq, $3) }
  | expr GT  expr                         { Binop($1, Gt, $3) }
  | expr GEQ  expr                        { Binop($1, Geq, $3) }
  | expr AND  expr                        { Binop($1, And, $3) }
  | expr OR  expr                         { Binop($1, Or, $3) }

args_opt:
  /*nothing*/ { [] }
  | args_list { $1 }

args_list:
  expr                   { [$1] }
  | args_list COMMA expr { $3 :: $1 }

array_opt:
  /*nothing*/            { [] }
  | expr                 { [$1] }
  | array_opt COMMA expr { $3 :: $1 }

dtype:
  INT           { Int }
  | FLOAT       { Float }
  | STRING      { String }
  | BOOL        { Bool }
  | VOID        { Void }
  | ARRAY dtype { Array($2) }
  | STRUCT_ID   { Struct($1) }
