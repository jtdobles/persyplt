%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA ARROPEN ARRCLOSE DOT
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN NOT
%token PLUS_ASSIGN MINUS_ASSIGN TIMES_ASSIGN DIVIDE_ASSIGN
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE WHILE INT BOOL FLOAT STRING VOID FUNC
%token CONT EXIT
%token <int> INTLIT
%token <float> FLOATLIT
%token <string> STRLIT
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS TIMES DIVIDE MOD
%left PLUS_ASSIGN MINUS_ASSIGN TIMES_ASSIGN DIVIDE_ASSIGN
%nonassoc INCR DECR
%right NOT NEG
%left DOT

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], [] }  
 | decls fdecl { fst $1, ($2 :: snd $1) }
 | decls stmt { ($2 :: fst $1), snd $1 }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { typ = $1;
	   fname = $2;
	   formals = $4;
       fstmts = List.rev $7;
     } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT { Int }
  | BOOL { Bool }
  | VOID { Void }
  | FLOAT { Float }
  | STRING { String }
  | typ FUNC { Function($1) }
  
type_list:
    typ            { [$1] }
  | typ COMMA type_list { $1 :: $3 }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | typ ID SEMI { Dec($1, $2) }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5, Expr(Noexpr)) }
  | CONT SEMI { Cont Noexpr }
  | EXIT SEMI { Exit Noexpr }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    INTLIT   { IntLit($1) }
  | FLOATLIT { FloatLit($1) }
  | STRLIT   { StringLit($1) }
  | TRUE     { BoolLit(true) }
  | FALSE    { BoolLit(false) }
  | ID       { Id($1) }

  | MINUS expr %prec NEG        { Unop(Neg, $2) }
  | NOT expr                    { Unop(Not, $2) }
  | expr PLUS PLUS %prec INCR   { Unop(Incr, $1) }
  | expr MINUS MINUS %prec DECR { Unop(Decr, $1) }

  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr MOD expr    { Binop($1, Mod, $3) }

  | ID PLUS_ASSIGN expr   { OpAssign($1, Add, $3) }
  | ID MINUS_ASSIGN expr  { OpAssign($1, Sub, $3) }
  | ID TIMES_ASSIGN expr  { OpAssign($1, Mult, $3) }
  | ID DIVIDE_ASSIGN expr { OpAssign($1, Div, $3) }
 
  | expr EQ  expr { Binop($1, Equal, $3) }
  | expr NEQ expr { Binop($1, Neq,   $3) }
  | expr LT  expr { Binop($1, Less,  $3) }
  | expr LEQ expr { Binop($1, Leq,   $3) }
  | expr GT  expr { Binop($1, Greater, $3) }
  | expr GEQ expr { Binop($1, Geq,   $3) }
  | expr AND expr { Binop($1, And,   $3) }
  | expr OR  expr { Binop($1, Or,    $3) }

  | ID ASSIGN expr     { Assign($1, $3) }
  | typ ID ASSIGN expr { DecAssign($1, $2, $4) }

  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }