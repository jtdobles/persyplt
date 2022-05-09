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

open Parsing;;
let _ = parse_error;;
# 3 "dparse.mly"
 open Ast 
# 56 "dparse.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* LBRACE *);
  260 (* RBRACE *);
  261 (* LBRACKET *);
  262 (* RBRACKET *);
  263 (* NOT *);
  264 (* PLUS *);
  265 (* MINUS *);
  266 (* TIMES *);
  267 (* DIVIDE *);
  268 (* MOD *);
  269 (* EQ *);
  270 (* NEQ *);
  271 (* LT *);
  272 (* LEQ *);
  273 (* GT *);
  274 (* GEQ *);
  275 (* AND *);
  276 (* OR *);
  277 (* ASSIGN *);
  278 (* SEMI *);
  279 (* DOT *);
  280 (* COMMA *);
  281 (* IF *);
  282 (* ELSE *);
  283 (* FOR *);
  284 (* WHILE *);
  285 (* RETURN *);
  286 (* PRINTF *);
  287 (* TRUE *);
  288 (* FALSE *);
  289 (* INT *);
  290 (* FLOAT *);
  291 (* STRING *);
  292 (* BOOL *);
  293 (* NULL *);
  294 (* VOID *);
  295 (* ARRAY *);
  296 (* STRUCT *);
  297 (* MAIN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  298 (* INT_L *);
  299 (* FLOAT_L *);
  300 (* ID *);
  301 (* STRING_L *);
  302 (* STRUCT_ID *);
  303 (* BOOL_L *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\002\000\003\000\004\000\004\000\008\000\
\006\000\006\000\009\000\009\000\010\000\007\000\007\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\014\000\014\000\015\000\015\000\
\013\000\013\000\013\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\006\000\008\000\000\000\002\000\003\000\
\000\000\001\000\002\000\004\000\001\000\000\000\002\000\002\000\
\003\000\003\000\005\000\006\000\005\000\007\000\008\000\005\000\
\003\000\003\000\002\000\001\000\001\000\001\000\001\000\003\000\
\001\000\003\000\006\000\004\000\003\000\005\000\004\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\000\000\001\000\001\000\003\000\
\000\000\001\000\003\000\001\000\001\000\001\000\001\000\001\000\
\002\000\001\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\060\000\061\000\062\000\063\000\064\000\
\000\000\000\000\066\000\002\000\003\000\000\000\065\000\000\000\
\000\000\006\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\007\000\011\000\000\000\000\000\004\000\000\000\014\000\
\000\000\008\000\000\000\012\000\000\000\014\000\005\000\000\000\
\000\000\000\000\000\000\000\000\000\000\028\000\029\000\000\000\
\030\000\031\000\000\000\000\000\015\000\000\000\000\000\000\000\
\000\000\000\000\027\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\016\000\026\000\017\000\032\000\000\000\000\000\000\000\
\000\000\025\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\018\000\000\000\040\000\041\000\042\000\043\000\044\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\039\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\024\000\000\000\000\000\000\000\
\000\000\019\000\000\000\000\000\000\000\020\000\022\000\000\000\
\023\000"

let yydgoto = "\002\000\
\003\000\012\000\013\000\020\000\051\000\022\000\035\000\026\000\
\023\000\052\000\053\000\054\000\058\000\092\000\093\000"

let yysindex = "\004\000\
\000\000\000\000\108\255\000\000\000\000\000\000\000\000\000\000\
\155\255\216\254\000\000\000\000\000\000\221\254\000\000\007\255\
\016\255\000\000\155\255\061\255\233\254\020\255\022\255\008\255\
\003\255\000\000\000\000\047\255\155\255\000\000\033\255\000\000\
\028\255\000\000\081\255\000\000\019\255\000\000\000\000\019\255\
\019\255\076\255\078\255\088\255\019\255\000\000\000\000\006\255\
\000\000\000\000\057\255\070\255\000\000\255\000\185\000\128\255\
\085\001\068\255\000\000\019\255\222\255\019\255\014\001\019\255\
\019\255\058\255\253\254\019\255\019\255\019\255\019\255\019\255\
\019\255\019\255\019\255\019\255\019\255\019\255\019\255\019\255\
\019\255\000\000\000\000\000\000\000\000\019\255\204\000\019\255\
\223\000\000\000\085\001\102\255\089\255\029\001\084\255\019\255\
\019\255\000\000\085\001\000\000\000\000\000\000\000\000\000\000\
\004\255\004\255\004\255\004\255\004\255\004\255\098\001\098\001\
\085\001\222\255\042\001\222\255\000\000\019\255\091\255\019\255\
\057\001\070\001\104\255\019\255\000\000\085\001\019\255\085\001\
\112\255\000\000\222\255\242\000\085\001\000\000\000\000\222\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\136\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\135\255\000\000\000\000\000\000\137\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\074\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\224\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\087\255\000\000\000\000\000\000\000\000\000\000\000\000\143\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\043\255\000\000\147\255\000\000\012\000\000\000\
\000\000\000\000\051\255\000\000\000\000\000\000\000\000\000\000\
\050\000\069\000\088\000\107\000\126\000\145\000\151\000\166\000\
\097\255\000\000\000\000\000\000\000\000\000\000\031\000\000\000\
\000\000\000\000\175\255\000\000\000\000\046\255\000\000\116\255\
\000\000\000\000\000\000\000\000\163\255\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\049\000\000\000\112\000\000\000\
\000\000\000\000\196\255\219\255\000\000\000\000\000\000"

let yytablesize = 628
let yytable = "\055\000\
\088\000\096\000\057\000\059\000\001\000\016\000\064\000\063\000\
\017\000\018\000\065\000\069\000\070\000\071\000\072\000\073\000\
\019\000\097\000\098\000\037\000\027\000\028\000\087\000\040\000\
\089\000\041\000\091\000\094\000\066\000\030\000\099\000\100\000\
\101\000\102\000\103\000\104\000\105\000\106\000\107\000\108\000\
\109\000\110\000\111\000\112\000\055\000\029\000\031\000\056\000\
\113\000\032\000\115\000\014\000\034\000\123\000\034\000\125\000\
\034\000\015\000\121\000\122\000\046\000\047\000\048\000\049\000\
\024\000\050\000\055\000\021\000\025\000\056\000\135\000\036\000\
\034\000\085\000\034\000\137\000\060\000\033\000\061\000\057\000\
\126\000\037\000\128\000\038\000\039\000\040\000\132\000\041\000\
\062\000\133\000\068\000\086\000\058\000\004\000\005\000\006\000\
\007\000\057\000\008\000\009\000\067\000\095\000\059\000\117\000\
\120\000\042\000\011\000\043\000\044\000\045\000\058\000\127\000\
\118\000\004\000\005\000\006\000\007\000\038\000\008\000\009\000\
\059\000\038\000\046\000\047\000\048\000\049\000\011\000\050\000\
\037\000\131\000\038\000\084\000\040\000\134\000\041\000\067\000\
\009\000\038\000\010\000\038\000\004\000\005\000\006\000\007\000\
\053\000\008\000\009\000\010\000\054\000\056\000\000\000\000\000\
\042\000\011\000\043\000\044\000\045\000\000\000\000\000\000\000\
\004\000\005\000\006\000\007\000\035\000\008\000\009\000\000\000\
\035\000\046\000\047\000\048\000\049\000\011\000\050\000\021\000\
\000\000\021\000\021\000\021\000\000\000\021\000\000\000\000\000\
\035\000\000\000\035\000\004\000\005\000\006\000\007\000\000\000\
\008\000\009\000\000\000\000\000\000\000\000\000\000\000\021\000\
\011\000\021\000\021\000\021\000\000\000\000\000\000\000\021\000\
\021\000\021\000\021\000\000\000\021\000\021\000\000\000\000\000\
\021\000\021\000\021\000\021\000\021\000\021\000\037\000\000\000\
\038\000\033\000\040\000\000\000\041\000\033\000\000\000\033\000\
\033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
\033\000\033\000\033\000\033\000\013\000\033\000\042\000\033\000\
\043\000\044\000\045\000\000\000\000\000\000\000\004\000\005\000\
\006\000\007\000\000\000\008\000\009\000\000\000\000\000\046\000\
\047\000\048\000\049\000\011\000\050\000\037\000\000\000\000\000\
\000\000\037\000\000\000\037\000\037\000\037\000\037\000\037\000\
\037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
\036\000\037\000\000\000\037\000\036\000\000\000\036\000\036\000\
\036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
\036\000\036\000\036\000\045\000\036\000\000\000\036\000\045\000\
\000\000\000\000\000\000\000\000\000\000\000\000\045\000\045\000\
\045\000\045\000\045\000\045\000\045\000\045\000\046\000\045\000\
\000\000\045\000\046\000\000\000\000\000\000\000\000\000\000\000\
\000\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
\046\000\047\000\046\000\000\000\046\000\047\000\000\000\000\000\
\000\000\000\000\000\000\000\000\047\000\047\000\047\000\047\000\
\047\000\047\000\047\000\047\000\048\000\047\000\000\000\047\000\
\048\000\000\000\000\000\000\000\000\000\000\000\000\000\048\000\
\048\000\048\000\048\000\048\000\048\000\048\000\048\000\049\000\
\048\000\000\000\048\000\049\000\000\000\000\000\000\000\000\000\
\000\000\000\000\049\000\049\000\049\000\049\000\049\000\049\000\
\049\000\049\000\050\000\049\000\000\000\049\000\050\000\000\000\
\051\000\000\000\000\000\000\000\051\000\050\000\050\000\050\000\
\050\000\050\000\050\000\050\000\050\000\000\000\050\000\052\000\
\050\000\051\000\051\000\052\000\051\000\000\000\051\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\052\000\052\000\083\000\052\000\000\000\052\000\000\000\000\000\
\069\000\070\000\071\000\072\000\073\000\074\000\075\000\076\000\
\077\000\078\000\079\000\080\000\081\000\114\000\000\000\000\000\
\000\000\000\000\000\000\069\000\070\000\071\000\072\000\073\000\
\074\000\075\000\076\000\077\000\078\000\079\000\080\000\081\000\
\116\000\000\000\000\000\000\000\000\000\000\000\069\000\070\000\
\071\000\072\000\073\000\074\000\075\000\076\000\077\000\078\000\
\079\000\080\000\081\000\136\000\000\000\000\000\000\000\000\000\
\000\000\069\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\079\000\080\000\081\000\069\000\070\000\
\071\000\072\000\073\000\074\000\075\000\076\000\077\000\078\000\
\079\000\080\000\081\000\000\000\082\000\069\000\070\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\079\000\
\080\000\081\000\119\000\090\000\069\000\070\000\071\000\072\000\
\073\000\074\000\075\000\076\000\077\000\078\000\079\000\080\000\
\081\000\069\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\079\000\080\000\081\000\129\000\124\000\
\069\000\070\000\071\000\072\000\073\000\074\000\075\000\076\000\
\077\000\078\000\079\000\080\000\081\000\069\000\070\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\079\000\
\080\000\081\000\000\000\130\000\069\000\070\000\071\000\072\000\
\073\000\074\000\075\000\076\000\077\000\078\000\079\000\080\000\
\081\000\069\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\079\000"

let yycheck = "\037\000\
\061\000\005\001\040\000\041\000\001\000\046\001\001\001\045\000\
\044\001\003\001\005\001\008\001\009\001\010\001\011\001\012\001\
\001\001\021\001\022\001\001\001\044\001\002\001\060\000\005\001\
\062\000\007\001\064\000\065\000\023\001\022\001\068\000\069\000\
\070\000\071\000\072\000\073\000\074\000\075\000\076\000\077\000\
\078\000\079\000\080\000\081\000\002\001\024\001\044\001\002\001\
\086\000\003\001\088\000\003\000\002\001\114\000\022\001\116\000\
\006\001\009\000\096\000\097\000\042\001\043\001\044\001\045\001\
\004\001\047\001\024\001\019\000\020\000\024\001\131\000\044\001\
\022\001\006\001\024\001\136\000\001\001\029\000\001\001\006\001\
\118\000\001\001\120\000\003\001\004\001\005\001\124\000\007\001\
\001\001\127\000\021\001\024\001\006\001\033\001\034\001\035\001\
\036\001\024\001\038\001\039\001\044\001\044\001\006\001\002\001\
\021\001\025\001\046\001\027\001\028\001\029\001\024\001\021\001\
\024\001\033\001\034\001\035\001\036\001\002\001\038\001\039\001\
\024\001\006\001\042\001\043\001\044\001\045\001\046\001\047\001\
\001\001\026\001\003\001\004\001\005\001\022\001\007\001\000\000\
\002\001\022\001\002\001\024\001\033\001\034\001\035\001\036\001\
\002\001\038\001\039\001\040\001\002\001\038\000\255\255\255\255\
\025\001\046\001\027\001\028\001\029\001\255\255\255\255\255\255\
\033\001\034\001\035\001\036\001\002\001\038\001\039\001\255\255\
\006\001\042\001\043\001\044\001\045\001\046\001\047\001\001\001\
\255\255\003\001\004\001\005\001\255\255\007\001\255\255\255\255\
\022\001\255\255\024\001\033\001\034\001\035\001\036\001\255\255\
\038\001\039\001\255\255\255\255\255\255\255\255\255\255\025\001\
\046\001\027\001\028\001\029\001\255\255\255\255\255\255\033\001\
\034\001\035\001\036\001\255\255\038\001\039\001\255\255\255\255\
\042\001\043\001\044\001\045\001\046\001\047\001\001\001\255\255\
\003\001\002\001\005\001\255\255\007\001\006\001\255\255\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\025\001\024\001\
\027\001\028\001\029\001\255\255\255\255\255\255\033\001\034\001\
\035\001\036\001\255\255\038\001\039\001\255\255\255\255\042\001\
\043\001\044\001\045\001\046\001\047\001\002\001\255\255\255\255\
\255\255\006\001\255\255\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\002\001\022\001\255\255\024\001\006\001\255\255\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\002\001\022\001\255\255\024\001\006\001\
\255\255\255\255\255\255\255\255\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\002\001\022\001\
\255\255\024\001\006\001\255\255\255\255\255\255\255\255\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\002\001\022\001\255\255\024\001\006\001\255\255\255\255\
\255\255\255\255\255\255\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\002\001\022\001\255\255\024\001\
\006\001\255\255\255\255\255\255\255\255\255\255\255\255\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\002\001\
\022\001\255\255\024\001\006\001\255\255\255\255\255\255\255\255\
\255\255\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\002\001\022\001\255\255\024\001\006\001\255\255\
\002\001\255\255\255\255\255\255\006\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\255\255\022\001\002\001\
\024\001\019\001\020\001\006\001\022\001\255\255\024\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\019\001\020\001\002\001\022\001\255\255\024\001\255\255\255\255\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\002\001\255\255\255\255\
\255\255\255\255\255\255\008\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\002\001\255\255\255\255\255\255\255\255\255\255\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\002\001\255\255\255\255\255\255\255\255\
\255\255\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\255\255\022\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\006\001\022\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\006\001\022\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\255\255\022\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACKET\000\
  RBRACKET\000\
  NOT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MOD\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  AND\000\
  OR\000\
  ASSIGN\000\
  SEMI\000\
  DOT\000\
  COMMA\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  RETURN\000\
  PRINTF\000\
  TRUE\000\
  FALSE\000\
  INT\000\
  FLOAT\000\
  STRING\000\
  BOOL\000\
  NULL\000\
  VOID\000\
  ARRAY\000\
  STRUCT\000\
  MAIN\000\
  EOF\000\
  "

let yynames_block = "\
  INT_L\000\
  FLOAT_L\000\
  ID\000\
  STRING_L\000\
  STRUCT_ID\000\
  BOOL_L\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "dparse.mly"
  ( [], [] )
# 424 "dparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'struct_decl) in
    Obj.repr(
# 43 "dparse.mly"
                        ( (_2 :: fst _1), snd _1 )
# 432 "dparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 44 "dparse.mly"
                      ( fst _1, (_2 :: snd _1) )
# 440 "dparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'struct_stmt_list) in
    Obj.repr(
# 48 "dparse.mly"
  ({ 
    stname = _2;
    members = List.rev _4;
  })
# 451 "dparse.ml"
               : 'struct_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'dtype) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 55 "dparse.mly"
  ({
    rtyp = _1;
    fname = _2;
    formals = _4;
    fstmts = List.rev _7;
  })
# 466 "dparse.ml"
               : 'func_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "dparse.mly"
                                ( [] )
# 472 "dparse.ml"
               : 'struct_stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'struct_stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'struc_stmt) in
    Obj.repr(
# 64 "dparse.mly"
                                ( _2 :: _1 )
# 480 "dparse.ml"
               : 'struct_stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'dtype) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 67 "dparse.mly"
                ( (_1, _2) )
# 488 "dparse.ml"
               : 'struc_stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "dparse.mly"
                 ( [] )
# 494 "dparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 71 "dparse.mly"
                 ( _1 )
# 501 "dparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'dtype) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 74 "dparse.mly"
                                ( [(_1, _2)] )
# 509 "dparse.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'dtype) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 75 "dparse.mly"
                                ( (_3, _4) :: _1 )
# 518 "dparse.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 78 "dparse.mly"
     ( Id(_1) )
# 525 "dparse.ml"
               : 'vname))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "dparse.mly"
                    ( [] )
# 531 "dparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 82 "dparse.mly"
                    ( _2 :: _1 )
# 539 "dparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 85 "dparse.mly"
                                               ( Expr _1 )
# 546 "dparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 86 "dparse.mly"
                                               ( Block(List.rev _2) )
# 553 "dparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'dtype) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 87 "dparse.mly"
                                               ( VarDecl(_1, _2, Noexpr(_1)) )
# 561 "dparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'dtype) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 88 "dparse.mly"
                                               ( VarDecl(_1, _2, _4) )
# 570 "dparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'dtype) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 89 "dparse.mly"
                                               ( ArrayDecl(_1, _2, _4, Noexpr(_1)))
# 579 "dparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 90 "dparse.mly"
                                               ( If(_3, _5, Block([])) )
# 587 "dparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 91 "dparse.mly"
                                               ( If(_3, _5, _7) )
# 596 "dparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'stmt) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 92 "dparse.mly"
                                               ( For(_3, _4, _6, _8) )
# 606 "dparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 93 "dparse.mly"
                                               ( While (_3, _5) )
# 614 "dparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 94 "dparse.mly"
                                               ( Return _2 )
# 621 "dparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 97 "dparse.mly"
                                          ( _2 )
# 628 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "dparse.mly"
                                          ( Not(_2) )
# 635 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 99 "dparse.mly"
                                          ( IntLit(_1) )
# 642 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 100 "dparse.mly"
                                          ( FloatLit(_1) )
# 649 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 101 "dparse.mly"
                                          ( StringLit(_1) )
# 656 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 102 "dparse.mly"
                                          ( BoolLit(_1) )
# 663 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'array_opt) in
    Obj.repr(
# 103 "dparse.mly"
                                          ( ArrayLit(List.rev _2) )
# 670 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 104 "dparse.mly"
                                          ( Id(_1) )
# 677 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vname) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "dparse.mly"
                                          ( Assign(_1, _3) )
# 685 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "dparse.mly"
                                          ( ArrayAssign(Id(_1), _3, _6) )
# 694 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 107 "dparse.mly"
                                          (ArrayIndex(Id(_1), _3))
# 702 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 108 "dparse.mly"
                                          ( StructUse(Id(_1), Id(_3)) )
# 710 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "dparse.mly"
                                          ( StructAssign(Id(_1), Id(_3), _5) )
# 719 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 110 "dparse.mly"
                                          ( Call (_1, _3) )
# 727 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "dparse.mly"
                                          ( Binop(_1, Add, _3) )
# 735 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "dparse.mly"
                                          ( Binop(_1, Sub, _3) )
# 743 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "dparse.mly"
                                          ( Binop(_1, Mult, _3) )
# 751 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "dparse.mly"
                                          ( Binop(_1, Div, _3) )
# 759 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "dparse.mly"
                                          ( Binop(_1, Mod, _3) )
# 767 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "dparse.mly"
                                          ( Binop(_1, Eq, _3) )
# 775 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "dparse.mly"
                                          ( Binop(_1, Neq, _3) )
# 783 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "dparse.mly"
                                          ( Binop(_1, Lt, _3) )
# 791 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "dparse.mly"
                                          ( Binop(_1, Leq, _3) )
# 799 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "dparse.mly"
                                          ( Binop(_1, Gt, _3) )
# 807 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "dparse.mly"
                                          ( Binop(_1, Geq, _3) )
# 815 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "dparse.mly"
                                          ( Binop(_1, And, _3) )
# 823 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "dparse.mly"
                                          ( Binop(_1, Or, _3) )
# 831 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "dparse.mly"
              ( [] )
# 837 "dparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 127 "dparse.mly"
              ( _1 )
# 844 "dparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "dparse.mly"
                         ( [_1] )
# 851 "dparse.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "dparse.mly"
                         ( _3 :: _1 )
# 859 "dparse.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "dparse.mly"
                         ( [] )
# 865 "dparse.ml"
               : 'array_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "dparse.mly"
                         ( [_1] )
# 872 "dparse.ml"
               : 'array_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'array_opt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "dparse.mly"
                         ( _3 :: _1 )
# 880 "dparse.ml"
               : 'array_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 139 "dparse.mly"
                ( Int )
# 886 "dparse.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "dparse.mly"
                ( Float )
# 892 "dparse.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "dparse.mly"
                ( String )
# 898 "dparse.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 142 "dparse.mly"
                ( Bool )
# 904 "dparse.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "dparse.mly"
                ( Void )
# 910 "dparse.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'dtype) in
    Obj.repr(
# 144 "dparse.mly"
                ( Array(_2) )
# 917 "dparse.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 145 "dparse.mly"
                ( Struct(_1) )
# 924 "dparse.ml"
               : 'dtype))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
