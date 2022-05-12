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

open Parsing;;
let _ = parse_error;;
# 3 "dparse.mly"
 open Ast 
# 60 "dparse.ml"
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
  269 (* PLUS_ASSIGN *);
  270 (* MINUS_ASSIGN *);
  271 (* TIMES_ASSIGN *);
  272 (* DIVIDE_ASSIGN *);
  273 (* EQ *);
  274 (* NEQ *);
  275 (* LT *);
  276 (* LEQ *);
  277 (* GT *);
  278 (* GEQ *);
  279 (* AND *);
  280 (* OR *);
  281 (* ASSIGN *);
  282 (* SEMI *);
  283 (* DOT *);
  284 (* COMMA *);
  285 (* IF *);
  286 (* ELSE *);
  287 (* FOR *);
  288 (* WHILE *);
  289 (* RETURN *);
  290 (* PRINTF *);
  291 (* TRUE *);
  292 (* FALSE *);
  293 (* INT *);
  294 (* FLOAT *);
  295 (* STRING *);
  296 (* BOOL *);
  297 (* NULL *);
  298 (* VOID *);
  299 (* ARRAY *);
  300 (* STRUCT *);
  301 (* MAIN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  302 (* INT_L *);
  303 (* FLOAT_L *);
  304 (* ID *);
  305 (* STRING_L *);
  306 (* STRUCT_ID *);
  307 (* BOOL_L *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\002\000\003\000\004\000\004\000\008\000\
\006\000\006\000\009\000\009\000\010\000\007\000\007\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\014\000\014\000\015\000\015\000\013\000\013\000\
\013\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\006\000\008\000\000\000\002\000\003\000\
\000\000\001\000\002\000\004\000\001\000\000\000\002\000\002\000\
\003\000\003\000\005\000\006\000\005\000\007\000\008\000\005\000\
\003\000\003\000\002\000\001\000\001\000\001\000\001\000\003\000\
\001\000\003\000\006\000\004\000\003\000\005\000\004\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\000\000\001\000\001\000\003\000\000\000\001\000\
\003\000\001\000\001\000\001\000\001\000\001\000\002\000\001\000\
\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\066\000\067\000\068\000\069\000\070\000\
\000\000\000\000\072\000\002\000\003\000\000\000\071\000\000\000\
\000\000\006\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\007\000\011\000\000\000\000\000\004\000\000\000\014\000\
\000\000\008\000\000\000\012\000\000\000\014\000\005\000\000\000\
\000\000\000\000\000\000\000\000\000\000\028\000\029\000\000\000\
\030\000\031\000\000\000\000\000\015\000\000\000\000\000\000\000\
\000\000\000\000\027\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\016\000\026\000\017\000\
\032\000\000\000\000\000\000\000\000\000\025\000\000\000\000\000\
\000\000\000\000\042\000\043\000\044\000\045\000\000\000\000\000\
\000\000\018\000\000\000\040\000\046\000\041\000\047\000\048\000\
\049\000\050\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\039\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\024\000\000\000\
\000\000\000\000\000\000\019\000\000\000\000\000\000\000\020\000\
\022\000\000\000\023\000"

let yydgoto = "\002\000\
\003\000\012\000\013\000\020\000\051\000\022\000\035\000\026\000\
\023\000\052\000\053\000\054\000\058\000\096\000\097\000"

let yysindex = "\001\000\
\000\000\000\000\167\001\000\000\000\000\000\000\000\000\000\000\
\173\000\211\254\000\000\000\000\000\000\214\254\000\000\007\255\
\008\255\000\000\173\000\175\255\221\254\012\255\244\254\249\254\
\228\254\000\000\000\000\018\255\173\000\000\000\252\254\000\000\
\234\254\000\000\070\255\000\000\202\255\000\000\000\000\202\255\
\202\255\050\255\051\255\061\255\202\255\000\000\000\000\204\000\
\000\000\000\000\001\255\045\255\000\000\054\001\224\000\121\255\
\164\001\006\255\000\000\202\255\195\255\202\255\073\001\202\255\
\202\255\202\255\202\255\202\255\202\255\024\255\080\255\202\255\
\010\255\083\255\202\255\202\255\202\255\202\255\202\255\202\255\
\202\255\202\255\202\255\202\255\202\255\000\000\000\000\000\000\
\000\000\202\255\247\000\202\255\014\001\000\000\164\001\076\255\
\055\255\092\001\000\000\000\000\000\000\000\000\069\255\202\255\
\202\255\000\000\164\001\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\130\255\130\255\130\255\130\255\130\255\130\255\
\181\001\181\001\164\001\195\255\109\001\195\255\000\000\202\255\
\071\255\202\255\128\001\145\001\065\255\202\255\000\000\164\001\
\202\255\164\001\072\255\000\000\195\255\037\001\164\001\000\000\
\000\000\195\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\104\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\112\255\000\000\000\000\000\000\125\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\048\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\250\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\054\255\000\000\000\000\000\000\000\000\000\000\000\000\131\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\005\255\000\000\
\134\255\000\000\000\000\000\000\000\000\000\000\019\000\000\000\
\000\000\000\000\109\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\065\000\088\000\111\000\134\000\157\000\180\000\
\072\000\095\000\059\255\000\000\000\000\000\000\000\000\000\000\
\042\000\000\000\000\000\000\000\144\255\000\000\000\000\022\255\
\000\000\118\000\000\000\000\000\000\000\000\000\139\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\060\000\000\000\085\000\000\000\
\000\000\000\000\196\255\219\255\000\000\000\000\000\000"

let yytablesize = 729
let yytable = "\055\000\
\092\000\001\000\057\000\059\000\016\000\017\000\061\000\063\000\
\019\000\018\000\037\000\089\000\027\000\028\000\040\000\029\000\
\041\000\108\000\030\000\031\000\032\000\034\000\091\000\062\000\
\093\000\036\000\095\000\098\000\099\000\100\000\101\000\102\000\
\061\000\090\000\107\000\109\000\111\000\112\000\113\000\114\000\
\115\000\116\000\117\000\118\000\119\000\120\000\121\000\122\000\
\071\000\062\000\060\000\061\000\123\000\063\000\125\000\046\000\
\047\000\048\000\049\000\064\000\050\000\062\000\014\000\133\000\
\065\000\135\000\131\000\132\000\015\000\072\000\037\000\103\000\
\038\000\039\000\040\000\063\000\041\000\127\000\021\000\025\000\
\145\000\064\000\128\000\037\000\104\000\147\000\065\000\040\000\
\033\000\041\000\136\000\110\000\138\000\130\000\141\000\137\000\
\142\000\144\000\042\000\143\000\043\000\044\000\045\000\073\000\
\105\000\106\000\004\000\005\000\006\000\007\000\034\000\008\000\
\009\000\009\000\034\000\046\000\047\000\048\000\049\000\011\000\
\050\000\037\000\056\000\038\000\088\000\040\000\010\000\041\000\
\046\000\047\000\048\000\049\000\059\000\050\000\034\000\060\000\
\034\000\073\000\074\000\075\000\076\000\077\000\000\000\000\000\
\021\000\000\000\021\000\021\000\021\000\042\000\021\000\043\000\
\044\000\045\000\000\000\000\000\000\000\004\000\005\000\006\000\
\007\000\000\000\008\000\009\000\000\000\000\000\046\000\047\000\
\048\000\049\000\011\000\050\000\021\000\000\000\021\000\021\000\
\021\000\000\000\024\000\000\000\021\000\021\000\021\000\021\000\
\000\000\021\000\021\000\000\000\000\000\021\000\021\000\021\000\
\021\000\021\000\021\000\037\000\000\000\038\000\000\000\040\000\
\000\000\041\000\037\000\000\000\000\000\000\000\040\000\000\000\
\041\000\000\000\000\000\004\000\005\000\006\000\007\000\000\000\
\008\000\009\000\000\000\000\000\000\000\000\000\000\000\042\000\
\011\000\043\000\044\000\045\000\000\000\000\000\000\000\004\000\
\005\000\006\000\007\000\000\000\008\000\009\000\000\000\000\000\
\046\000\047\000\048\000\049\000\011\000\050\000\000\000\046\000\
\047\000\048\000\049\000\033\000\050\000\000\000\000\000\033\000\
\000\000\033\000\033\000\033\000\033\000\033\000\000\000\000\000\
\000\000\000\000\033\000\033\000\033\000\033\000\033\000\033\000\
\033\000\033\000\013\000\033\000\037\000\033\000\000\000\000\000\
\037\000\000\000\037\000\037\000\037\000\037\000\037\000\000\000\
\000\000\000\000\000\000\037\000\037\000\037\000\037\000\037\000\
\037\000\037\000\037\000\036\000\037\000\000\000\037\000\036\000\
\000\000\036\000\036\000\036\000\036\000\036\000\000\000\000\000\
\000\000\000\000\036\000\036\000\036\000\036\000\036\000\036\000\
\036\000\036\000\051\000\036\000\000\000\036\000\051\000\000\000\
\000\000\057\000\000\000\000\000\000\000\057\000\000\000\000\000\
\000\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
\051\000\052\000\051\000\000\000\051\000\052\000\057\000\057\000\
\058\000\057\000\000\000\057\000\058\000\000\000\000\000\000\000\
\052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
\053\000\052\000\000\000\052\000\053\000\058\000\058\000\038\000\
\058\000\000\000\058\000\038\000\000\000\000\000\000\000\053\000\
\053\000\053\000\053\000\053\000\053\000\053\000\053\000\054\000\
\053\000\000\000\053\000\054\000\035\000\000\000\000\000\038\000\
\035\000\038\000\000\000\000\000\000\000\000\000\054\000\054\000\
\054\000\054\000\054\000\054\000\054\000\054\000\055\000\054\000\
\000\000\054\000\055\000\000\000\035\000\000\000\035\000\000\000\
\000\000\000\000\000\000\000\000\000\000\055\000\055\000\055\000\
\055\000\055\000\055\000\055\000\055\000\056\000\055\000\000\000\
\055\000\056\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\056\000\056\000\056\000\056\000\
\056\000\056\000\056\000\056\000\064\000\056\000\000\000\056\000\
\065\000\004\000\005\000\006\000\007\000\000\000\008\000\009\000\
\066\000\067\000\068\000\069\000\000\000\000\000\011\000\000\000\
\000\000\087\000\000\000\000\000\000\000\000\000\070\000\073\000\
\074\000\075\000\076\000\077\000\000\000\000\000\000\000\000\000\
\078\000\079\000\080\000\081\000\082\000\083\000\084\000\085\000\
\124\000\000\000\000\000\000\000\000\000\000\000\073\000\074\000\
\075\000\076\000\077\000\000\000\000\000\000\000\000\000\078\000\
\079\000\080\000\081\000\082\000\083\000\084\000\085\000\126\000\
\000\000\000\000\000\000\000\000\000\000\073\000\074\000\075\000\
\076\000\077\000\000\000\000\000\000\000\000\000\078\000\079\000\
\080\000\081\000\082\000\083\000\084\000\085\000\146\000\000\000\
\000\000\000\000\000\000\000\000\073\000\074\000\075\000\076\000\
\077\000\000\000\000\000\000\000\000\000\078\000\079\000\080\000\
\081\000\082\000\083\000\084\000\085\000\073\000\074\000\075\000\
\076\000\077\000\000\000\000\000\000\000\000\000\078\000\079\000\
\080\000\081\000\082\000\083\000\084\000\085\000\000\000\086\000\
\073\000\074\000\075\000\076\000\077\000\000\000\000\000\000\000\
\000\000\078\000\079\000\080\000\081\000\082\000\083\000\084\000\
\085\000\129\000\094\000\073\000\074\000\075\000\076\000\077\000\
\000\000\000\000\000\000\000\000\078\000\079\000\080\000\081\000\
\082\000\083\000\084\000\085\000\073\000\074\000\075\000\076\000\
\077\000\000\000\000\000\000\000\000\000\078\000\079\000\080\000\
\081\000\082\000\083\000\084\000\085\000\139\000\134\000\073\000\
\074\000\075\000\076\000\077\000\000\000\000\000\000\000\000\000\
\078\000\079\000\080\000\081\000\082\000\083\000\084\000\085\000\
\073\000\074\000\075\000\076\000\077\000\000\000\000\000\000\000\
\000\000\078\000\079\000\080\000\081\000\082\000\083\000\084\000\
\085\000\000\000\140\000\073\000\074\000\075\000\076\000\077\000\
\000\000\000\000\000\000\000\000\078\000\079\000\080\000\081\000\
\082\000\083\000\084\000\085\000\073\000\074\000\075\000\076\000\
\077\000\000\000\000\000\000\000\000\000\078\000\079\000\080\000\
\081\000\082\000\083\000\004\000\005\000\006\000\007\000\000\000\
\008\000\009\000\010\000\000\000\000\000\000\000\000\000\000\000\
\011\000"

let yycheck = "\037\000\
\061\000\001\000\040\000\041\000\050\001\048\001\002\001\045\000\
\001\001\003\001\001\001\006\001\048\001\002\001\005\001\028\001\
\007\001\008\001\026\001\048\001\003\001\026\001\060\000\002\001\
\062\000\048\001\064\000\065\000\066\000\067\000\068\000\069\000\
\028\001\028\001\072\000\073\000\074\000\075\000\076\000\077\000\
\078\000\079\000\080\000\081\000\082\000\083\000\084\000\085\000\
\048\001\028\001\001\001\001\001\090\000\006\001\092\000\046\001\
\047\001\048\001\049\001\006\001\051\001\001\001\003\000\124\000\
\006\001\126\000\104\000\105\000\009\000\025\001\001\001\048\001\
\003\001\004\001\005\001\028\001\007\001\002\001\019\000\020\000\
\141\000\028\001\028\001\001\001\005\001\146\000\028\001\005\001\
\029\000\007\001\128\000\009\001\130\000\025\001\030\001\025\001\
\134\000\026\001\029\001\137\000\031\001\032\001\033\001\000\000\
\025\001\026\001\037\001\038\001\039\001\040\001\002\001\042\001\
\043\001\002\001\006\001\046\001\047\001\048\001\049\001\050\001\
\051\001\001\001\038\000\003\001\004\001\005\001\002\001\007\001\
\046\001\047\001\048\001\049\001\002\001\051\001\026\001\002\001\
\028\001\008\001\009\001\010\001\011\001\012\001\255\255\255\255\
\001\001\255\255\003\001\004\001\005\001\029\001\007\001\031\001\
\032\001\033\001\255\255\255\255\255\255\037\001\038\001\039\001\
\040\001\255\255\042\001\043\001\255\255\255\255\046\001\047\001\
\048\001\049\001\050\001\051\001\029\001\255\255\031\001\032\001\
\033\001\255\255\004\001\255\255\037\001\038\001\039\001\040\001\
\255\255\042\001\043\001\255\255\255\255\046\001\047\001\048\001\
\049\001\050\001\051\001\001\001\255\255\003\001\255\255\005\001\
\255\255\007\001\001\001\255\255\255\255\255\255\005\001\255\255\
\007\001\255\255\255\255\037\001\038\001\039\001\040\001\255\255\
\042\001\043\001\255\255\255\255\255\255\255\255\255\255\029\001\
\050\001\031\001\032\001\033\001\255\255\255\255\255\255\037\001\
\038\001\039\001\040\001\255\255\042\001\043\001\255\255\255\255\
\046\001\047\001\048\001\049\001\050\001\051\001\255\255\046\001\
\047\001\048\001\049\001\002\001\051\001\255\255\255\255\006\001\
\255\255\008\001\009\001\010\001\011\001\012\001\255\255\255\255\
\255\255\255\255\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\002\001\028\001\255\255\255\255\
\006\001\255\255\008\001\009\001\010\001\011\001\012\001\255\255\
\255\255\255\255\255\255\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\002\001\026\001\255\255\028\001\006\001\
\255\255\008\001\009\001\010\001\011\001\012\001\255\255\255\255\
\255\255\255\255\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\002\001\026\001\255\255\028\001\006\001\255\255\
\255\255\002\001\255\255\255\255\255\255\006\001\255\255\255\255\
\255\255\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\002\001\026\001\255\255\028\001\006\001\023\001\024\001\
\002\001\026\001\255\255\028\001\006\001\255\255\255\255\255\255\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\002\001\026\001\255\255\028\001\006\001\023\001\024\001\002\001\
\026\001\255\255\028\001\006\001\255\255\255\255\255\255\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\002\001\
\026\001\255\255\028\001\006\001\002\001\255\255\255\255\026\001\
\006\001\028\001\255\255\255\255\255\255\255\255\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\002\001\026\001\
\255\255\028\001\006\001\255\255\026\001\255\255\028\001\255\255\
\255\255\255\255\255\255\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\002\001\026\001\255\255\
\028\001\006\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\001\001\026\001\255\255\028\001\
\005\001\037\001\038\001\039\001\040\001\255\255\042\001\043\001\
\013\001\014\001\015\001\016\001\255\255\255\255\050\001\255\255\
\255\255\002\001\255\255\255\255\255\255\255\255\027\001\008\001\
\009\001\010\001\011\001\012\001\255\255\255\255\255\255\255\255\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\002\001\255\255\255\255\255\255\255\255\255\255\008\001\009\001\
\010\001\011\001\012\001\255\255\255\255\255\255\255\255\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\002\001\
\255\255\255\255\255\255\255\255\255\255\008\001\009\001\010\001\
\011\001\012\001\255\255\255\255\255\255\255\255\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\002\001\255\255\
\255\255\255\255\255\255\255\255\008\001\009\001\010\001\011\001\
\012\001\255\255\255\255\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\008\001\009\001\010\001\
\011\001\012\001\255\255\255\255\255\255\255\255\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\255\255\026\001\
\008\001\009\001\010\001\011\001\012\001\255\255\255\255\255\255\
\255\255\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\006\001\026\001\008\001\009\001\010\001\011\001\012\001\
\255\255\255\255\255\255\255\255\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\008\001\009\001\010\001\011\001\
\012\001\255\255\255\255\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\006\001\026\001\008\001\
\009\001\010\001\011\001\012\001\255\255\255\255\255\255\255\255\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\008\001\009\001\010\001\011\001\012\001\255\255\255\255\255\255\
\255\255\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\255\255\026\001\008\001\009\001\010\001\011\001\012\001\
\255\255\255\255\255\255\255\255\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\008\001\009\001\010\001\011\001\
\012\001\255\255\255\255\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\037\001\038\001\039\001\040\001\255\255\
\042\001\043\001\044\001\255\255\255\255\255\255\255\255\255\255\
\050\001"

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
  PLUS_ASSIGN\000\
  MINUS_ASSIGN\000\
  TIMES_ASSIGN\000\
  DIVIDE_ASSIGN\000\
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
# 45 "dparse.mly"
  ( [], [] )
# 467 "dparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'struct_decl) in
    Obj.repr(
# 46 "dparse.mly"
                        ( (_2 :: fst _1), snd _1 )
# 475 "dparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_decl) in
    Obj.repr(
# 47 "dparse.mly"
                      ( fst _1, (_2 :: snd _1) )
# 483 "dparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'struct_stmt_list) in
    Obj.repr(
# 51 "dparse.mly"
  ({ 
    stname = _2;
    members = List.rev _4;
  })
# 494 "dparse.ml"
               : 'struct_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'dtype) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 58 "dparse.mly"
  ({
    rtyp = _1;
    fname = _2;
    formals = _4;
    fstmts = List.rev _7;
  })
# 509 "dparse.ml"
               : 'func_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "dparse.mly"
                                ( [] )
# 515 "dparse.ml"
               : 'struct_stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'struct_stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'struc_stmt) in
    Obj.repr(
# 67 "dparse.mly"
                                ( _2 :: _1 )
# 523 "dparse.ml"
               : 'struct_stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'dtype) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 70 "dparse.mly"
                ( (_1, _2) )
# 531 "dparse.ml"
               : 'struc_stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "dparse.mly"
                 ( [] )
# 537 "dparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 74 "dparse.mly"
                 ( _1 )
# 544 "dparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'dtype) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 77 "dparse.mly"
                                ( [(_1, _2)] )
# 552 "dparse.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'dtype) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 78 "dparse.mly"
                                ( (_3, _4) :: _1 )
# 561 "dparse.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "dparse.mly"
     ( Id(_1) )
# 568 "dparse.ml"
               : 'vname))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "dparse.mly"
                    ( [] )
# 574 "dparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 85 "dparse.mly"
                    ( _2 :: _1 )
# 582 "dparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 88 "dparse.mly"
                                               ( Expr _1 )
# 589 "dparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 89 "dparse.mly"
                                               ( Block(List.rev _2) )
# 596 "dparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'dtype) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 90 "dparse.mly"
                                               ( VarDecl(_1, _2, Noexpr(_1)) )
# 604 "dparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'dtype) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 91 "dparse.mly"
                                               ( VarDecl(_1, _2, _4) )
# 613 "dparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'dtype) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 92 "dparse.mly"
                                               ( ArrayDecl(_1, _2, _4, Noexpr(_1)))
# 622 "dparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 93 "dparse.mly"
                                               ( If(_3, _5, Block([])) )
# 630 "dparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 94 "dparse.mly"
                                               ( If(_3, _5, _7) )
# 639 "dparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'stmt) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 95 "dparse.mly"
                                               ( For(_3, _4, _6, _8) )
# 649 "dparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 96 "dparse.mly"
                                               ( While (_3, _5) )
# 657 "dparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 97 "dparse.mly"
                                               ( Return _2 )
# 664 "dparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 100 "dparse.mly"
                                          ( _2 )
# 671 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "dparse.mly"
                                          ( Not(_2) )
# 678 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 102 "dparse.mly"
                                          ( IntLit(_1) )
# 685 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 103 "dparse.mly"
                                          ( FloatLit(_1) )
# 692 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 104 "dparse.mly"
                                          ( StringLit(_1) )
# 699 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 105 "dparse.mly"
                                          ( BoolLit(_1) )
# 706 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'array_opt) in
    Obj.repr(
# 106 "dparse.mly"
                                          ( ArrayLit(List.rev _2) )
# 713 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 107 "dparse.mly"
                                          ( Id(_1) )
# 720 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vname) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "dparse.mly"
                                          ( Assign(_1, _3) )
# 728 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "dparse.mly"
                                          ( ArrayAssign(Id(_1), _3, _6) )
# 737 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 110 "dparse.mly"
                                          (ArrayIndex(Id(_1), _3))
# 745 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 111 "dparse.mly"
                                          ( StructUse(Id(_1), Id(_3)) )
# 753 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "dparse.mly"
                                          ( StructAssign(Id(_1), Id(_3), _5) )
# 762 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 113 "dparse.mly"
                                          ( Call (_1, _3) )
# 770 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 114 "dparse.mly"
                                          ( Unop(Incr, _1) )
# 777 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 115 "dparse.mly"
                                          ( Unop(Decr, _1) )
# 784 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "dparse.mly"
                        ( OpAssign(_1, Add, _3) )
# 792 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "dparse.mly"
                         ( OpAssign(_1, Sub, _3) )
# 800 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "dparse.mly"
                         ( OpAssign(_1, Mult, _3) )
# 808 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "dparse.mly"
                          ( OpAssign(_1, Div, _3) )
# 816 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "dparse.mly"
                                          ( Binop(_1, Add, _3) )
# 824 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "dparse.mly"
                                          ( Binop(_1, Sub, _3) )
# 832 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "dparse.mly"
                                          ( Binop(_1, Mult, _3) )
# 840 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "dparse.mly"
                                          ( Binop(_1, Div, _3) )
# 848 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "dparse.mly"
                                          ( Binop(_1, Mod, _3) )
# 856 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "dparse.mly"
                                          ( Binop(_1, Eq, _3) )
# 864 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "dparse.mly"
                                          ( Binop(_1, Neq, _3) )
# 872 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "dparse.mly"
                                          ( Binop(_1, Lt, _3) )
# 880 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "dparse.mly"
                                          ( Binop(_1, Leq, _3) )
# 888 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "dparse.mly"
                                          ( Binop(_1, Gt, _3) )
# 896 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "dparse.mly"
                                          ( Binop(_1, Geq, _3) )
# 904 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "dparse.mly"
                                          ( Binop(_1, And, _3) )
# 912 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "dparse.mly"
                                          ( Binop(_1, Or, _3) )
# 920 "dparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "dparse.mly"
              ( [] )
# 926 "dparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 138 "dparse.mly"
              ( _1 )
# 933 "dparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "dparse.mly"
                         ( [_1] )
# 940 "dparse.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "dparse.mly"
                         ( _3 :: _1 )
# 948 "dparse.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 145 "dparse.mly"
                         ( [] )
# 954 "dparse.ml"
               : 'array_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 146 "dparse.mly"
                         ( [_1] )
# 961 "dparse.ml"
               : 'array_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'array_opt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 147 "dparse.mly"
                         ( _3 :: _1 )
# 969 "dparse.ml"
               : 'array_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 150 "dparse.mly"
                ( Int )
# 975 "dparse.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 151 "dparse.mly"
                ( Float )
# 981 "dparse.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 152 "dparse.mly"
                ( String )
# 987 "dparse.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 153 "dparse.mly"
                ( Bool )
# 993 "dparse.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    Obj.repr(
# 154 "dparse.mly"
                ( Void )
# 999 "dparse.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'dtype) in
    Obj.repr(
# 155 "dparse.mly"
                ( Array(_2) )
# 1006 "dparse.ml"
               : 'dtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 156 "dparse.mly"
                ( Struct(_1) )
# 1013 "dparse.ml"
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
