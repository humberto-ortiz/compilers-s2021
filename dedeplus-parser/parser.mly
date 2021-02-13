/* parser.mly - grammar for depython */
%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token PRINT
%token COMMA
%token EOL                      /* end of line */
%token EOF                      /* end of file */

%left PLUS MINUS                /* lowest precedence */
%left TIMES DIV                 /* medium precedence */
%nonassoc UMINUS                /* highest precedence */

%start <Syntax.prog> main
%{ open Syntax %}

%%

main:
| ss = stms EOF
    { Module ss }
;

stms:
| 
    { [] }
| s = stm
    { [s] } (* single statement, no newline *)
| s = stm EOL ss = stms
    { s :: ss }
;

stm:
| e = expr
    { Expr e }
| PRINT es = exprs { Print es }
;

exprs:
|
    { [] }
| e = expr
    { [e] }
| e = expr COMMA es = exprs
    { e :: es }
;

expr:
| i = INT
    { Num i }
| LPAREN e = expr RPAREN
    { e }
| e1 = expr PLUS e2 = expr
    { BinOp (e1, Add, e2) }
| e1 = expr MINUS e2 = expr
    { BinOp (e1, Sub, e2) }
| e1 = expr TIMES e2 = expr
    { BinOp (e1, Mult, e2) }
| e1 = expr DIV e2 = expr
    { BinOp (e1, Div, e2) }
| MINUS e = expr %prec UMINUS
    { BinOp (e, Mult, (Num (-1))) }
;
