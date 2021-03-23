/* parser.mly - grammar for depython */
%token <int64> INT
%token <string> ID
%token LPAREN RPAREN
%token ADD1
%token SUB1
%token PLUS
%token MINUS
%token TIMES
%token LET
%token IF
%token TRUE FALSE

%start <Syntax.expr> expr
%{ open Syntax %}

%%
expr:
| i = INT
    { ENum i }
| TRUE
    { EBool true }
| FALSE
    { EBool false }
/* estos no los he arreglado
| id = ID
    { Id id }
| LPAREN ADD1 e = expr RPAREN
    { Add1 e }
| LPAREN SUB1 e = expr RPAREN
    { Sub1 e }
| LPAREN LET LPAREN s = ID e1 = expr RPAREN e2 = expr RPAREN
    { Let (s, e1, e2) }
| LPAREN IFNZ e1 = expr e2 = expr e3 = expr RPAREN
    { Ifnz (e1, e2, e3) }
| LPAREN e1 = expr PLUS e2 = expr RPAREN
    { Prim2 (Plus, e1, e2) }
| LPAREN e1 = expr MINUS e2 = expr RPAREN
                               { Prim2 (Minus, e1, e2) }
| LPAREN e1 = expr TIMES e2 = expr RPAREN
                               { Prim2 (Times, e1, e2) }
*/
