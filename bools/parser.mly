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
| LPAREN ADD1 e = expr RPAREN
  { EPrim1 (Add1, e) }
| LPAREN SUB1 e = expr RPAREN
  { EPrim1 (Sub1, e) }
| id = ID
  { EId id }
| LPAREN LET LPAREN s = ID e1 = expr RPAREN e2 = expr RPAREN
  { ELet (s, e1, e2) }

/* estos no los he arreglado
 LPAREN IFNZ e1 = expr e2 = expr e3 = expr RPAREN
    { Ifnz (e1, e2, e3) }
| LPAREN e1 = expr PLUS e2 = expr RPAREN
    { Prim2 (Plus, e1, e2) }
| LPAREN e1 = expr MINUS e2 = expr RPAREN
                               { Prim2 (Minus, e1, e2) }
| LPAREN e1 = expr TIMES e2 = expr RPAREN
                               { Prim2 (Times, e1, e2) }
*/
