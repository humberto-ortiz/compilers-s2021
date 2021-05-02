/* parser.mly - grammar for depython */
%token <int64> INT
%token <string> ID
%token LPAREN RPAREN
%token ADD1
%token SUB1
%token PLUS
%token MINUS
%token TIMES
%token AND 
%token OR 
%token LESS 
%token GREATER 
%token LESSEQ
%token GREATEREQ 
%token EQ  
%token NE
%token LET
%token IF
%token TRUE FALSE
%token PRINT

%start <Syntax.expr> expr
%{ open Syntax %}

%%
prim2:
| PLUS { Plus }
| MINUS { Minus }
| TIMES { Times }
| AND { And } 
| OR { Or } 
| LESS { Less } 
| GREATER { Greater } 
| LESSEQ { LessEq } 
| GREATEREQ { GreaterEq } 
| EQ { Eq } 
| NE { Ne }


expr:
| i = INT
  { ENum i }
| TRUE
  { EBool true }
| FALSE
  { EBool false }
| LPAREN ADD1 e = expr RPAREN
  { EPrim1 (Add1, e) }
| id = ID
  { EId id }
| LPAREN LET LPAREN s = ID e1 = expr RPAREN e2 = expr RPAREN
  { ELet (s, e1, e2) }
| LPAREN PRINT e = expr RPAREN
  { EPrim1 (Print, e) }
| LPAREN e1 = expr op = prim2 e2 = expr RPAREN
	{ EPrim2 (op, e1, e2) }
/* estos no los he arreglado
 LPAREN IFNZ e1 = expr e2 = expr e3 = expr RPAREN
    { Ifnz (e1, e2, e3) }
(*| LPAREN e1 = expr PLUS e2 = expr RPAREN
    { Prim2 (Plus, e1, e2) } Ya arreglado arriba.*)
| LPAREN e1 = expr MINUS e2 = expr RPAREN
                               { Prim2 (Minus, e1, e2) }
| LPAREN e1 = expr TIMES e2 = expr RPAREN
                               { Prim2 (Times, e1, e2) }
*/
