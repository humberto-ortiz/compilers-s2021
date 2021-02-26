/* parser.mly - grammar for depython */
%token <int> NUMBER
%token <string> ID
%token LPAREN RPAREN
%token EOF                      /* end of file */
%token ADD1 SUB1
%token LET

%start <Syntax.expr> expr
%{ open Syntax %}

%%

expr:
| i = NUMBER
    { Num i }
| v = ID 
    { Id v }
| LPAREN ADD1 e = expr RPAREN
    { Add1 e }
| LPAREN SUB1 e = expr RPAREN
    { Sub1 e }
| LPAREN LET LPAREN s = ID e1 = expr RPAREN e2 = expr RPAREN
    { Let (s, e1, e2) }
;
