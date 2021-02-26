(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
}
rule token = parse
  [' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
| ['0'-'9']+ as lxm { NUMBER( Int64.of_string lxm) }
| '('            { LPAREN }
| ')'            { RPAREN }
| "sub1"        { SUB1 }
| "add1"        { ADD1 }
| "let"         { LET }
| ['a'-'z' 'A'-'Z']+ as lxm { ID lxm }
| eof            { EOF }
