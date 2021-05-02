(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
}
rule token = parse
  | [' ' '\t' '\n']   { token lexbuf }     (* skip blanks *)
  | ['0'-'9']+ as lxm { INT(Int64.of_string lxm) }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | "add1"            { ADD1 }
  | "sub1"            { SUB1 }
  | "let"             { LET }
  | "if"              { IF }
  | "true"            { TRUE }
  | "false"           { FALSE }
  | "+"            { PLUS }
  | "-"            { MINUS }
  | "*"            { TIMES }
  | "&&"           { AND } 
  | "||"           { OR }
  | "<"            { LESS } 
  | ">"            { GREATER } 
  | "<="           { LESSEQ }
  | ">="           { GREATEREQ } 
  | "=="           { EQ }
  | "!="           { NE } 
  | ['a'-'z' 'A'-'Z' '_' '0'-'9']+ as id { ID id}
