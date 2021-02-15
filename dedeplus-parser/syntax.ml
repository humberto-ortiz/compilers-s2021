(* syntax.ml - Abstract syntax for dedepython *)

type binop =
  | Add | Sub | Mult | Div

type exp =
  | Num of int
  | BinOp of exp * binop * exp

type stm =
    | Expr of exp
    | Print of exp list

type prog =
    | Module of stm list

