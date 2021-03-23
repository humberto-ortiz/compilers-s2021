(* syntax.ml - Abstract syntax for parenlet *)

type prim1 =
  | Not
  | Add1
  | Sub1

type prim2 =
  | Plus
  | Minus
  | Times
  | And | Or | Less | Greater | LessEq | GreaterEq | Eq | Ne

type expr =
  | ENum of int64
  | EPrim1 of prim1 * expr
  | EId of string
  | ELet of string * expr * expr
  | EIfnz of expr * expr * expr
  | EPrim2 of prim2 * expr * expr
  | EBool of bool
