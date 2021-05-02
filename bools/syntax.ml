(* syntax.ml - Abstract syntax for parenlet *)

type prim1 =
  | Not
  | Add1
  | Sub1
  | Print

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
  | ECall of string * (expr list)

type decl =
  (* function name, argument names, body *)
  | DFun of string * string list * expr 

type program =
  | Program of decl list * expr
