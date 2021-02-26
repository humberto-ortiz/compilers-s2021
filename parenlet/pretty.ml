open Syntax

let rec pretty_exp expr =
  match expr with
  | Num n -> "Num " ^ string_of_int n
  | Id v -> "Id " ^ v
  | Let (id, e1, e2) -> "Let " ^ id ^ pretty_exp e1 ^ pretty_exp e2
  | Add1 e -> "Add1 " ^ pretty_exp e
  | Sub1 e -> "Sub1 " ^ pretty_exp e
