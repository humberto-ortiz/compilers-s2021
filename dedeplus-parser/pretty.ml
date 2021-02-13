open Syntax

let pretty_binop op =
  match op with
  | Add -> ", Add, "
  | Sub -> ", Sub, "
  | Mult -> ", Mult, "
  | Div -> ", Div, " 

let rec pretty_exp expr =
  match expr with
  | Num n -> string_of_int n
  | BinOp (e1, op, e2) ->
     "BinOp (" ^
     pretty_exp e1 ^
     pretty_binop op ^
     pretty_exp e2 ^
     ")"

let rec pretty_list_exp lst =
  match lst with
  | [] -> ""
  | [exp] -> pretty_exp exp
  | exp :: exprs -> pretty_exp exp ^ ", " ^ pretty_list_exp exprs

let pretty_stm stm =
  match stm with
  | Expr exp -> "Expr " ^ pretty_exp exp ^ "\n"
  | Print exps -> "Print " ^ String.concat ", " (List.map pretty_exp exps) ^ "\n"

let pretty_prog prog =
  let text = match prog with
    | Module stms ->
      "Module [\n" ^
      String.concat "" (List.map pretty_stm stms) ^
      "]\n"
  in
  text
