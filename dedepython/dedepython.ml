(* dedepython.ml - a simplified python AST interpreter *)
(* Copyright 2021 - Humberto Ortiz-Zuazaga <humberto.ortiz@upr.edu> *)
(* See LICENSE for conditions *)

type binop = Add | Sub | Mult | Div

type exp =
    | Num of int
    | BinOp of exp * binop * exp

type stm =
    | Expr of exp

type prog =
    | Module of stm list

let interp (p : prog) : int =
  (* You need to change this function to actually implement an interpreter *)
  (* You might want to define some helper functions *)
  match p with
  | _ -> 0                      (* return 0 for everything *)

