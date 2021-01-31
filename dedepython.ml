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

let p1 = Module [Expr (Num 123)]

let p2 = Module [Expr (BinOp (Num 3, Add, Num 2))]

let p3 = Module [Expr (Num 1); Expr (Num 2)]

let p4 = Module [Expr (BinOp (BinOp (Num 1, Add, Num 2), Add, Num 3))]

let interp (p : prog) : int =
  (* You need to change this function to actually implement an interpreter *)
  (* You might want to define some helper functions *)
  match p with
  | _ -> 0                      (* return 0 for everything *)

