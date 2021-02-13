(* test.ml - unit test functions for your dedepython homework *)
(* Copyright 2021 - Humberto Ortiz-Zuazaga <humberto.ortiz@upr.edu> *)
(* See LICENSE file for terms and conditions *)

(* unit testing framework *)
open OUnit2

open Dedepython

(** [make_i n i p] makes an OUnit test named [n] that expects
    program [p] to evalute to [i]. *)
let make_i n i p =
  n >:: (fun _ -> assert_equal i (interp p))
;;


(* Test Cases *)
let p0 = Module [Expr (Num 22)];;
let p1 = Module [Expr (Num 123)];;
let p2 = Module [Expr (BinOp (Num 3, Add, Num 2))];;
let p3 = Module [Expr (BinOp (Num 3, Sub, Num 2))];;
let p4 = Module [Expr (BinOp (Num 3, Mult, Num 2))];;
let p5 = Module [Expr (BinOp (Num 4, Div, Num 2))];;
let p6 = Module [Expr (Num 1); Expr (Num 2)];;
let p7 = Module [Expr (BinOp (BinOp (Num 1, Add, Num 2), Add, Num 3))];;
let p8 = Module [];;
let p9 = Module [Expr (Num 1) ; Expr (Num 0)] (* should be 0 *)
let p10 = Module [Expr (BinOp (BinOp (Num 1, Add, Num 2), Add, Num 3)); Expr (BinOp (BinOp (Num 1, Add, BinOp (Num 3, Mult, Num 2)), Mult, BinOp (Num 1, Add, BinOp (Num 1, Add, BinOp (Num 1, Add, Num 2)))))]

let tests = [
  make_i "int" 22 p0;
  make_i "int" 123 p1;
  make_i "add" 5 p2;
  make_i "sub" 1 p3;
  make_i "mult" 6 p4;
  make_i "div" 2 p5;
  make_i "multiple_nums" 2 p6;
  make_i "nested_binops" 6 p7;
  make_i "null" 0 p8;
  make_i "1_0" 0 p9;
  make_i "toodeep" 35 p10;
];;

let _ = run_test_tt_main ("suite" >::: tests);;
