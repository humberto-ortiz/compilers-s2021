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

let tests = [
  make_i "int" 22 (Module [Expr (Num 22)]);
  make_i "add" 5 p2
]

let _ = run_test_tt_main ("suite" >::: tests)
