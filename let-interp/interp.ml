(* interp.ml - Interpreter for lecture 3 abstract syntax *)
(* Copyright 2021 - Humberto Ortiz Zuazaga
   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
   ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
   LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
   SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
   POSSIBILITY OF SUCH DAMAGE.
*)

type expr = 
  | Num of int64
  | Add1 of expr
  | Sub1 of expr
  | Id of string
  | Let of string * expr * expr

type env = (string * expr) list

let rec lookup id env =
  match env with
  | [] -> raise (Failure "no such variable")
  | (var, exp)::rest -> if var = id then exp else lookup id rest

let interp (e : expr) : int64 =
  let rec interp_in_env (e : expr) (env : env) =
    match e with
    | Num n -> n
    | Add1 e1 -> Int64.add 1L  (interp_in_env e1 env)
    | Sub1 e1 -> Int64.sub (interp_in_env e1 env) 1L
    | Id id -> interp_in_env (lookup id env) env
    | Let (id, e1, e2) ->
      let env' = (id, e1) :: env in
      interp_in_env e2 env'
  in
     interp_in_env e []
