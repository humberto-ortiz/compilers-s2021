(* compiler.ml - an almost working compiler for numbers and Bool constants
 Copyright 2021 - Humberto Ortiz Zuazaga
   Copyright 2021 Humberto Ortiz-Zuazaga

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
open Printf
open Syntax

type reg =
  | RAX
  | RSP
  | RDI
  | R11

type arg =
  | Const of int64
  | Reg of reg
  | RegOffset of reg * int (* RegOffset(reg, i) represents address [reg + 8*i] *)

type instruction =
  | IMov of arg * arg
  | IAdd of arg * arg
  | ISub of arg * arg
  | IMul of arg * arg
  | ICmp of arg * arg
  | ITest of arg * arg
  | IJne of string
  | IJnz of string
  | IJmp of string
  | ILabel of string
  | ICall of string

let reg_to_string r =
  match r with
  | RAX -> "rax"
  | RSP -> "rsp"
  | RDI -> "rdi"
  | R11 -> "r11"

let arg_to_string a =
  match a with
  | Reg r -> reg_to_string r
  | Const n -> Int64.to_string n
  | RegOffset (r, i) -> "[" ^ reg_to_string r ^ " + 8*" ^ string_of_int i ^ "]"

let inst_to_string inst =
  match inst with
  | IMov (a1, a2) -> "mov " ^ arg_to_string a1 ^ ", " ^ arg_to_string a2
  | IAdd (a1, a2) -> "add " ^ arg_to_string a1 ^ ", " ^ arg_to_string a2
  | ISub (a1, a2) -> "sub " ^ arg_to_string a1 ^ ", " ^ arg_to_string a2
  | IMul (a1, a2) -> "imul " ^ arg_to_string a1 ^ ", " ^ arg_to_string a2
  | ICmp (a1, a2) -> "cmp " ^ arg_to_string a1 ^ ", " ^ arg_to_string a2
  | ITest (a1, a2) -> "test " ^ arg_to_string a1 ^ ", " ^ arg_to_string a2
  | IJne target -> "jne " ^ target
  | IJnz target -> "jnz " ^ target
  | IJmp target -> "jmp " ^ target
  | ILabel label -> label ^ ":"
  | ICall label -> "call " ^ label

let rec asm_to_string (asm : instruction list) : string =
  match asm with
  | [] -> ""
  | inst::instrs -> inst_to_string inst ^ "\n  " ^ asm_to_string instrs

(* A less unsophisticated compiler - this one actually has to do stuff *)
type env = (string * int) list

let rec lookup name env =
  match env with
  | [] -> failwith (sprintf "Identifier %s not found in environment" name)
  | (n, i)::rest ->
    if n = name then i else (lookup name rest)

let add name env : (env * int) =
  let slot = 1 + (List.length env) in
  ((name, slot)::env, slot)

(* TODO: fix is_imm *)
let is_imm e =
  match e with
  | ENum _ -> true
  | EId _ -> true
  | _ -> false

let rec is_anf e =
  match e with
  | EPrim1 (_,  e) -> is_imm e
  | ELet (_, e1, e2) -> is_anf e1 && is_anf e2
  | _ -> is_imm e

let gensym =
  let counter = ref 0 in
  (fun basename -> 
     counter := !counter + 1;
     sprintf "%s_%d" basename !counter)

let rec anf e =
  match e with
  | ENum _ -> e
  | EBool _ -> e
  | EId _ -> e
  | EPrim1 (op, e1) ->
    let temp = gensym "e1" in
    ELet (temp, anf e1, EPrim1 (op, EId temp))
  | ELet (v, e1, e2) -> ELet (v, anf e1, anf e2)

let min_cobra_int = Int64.div Int64.min_int 2L
let max_cobra_int = Int64.div Int64.max_int 2L

let const_true = Int64.max_int
let const_false = -1L

let rec compile_expr (e : expr) (env : env) : instruction list =
  match e with
  | ENum n ->
    if n > max_cobra_int || n < min_cobra_int then
      failwith ("Integer overflow: " ^ (Int64.to_string n))
    else
      [ IMov(Reg(RAX), Const(Int64.shift_left n 1))]
  | EBool true -> [IMov (Reg RAX, Const (const_true))]
  | EBool false -> [IMov (Reg RAX, Const (const_false))]
  | EPrim1 (Add1, e1) ->
    compile_expr e1 env @
    [ ITest (Reg RAX, Const 1L) ;
      IJnz "error_not_number" ;
      IAdd (Reg RAX, Const 1L) ] 
  | EPrim1 (Print, e) ->
    compile_expr e env @
    [ IMov (Reg RDI, Reg RAX);
      ICall "print" ]
  | EId v ->
    let slot = lookup v env in 
    [ IMov (Reg RAX, RegOffset (RSP, ~-1 * slot))]
  | ELet (id, e1, e2) ->
    let (env', slot) = add id env in
    compile_expr e1 env @
    [ IMov (RegOffset (RSP, ~-1 * slot), Reg RAX)] @
    compile_expr e2 env'
  (* voy a picharle, tienen que arregarlo ustedes *)
  | _ -> failwith "Don't know how to compile that yet!"
(* TODO: implementar los demas casos *) 
(*
(* TODO: Add1 y Sub1 ahora son Prim1 *)
  | Id v ->
    let slot = lookup v env in 
    [ IMov (Reg RAX, RegOffset (RSP, ~-1 * slot))]
  | Let (id, e1, e2) ->
    let (env', slot) = add id env in
    compile_expr e1 env @
    [ IMov (RegOffset (RSP, ~-1 * slot), Reg RAX)] @
    compile_expr e2 env'
                 *)
;;

let compile_prog (e : expr) : string =
  let instrs = compile_expr e [] in
  let instr_string = asm_to_string instrs in

"
section .text

extern error
extern print

error_not_number:
  mov RSI, RAX      ;; Arg 2: the badly behaved value
  mov RDI, 1        ;; Arg 1: a constant describing which error-code occurred
  call error        ;; our error handler

global our_code_starts_here
our_code_starts_here:
  push RBP          ;; save (previous, caller's) RBP on stack
  mov RBP, RSP      ;; make current RSP the new RBP

  " ^ instr_string ^ "
  mov RSP, RBP      ;; restore value of RSP to that just before call
                    ;; now, value at [RSP] is caller's (saved) RBP
  pop RBP           ;; so: restore caller's RBP from stack [RSP]
  ret               ;; return to caller
";;

(* Some OCaml boilerplate for reading files and command-line arguments *)
let () =
  let infile = (open_in (Sys.argv.(1))) in
  let lexbuf = Lexing.from_channel infile in
  let input_ast =  Parser.expr Lexer.token lexbuf in
  let anfed = anf input_ast in
  let program = (compile_prog anfed) in
  printf "%s\n" program;;
