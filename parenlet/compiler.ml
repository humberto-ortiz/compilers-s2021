open Printf
open Syntax

type reg =
  | RAX
  | RSP

type arg =
  | Const of int64
  | Reg of reg
  | RegOffset of reg * int (* RegOffset(reg, i) represents address [reg + 8*i] *)

type instruction =
  | IMov of arg * arg
  | IAdd of arg * arg

let reg_to_string r =
  match r with
  | RAX -> "rax"
  |  RSP -> "rsp"

let arg_to_string a =
  match a with
  | Reg r -> reg_to_string r
  | Const n -> Int64.to_string n
  | RegOffset (r, i) -> "[" ^ reg_to_string r ^ " + 8*" ^ string_of_int i ^ "]"

let inst_to_string inst =
  match inst with
  | IMov (a1, a2) -> "mov " ^ arg_to_string a1 ^ ", " ^ arg_to_string a2
  | IAdd (a1, a2) -> "add " ^ arg_to_string a1 ^ ", " ^ arg_to_string a2

let rec asm_to_string (asm : instruction list) : string =
  match asm with
  | [] -> ""
  | inst::instrs -> inst_to_string inst ^ "\n" ^ asm_to_string instrs

(* A very sophisticated compiler - insert the given integer into the mov
   instruction at the correct place *)
type env = (string * int) list

let rec lookup name env =
  match env with
  | [] -> failwith (sprintf "Identifier %s not found in environment" name)
  | (n, i)::rest ->
    if n = name then i else (lookup name rest)

let add name env : (env * int) =
  let slot = 1 + (List.length env) in
  ((name, slot)::env, slot)

let rec compile_expr (e : expr) (env : env) : instruction list =
  match e with
  | Num n -> [ IMov(Reg(RAX), Const(n)) ]
  | Add1 otra_expr -> compile_expr otra_expr env @ 
                      [ IAdd (Reg RAX, Const 1L) ] 
  | Sub1 otra_expr -> compile_expr otra_expr env @
                      [ IAdd (Reg RAX, Const (-1L)) ]
  | Id v ->
    let slot = lookup v env in 
    [ IMov (Reg RAX, RegOffset (RSP, ~-1 * slot))]
  | Let (id, e1, e2) ->
    let (env', slot) = add id env in
    compile_expr e1 env @
    [ IMov (RegOffset (RSP, ~-1 * slot), Reg RAX)] @
    compile_expr e2 env'
;;

let compile_prog (e : expr) : string =
  let instrs = compile_expr e [] in
  let instr_string = asm_to_string instrs in

"
section .text
global our_code_starts_here
our_code_starts_here:
" ^ instr_string ^ "
  ret
";;

(* Some OCaml boilerplate for reading files and command-line arguments *)

let ast_test infile =
   let lexbuf = Lexing.from_channel infile in
   let loop () =
     let prog  = Parser.expr Lexer.token lexbuf in
     printf "%s\n" (compile_prog prog)
   in
   loop ()


let () =
  let input_file = (open_in (Sys.argv.(1))) in
  ast_test input_file
