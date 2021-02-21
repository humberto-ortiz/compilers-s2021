# Interpreter for lecture 3 abstract syntax

In lecture 3, a small language with `let` is defined. This is an interpreter for
that language, as discussed in class on Thursday.

The language allows numbers, add1 and sub1 of expressions, identifiers, and let
expressions of the form: `let <id> = <expr> in <expr>`.

I've defined the syntax and the interpreter in the file [interp.ml](interp.ml),
you can run it and test some expressions:
```
utop # #use "interp.ml";;
type expr =
    Num of int64
  | Add1 of expr
  | Sub1 of expr
  | Id of string
  | Let of string * expr * expr
type env = (string * expr) list
val lookup : 'a -> ('a * 'b) list -> 'b = <fun>
val interp : expr -> int64 = <fun>
utop # let e4 = (Let ("x", Num 3L, Add1 (Id "x")));;
val e4 : expr = Let ("x", Num 3L, Add1 (Id "x"))
utop # interp e4;;
- : int64 = 4L
```
