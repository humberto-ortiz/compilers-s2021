# dedepython parser

A sample parser for a slightly augmented dedepython (I added a print
statement). It is built from several interconnected parts.

`dune` - build instructions  
`syntax.ml` - defines the abstract syntax  
`lexer.mll` - builds tokens  
`parser.mly` - defines tokens and builds syntax tree  
`pretty.ml` - converts syntax to string, for debugging  
`main.ml` - driver for parser, read a file, parse it, print it  
`test/` - folder with dedepython+ test programs  

You can run a test using `dune exec`:

```
$ dune exec ./main.exe test/112.dp 
Module [
Expr Num 112
]
```

If you had an interpreter, you could plug it into `main.ml` in place of the
pretty printer.

# dedepython plus syntax

A grammar for dedepython+. A program is a list of statments separated by
newlines. A statement can be the keyword print followed by a comma separated
list of expressions or an expression. An expression is a NUMBER or an expression
in parentheses or two expressions separated by a binary operator. A NUMBER is a
sequence of decimal digits. The binary operators are + - * /.

```
prog: stms

stms:
  stm
  stm EOL stms
 
stm:
  print exprs
  exp
  
exprs:
  exp
  exp , exprs

exp:
  NUMBER
  ( exp )
  exp binop exp

binop: + | - | * | /
```

# Sample dedepython+ programs

There are several examples in the test/ directory in this repository. I am including some below.

```
112
```

```
1 + 2 + 3
```

```
print 1, 2, 3
```

```
1
2
3
```
