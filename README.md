# dedepython - a subset of python

I've defined a subset of the python Abstract Syntax Tree (AST) in `dedepython.ml`.

In the `test.ml` file I defined some unit tests. You can run the tests using the
dune build system from the directory with these files:

```
dune runtests
```

If you don't have dune and ounit2, install them with opam:

```
opam install dune ounit2
```

Your homework is to implement a working interpreter for this ast. All my tests
should pass. You may want to write more tests to make sure your implementation
is correct. I'll grade it with a larger test suite.

You must not change the type of the `interp` function, or my test suite will fail
to compile, and you'll get a failing grade. You may (and probably should) write
additional helper functions to help you interpret dedepython programs.

OUnit2 prints a '.' if a test passed, and 'F' if it fails.
