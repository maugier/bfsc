
BFSC v0.1
=========

BFSC, the BrainFck Simple Compiler.


String generation
-----------------

Generates decent BF code to generate fixed output. Pass the string as an argument.

The code requires that memory right of the ptr is empty, and will leave garbage behind.

BFSC provides a compilation monad that simulates the program state and allows nondeterministic
choice when generating code. The path with the best code size will be retained in the end. Naive
optimizations (removal of adjunct opposite operands) will be performed


Symbolic Execution
------------------

Work in progress.
