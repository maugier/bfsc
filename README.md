
BFSC v0.1
=========

BFSC, the BrainFck Simple Compiler.


Fixed string generation
-----------------------

BFSC.Compile is an attempt at slightly-better-than-naive string generation.
The generated code will only use memory right of the ptr, and leave garbage behind. The standalone executable `bfputs` will assume an empty memory at the start,
and so generate standalone programs, but a different starting state can be passed in the code.

Internally, the module provides a compilation monad that simulates the program state and allows nondeterministic choice when generating code. The path with the best code size will be retained in the end. Naive
optimizations (removal of adjunct opposite operands) will be performed. 

The main string generation technique (vectorPrint) consists of first generating
a good vector of starting values matching popular ascii ranges, using parallel
multiplication; then for each character of the string, find the shortest way to change one of these values to match the desired output character. This includes making use of wrapping around 256 if possible. The new state is retained as the starting point for the next character.

    Prelude> :l BFSC.Compile
    Prelude> let prog = compile $ vectorPrint "Hello World !"
    Prelude> prog
    "+++[>++++++<-]>[>++>++++>+++++>++++++<<<<-]>>.>>-------.+++++++..+++.<<<----.>>---.>.+++.------.--------.<<<.+."
    Prelude> runBF prog
    (0|0(33)72|87|100,"Hello World !")


Symbolic Execution
------------------

BFSC.SymbolicExec is an attempt at symbolic execution, and very incomplete
at the moment.

Input is not yet supported, but the machine state can be manually initialized
with symbolic values. 

For now, only flat, <>-balanced loops are supported. Simple additions will be
recognized.

The next version will recognize linearizable nested loops, and simple multiplications. The version after that will support branching and simple equality or non-equality constraints on variables. Somewhere in between, support for byte input should be added, with input bytes as unknown variables.
