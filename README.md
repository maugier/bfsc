
BFSC v0.1
=========

BFSC, the BrainFck Simple Compiler.


## Fixed string generation

BFSC.Compile is an attempt at slightly-better-than-naive string generation.
The generated code will only use memory right of the ptr, and leave garbage behind. The standalone executable `bfputs` will assume an empty memory at the start,
and so generate standalone programs, but a different starting state can be passed in the code.

Internally, the module provides a compilation monad that simulates the program state and allows nondeterministic choice when generating code. The path with the best code size will be retained in the end. Naive
optimizations (removal of adjunct opposite operands) will be performed. 

The main string generation technique (vectorPrint) consists of first generating
a good vector of starting values matching popular ascii ranges, using parallel
multiplication; then for each character of the string, find the shortest way to change one of these values to match the desired output character. This includes making use of wrapping around 256 if possible. The new state is retained as the starting point for the next character.

    Prelude> :l BFSC.Compile
    Prelude BFSC.Compile> let prog = compile $ vectorPrint "Hello World !"
    Prelude BFSC.Compile> prog
    "+++[>++++++<-]>[>++>++++>+++++>++++++<<<<-]>>.>>-------.+++++++..+++.<<<----.>>---.>.+++.------.--------.<<<.+."
    Prelude BFSC.Compile> runBF prog
    (0|0(33)72|87|100,"Hello World !")

Or with the CLI tool:

    $ ./bfputs 'Hello World !!!'
    +++[>++++++<-]>[>++>++++>+++++>++++++<<<<-]>>.>>-------.+++++++..+++.<<<----.>>---.>.+++.------.-----
    ---.<<<.+...

## Symbolic Execution

BFSC.SymbolicExec is an attempt at symbolic execution, and very incomplete
at the moment.

Some _linearizable_ loops are recognized and handled as such; these are loops with no nested
subloops, a balanced number of `<` and `>` instructions, and that only increment or decrement the tested cell by one unit. These loop are fully translated to algebraic operations on memory, are 
only executed once and do not cause branching.

Other loops cause branching; the program keeps track of constraints on symbolic expressions when branching.

Simple constraints (for now, only constraints involving linear equations with a single solution over a single variable) are automatically translated into variable bindings, and substituted within
the symbolic state.


### Example run

The following program asks for one byte of input, then outputs "yyyy" only if the input byte
was 'y', and outputs nothing otherwise.

121 is the ascii code for 'y'. Arithmetic is done modulo 2^8, so 135+in0 is equivalent to in0-121.

    ./bfsymrun <<< ",>>+++++[<++++++>-]<[<<++++>---->-]+<-[>-<[-]]>[<<+....>]"
    -----
    Out: 'y', 'y', 'y', 'y'
    Known variables: "in0" -> 121
    Constraints: 
    Final State: 121(0)1|0
    -----
    Out: 
    Known variables: 
    Constraints: 135+in0 != 0
    Final State: 120|0(0)0


