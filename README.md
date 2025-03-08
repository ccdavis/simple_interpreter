The project demonstrates a technique for compiling a programming language to a flattened structure and interpreting it mostly left to right, rather than recursively top-down. The compiler adds to the target byte code in a natural recursive descent style.

This approach ought to execute many times faster than classic tree-walking without significantly complicating the interpreter or target format.

It also has the benefit of being relatively  easy to translate to other intermediate representations and even assembly languages. 

One disadvantage is that it will be harder to apply suphisticated optimizations since there's no AST, or at least one has to create one after the fact to analyze the program, then re-emit the flattened code if you wanted to optimize on a tree.

## Goals

The goal for now is to make a language and interpreter sufficient to write and test out interesting benchmark programs and try to maximize performance of the interpreter.

### Details and status

Unlike a classic byte code virtual machine, the interpreter takes expressions of the language directly and treats them as bytecode at a somewhat higher level than a typical vm would. Nevertheless one could consider these expressions as a byte code.  See the `expression` module.

The vm loops through a vector of expressions in order left to right, rather than navigating them semantically top-down as a tree. That ought to improve cache locality for better performance. If we show that interpretation strategy produces correct program execution, we could confidently make a simple translator from the expressions to WASM or even assembly. The expressions contain all the necessary information to generate labels for flow-control.

I put in some effort to ensure all varients  of the `Expr` type require no more than8 bytes. I could probably get that lower with better use of side tables for types and references to lists.

I needed to compromise  somewhat on the expression type to communicate some lower level behavior to the vm, and in the compiler I had to do a few tricks to change up parsing order to 
 acomadate the way the vm operates. Mostly this looks similar to how recursive descent parsers generate labels for variable declarations and expression results, but in this case we use an ever incrementing address instead of labels for an assembler. 

The language currently only has the bare minimum features to write interesting benchmark programs. (Well, I included several data types and an 'output' statement too.) No functions yet, just variables, "for" loops, "if" statements and logical operators and comparison operators and arithmetic.

I've tried to write a naive Mandelbrot set renderer as a first real program to benchmark.No results yet.

The syntax has some warts I know how to correct, and will at some point.

```
let x := 8;
let y := 999;
let result := false;
if (x > y) {
    result := true
} else {
    result := false;
};

let factors :=0;
for (x < y>) {    
    if (x / 9 = 0.0 and x / 2 = 0.0) {
        output "Factor of 9 and two!";
        output x;
        factors := factors + 1
    };
    x := x + 1
};
output "Total factors of 9 and two:";
output factors
```

The above uses all the currently available language features. (Yes, this is not an interesting benchmark.) 

As in Pascal, the ';' separates statements rather than terminate them. In addition, taking a page from Rust, statements really function like expressions internally and always have a value. The last statement in a list needs no terminating semicolon; if you add one it functions like Rust: Instead of returning the value of the last statement-expression it adds a "unit" expression after the semicolon and returns the Unit type and value. I haven't exposed that behavior to the language yet but plan to have it operate for function return values and maybe conditionals.

The '(',')' symbols are redundant in the "if" and "for" statements. The ':=' assignment statement is also used on variable declaration which I don't love, though maybe it's fine. 

at some point I'd like to add mutable and immutable variables in case that helps with performance tuning when I add functions. Also I'd like some basic structured data type like arrays at least to make more algorithms easier to express.
