The project demonstrates a technique for compiling a programming language to a flattened structure and interpreting it mostly left to right, rather than recursively top-down. The compiler adds to the target byte code in a natural recursive descent style.

This approach ought to execute several times faster than classic tree-walking without significantly complicating the interpreter or target format.

It also has the benefit of being relatively  easy to translate to other intermediate representations and even assembly languages. 

One disadvantage is that it will be harder to apply suphisticated optimizations since there's no AST, or at least one has to create one after the fact to analyze the program, then re-emit the flattened code if you wanted to optimize on a tree.

## Goals

The goal for now is to make a language and interpreter sufficient to write and test out interesting benchmark programs and try to maximize performance of the interpreter.

## Non Goals

* An interesting programming language
* Fully featured language  with data structures or modules


### Details and status

### Interpreter

Unlike a classic byte code virtual machine, this interpreter takes expressions of the language directly (the `Expr` type in the code.) and treats them as bytecode at a somewhat higher level than a typical vm would. Nevertheless one could consider these expressions as a byte code.  See the `expression` module.

The vm loops through a vector of expressions in order left to right, rather than navigating them semantically top-down as a tree. That ought to improve cache locality for better performance. If we show this interpretation strategy produces correct program execution, we could confidently make a simple translator from the expressions to WASM or even assembly. The expressions contain all the necessary information to generate labels for flow-control.

I put in some effort to ensure all varients  of the `Expr` type require no more than8 bytes. I could probably get that lower with better use of side tables for types and references to lists.

###  Design improvement ideas

#### Speed up type-generic operations 

Without fully building a machine specific JIT, I could take advantage of the fact that my compiler produces fully typed expressions. In the current design, operations on values are the smallest instruction and the type-specific operations are determined at runtime every time. There's just one `Add(lhs,rhs)` "instruction" expression. The interpreter chooses which block of code to do the addition depending on the types of the operands using a Rust "match" expression at runtime. This could be "just-in-time" or "ahead-of-time" (JIT or AOT.)

##### JIT 

Instead of runtime dispatch to the right type of instruction, a JIT could realize that the particular expression will always use the same types  and skip the match, generating or plucking out a ready made function. In Rust you could do this with closures. That approach might hurt the cache efficiency so profiling would be called for.  

This approach has the advantage that it can  adapt to statically typed or dynamically typed languages up to a point. As the interpreter runs it can usually know that every time it returns to an instruction the operands will be the same type as the first time (but not in all scenarios.) 

##### AOT

Since the language in this project is statically typed, an ahead-of-time pass could plug in the right type-specific closures or swap out one expression for a more specific one.

One could make a second tier of the `Expr` type that the compiler need not emit but that can be  deduced  as expressions get added to the target. A generic `Add` expression would be replaced by the type-specific expression varient. `Add(Expr, Expr)` becomes `AddFloats(f64,f64)` for instance.   For  flexibility the VM could support both type generic and type specific versions of operations.

Depending on the compiler design this "specialization" could be done immediately by the compiler, but it might be that the type specific expression can't be known on the spot and a pass over the target output is needed before starting the  VM.

#### Fully replace expressions / instructions with function pointers / closures

In this model of interpretation   the VM becomes radically simplified: The compiler spits out functions for operations -- there's no "instruction set" as such, and the VM state becomes simpler to manage.  The "instructions" the compiler outputs consists of an array of function pointers. In the traditional VM (and my current design) the VM is mostly a big "switch" or "match" for every type of operation but in this new model that evaporates.

Each function the compiler emits will return  an offset from its current instruction pointer, which can include negative offsets for looping behavior. You still have a basic stack for remembering scopes. 

The functions take the VM as an argument and their other arguments will carry the specific data needed for their work.   Normally the function will return 1, which will increment the VM's instruction pointer by 1, and it calls the function at that index in the instructions and the interpretation continues.

How you implement this technique highly depends on the  language being used -- it may be nearly impossible in plain C. In Rust I think you'd use boxed closures. 

I've seen this before, or some variation of it anyhow. Here <a href="https://planetscale.com/blog/faster-interpreters-in-go-catching-up-with-cpp"> Faster Interpreters in Go </a> explains it pretty well with  performance test results. They point out there's a 20% overhead on these "instruction" functions. I'm not sure how similar a Rust implementation using boxed fn would be, but probably similar. It might turn out that just doing specialized operations would be faster for some types of work.

Finally, I have to note that aside from likely performance benefits, the function-list technique ought to make maintaining the language as a whole a lot easier. There's basically no separate VM and front end of the interpreter to keep in sync: You add new functionality to the language by having the compiler emit new kinds of functions. The VM is bound to execute them as long as they behave like the other function-instructions.


### Parser and Compiler

The parser works its way top-down recursively through the `source` input tokens to terminals in the language's grammar.  It's organized so that each parse function for an expression varient adds an expression to the `target` in order. The parsing order transforms the  hypothetical syntax tree to the flat structure the interpreter needs. 

The single pass compilation process uses the parser and a symbol table   to remember symbol locations -- every time the parser adds an expression to the `target` output, it gets the "address" and data type of the expression back.  When it has added a variable's declaration it can store that address in a symbol table along with the current scope level and data types. Some type checking can take place during parsing with information at hand, but some requires lookups from  the symbol table when variables or functions defined elsewhere are used.

I needed to compromise  somewhat on the expression type to communicate some lower level behavior to the vm, and in the compiler I had to do a few tricks to change up parsing order to  acomadate the way the vm operates so it gets enough information to jump over conditional blocks or perform loops.  Mostly this looks similar to how recursive descent parsers generate assembly language labels for variable declarations  and expression results, but in this case we use an ever incrementing address instead of labels for an assembler. 

It was hard to entirely avoid having the output of the compiler influenced by the working of the virtual machine. Because I was trying to only emit normal PL expressions I had to contort bits of the interpreter to compensate, and also had to introduce a couple of expressions coming out of the compiler that look more like machine instructions.

## The Language

The language currently only has the bare minimum features to write interesting benchmark programs. (Well, I included several data types and an 'output' statement too.) No functions yet, just variables, "for" loops, "if" statements and logical operators and comparison operators and arithmetic.

I've tried to write a naive Mandelbrot set renderer as a first real program to benchmark. No results yet.

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
