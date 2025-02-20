A demonstration of a technique for compiling to a flattened structure and interpreting it mostly left to right, rather than recursively top-down. The compiler adds to the target byte code in a natural recursive descent style.

This approach ought to execute many times faster than classic tree-walking without significantly complicating the interpreter or target format.

It also has the benefit of being relatively  easy to translate to other intermediate representations and even assembly languages. 

One disadvantage is that it will be harder to apply suphisticated optimizations since there's no AST, or at least one has to create one after the fact to analyze the program, then re-emit the flattened code if you wanted to optimize on a tree.

