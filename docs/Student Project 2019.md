
# PyParsing Student Project (GSOC 2019)


## Background 

`moz-sql-parser` uses the [pyparsing library](https://github.com/pyparsing/pyparsing): This library makes the language specification easy, much like a [PEG](https://en.wikipedia.org/wiki/Parsing_expression_grammar).  Personally, this library provides the best parser specification language I have seen anywhere else: taking advantage pf Python's operator overloading and visual simplicity to provide a simple-yet-powerful domain specific language.

## Problem

Mozilla has a [simple SQL parser](https://github.com/mozilla/moz-sql-parser), but [it does not work for "complex" SQL](https://github.com/mozilla/moz-sql-parser/issues/41). Actually, we can hardly call the SQL "complex" when it breaks with so few tokens. 


## Solutions


Depending on how deep you look, there are three ways this problem can be solved

### moz-sql-parser should better-define the grammar rules

The language specification for infix operators uses too much stack space. To reduce this stack space, the operators (and their operands) should be parsed as an alternating sequence of operators and operands, with some post-processing to assemble the parse tree in precedence order.

I do not like this solution because it is working around `pyparsing` rather than with it. The grammar gets complicated, without doing any more. Plus, this type of solution can be made to work in general, for the benefit of others. 

### The pyparsing infixNotation is busted

The problem is caused by [infixNotation in the pyparsing library](https://github.com/pyparsing/pyparsing/issues/26).  If we fix pyparsing's infixNotation method, much like we would have fixed `moz-sql-parser`, then we can gain all the same benefits, while benefiting others who use pyparsing.

This is more work, as the pyparsing library will require some refactoring to track more context on the alternating operators/operands for later tree assembly.
 
I *think* this is the most pragmatic solution, but it may just defer the inevitable O(2^N) parsing problems; which will show up in some other parsing sequence; leaving `moz-sql-parser` still slow on "complex" SQL.  

### Backtrack parsers have inherent O(2^n) issues

Fundamentally, the problem is caused by backtracking parsers, which run the risk of O(2^n) parsing times.  `infixNotation()` generates a number of left-recursion parsing rules, which cause the O(2^n) parsing times. We could attempt to solve this: The known solution is to [remove the left recursion](https://en.wikipedia.org/wiki/Left_recursion#Removing_left_recursion). This will result in a significantly faster parser with much less stack usage.

But, this solution is complicated. Removing left recursion changes the parser significantly; which results in a different parse tree. It is not clear how easy it is to implement "suitable bookkeeping" (see wikipedia link) to fix that problem. This is made more complicated by the pyparsing code, which may have design that directly conflicts with this strategy: It may be easier to write a new parser generator.

Going with this solution requires a student with exceptional skills. 


## GSOC 

The project steps would look something like:

* Fork pyparsing code, use it as a basis for your work
* Write prototype parser re-writer to remove left recursion, including the "book keeping" required to assemble final parse tree **this is the hard part**
* At this point we have a better parser.
* Split up project into a number of refactoring PRs for pyparsing project; separating the many DSL features from the core parsing logic; merging the fork back into pyparsing.

 



