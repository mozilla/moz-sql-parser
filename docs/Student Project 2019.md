
# PyParsing Student Project (GSOC 2019)


## Background

`moz-sql-parser` uses the [pyparsing library](https://github.com/pyparsing/pyparsing): This library makes the language specification easy, much like a [PEG](https://en.wikipedia.org/wiki/Parsing_expression_grammar).  Personally, this library provides the best parser specification language I have seen anywhere else: taking advantage pf Python's operator overloading and visual simplicity to provide a simple-yet-powerful domain specific language.

## Problem 1

Mozilla has a [simple SQL parser](https://github.com/mozilla/moz-sql-parser), but [it does not work for "complex" SQL](https://github.com/mozilla/moz-sql-parser/issues/41). Actually, we can hardly call the SQL "complex" when it breaks with so few tokens.

## Solutions for Problem 1


Depending on how deep you look, there are three ways this problem can be solved

### moz-sql-parser should better-define the grammar rules

The language specification for infix operators uses too much stack space.  To reduce this stack space, the operators (and their operands) should be parsed as an alternating sequence of operators and operands, with some post-processing to assemble the parse tree in precedence order.

I do not like this solution because it is working around `pyparsing` rather than with it.  The grammar get complicated, without doing any more.  Plus, this type of solution can be made to work in general, for the benefit of others

### The pyparsing infixNotation is busted

The problem is caused by [infixNotation in the pyparsing library](https://github.com/pyparsing/pyparsing/issues/26).  If we fix this method, much like we would have fixed `moz-sql-parser`, then we can gain all the same benefits, while benefiting others who use this same code.

This is more work, as the pyparsing library will require some refactoring to track more context on the alternating operators/operands for later tree assembly.

I *think* this is the most pragmatic solution, but it may just defer the inevitable O(2^N) parsing problems; which will show up in some other parsing sequence; leaving `moz-sql-parser` to still be slow on "complex" SQL.

### Backtrack parsers have inherent O(2^n) issues

Fundamentally, the problem is caused by backtracking parsers, which run the risk of O(2^n) parsing times.  `infixNotation()` generates a number of left-recursion parsing rules, which cause the O(2^n) parsing times. We could attempt to solve this: The known solution is to [remove the left recursion])https://en.wikipedia.org/wiki/Left_recursion#Removing_left_recursion). This will result in a significantly faster parser with much less stack usage.

But, this solution is complicated: Removing left recursion changes the parse parser significantly; which results in a different parse tree. It is not clear how easy it is to implement "suitable bookkeeping" (see wikipedia link) to fix that problem. This is made more complicated by the pyparsing code, which may have design that directly conflicts with this strategy:  It may be easier to write a new parser generator.

Going with this solution requires a student with exceptional skills. If it is done, then steps would look like:

* Fork pyparsing code, use it as a basis for your work
* Write prototype parser re-writer to remove left recursion, including the "book keeping" required to assemble final parse tree **this is the hard part**
* Split up project into a number of refactoring PRs for pyparsing project; separating the many DSL features from the core parsing logic.
* One final PR that will replace the old parser with the new one


## Problem 2

The runtime of parsing an SQL statement with the Mozilla SQL Parser is relatively slow. When compared with the popular [sqlparse](https://github.com/andialbrecht/sqlparse) Python project, getting an initial result with Mozilla's parser is upwards of 10x slower. (However, the downside of sqlparse is that the outputted format is simply a list of tokens, which would require extensive post-processing to generate the desired tree structure that moz-sql-parser provides.

## Solutions for Problem 2

There are probably multiple areas where the runtime can be improved; here are a few options:

### Solve problem 1
The solutions for problem 1 may also improve the runtime for parsing, at least for more simple SQL statements.

## Revise how pyparsing used
The pyparsing project recently include a list of [performance tips](https://github.com/pyparsing/pyparsing/wiki/Performance-Tips), and some of these can probably be used to speed up the Mozilla parser.

## Improvements to pyparsing
The pyparsing library could benefit from optimizations.  This includes cleaning up the basic data structures it uses: Using less attributes, not copying whole objects, using **slots_**, etc. Additionally, some of the code could be ported to Cython or C extensions, where certain operations can be much faster in a lower-level language.
