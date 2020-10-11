
# PyParsing Student Project (GSOC 2019)

<div style="width:100%;background:red;color:white;text-align:center;font-weight:bold;">See Student Questions below</div>

## Background

`moz-sql-parser` uses the [pyparsing library](https://github.com/pyparsing/pyparsing): This library makes the language specification easy, much like a [PEG](https://en.wikipedia.org/wiki/Parsing_expression_grammar).  Personally, this library provides the best parser specification language I have seen anywhere else: taking advantage pf Python's operator overloading and visual simplicity to provide a simple-yet-powerful domain specific language.

## Problem 1

Mozilla has a [simple SQL parser](https://github.com/mozilla/moz-sql-parser), but [it does not work for "complex" SQL](https://github.com/mozilla/moz-sql-parser/issues/41). Actually, we can hardly call the SQL "complex" when it breaks with so few tokens.

## Solutions for Problem 1


Depending on how deep you look, there are three ways this problem can be solved

### moz-sql-parser should better-define the grammar rules

The language specification for infix operators uses too much stack space. To reduce this stack space, the operators (and their operands) should be parsed as an alternating sequence of operators and operands, with some post-processing to assemble the parse tree in precedence order.

I do not like this solution because it is working around `pyparsing` rather than with it. The grammar gets complicated, without doing any more. Plus, this type of solution can be made to work in general, for the benefit of others. 


### The pyparsing infixNotation is busted

The problem is caused by [infixNotation in the pyparsing library](https://github.com/pyparsing/pyparsing/issues/26).  If we fix pyparsing's infixNotation method, much like we would have fixed `moz-sql-parser`, then we can gain all the same benefits, while benefiting others who use pyparsing.

This is more work, as the pyparsing library will require some refactoring to track more context on the alternating operators/operands for later tree assembly.
 
I *think* this is the most pragmatic solution, but it may just defer the inevitable O(2^N) parsing problems; which will show up in some other parsing sequence; leaving `moz-sql-parser` still slow on "complex" SQL.  

### Backtrack parsers have inherent O(2^n) issues

**This might be invalid. The `infixNotation()` already prevents backtracking at the expense of consuming stackspace, maybe** 

Fundamentally, the problem is caused by backtracking parsers, which run the risk of O(2^n) parsing times.  `infixNotation()` generates a number of left-recursion parsing rules, which cause the O(2^n) parsing times. We could attempt to solve this: The known solution is to [remove the left recursion](https://en.wikipedia.org/wiki/Left_recursion#Removing_left_recursion). This will result in a significantly faster parser with much less stack usage.

But, this solution is complicated. Removing left recursion changes the parser significantly; which results in a different parse tree. It is not clear how easy it is to implement "suitable bookkeeping" (see wikipedia link) to fix that problem. This is made more complicated by the pyparsing code, which may have design that directly conflicts with this strategy: It may be easier to write a new parser generator.

Going with this solution requires a student with exceptional skills.


## GSOC 

The project steps would look something like:

* Fork pyparsing code, use it as a basis for your work
* Write prototype parser re-writer to remove left recursion, including the "book keeping" required to assemble final parse tree **this is the hard part**
* At this point we have a better parser.
* Split up project into a number of refactoring PRs for pyparsing project; separating the many DSL features from the core parsing logic; merging the fork back into pyparsing.
* One final PR to pyparsing that will replace the old parser with the new one


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

## Student Questions

I will be adding questions here:


**April 3rd, 2019**

**How can I make the parsing more efficient?**

Look at `infixNotation()`, notice how it handles a single operator, we will focus on just `LEFT_ASSOC`, and `arity==2` because the rest have a similar problem:

https://github.com/pyparsing/pyparsing/blob/0d88a303a7f7e574bfc0c06ad6f84ca8c9d4d248/pyparsing.py#L5661

    matchExpr = _FB(lastExpr + opExpr + lastExpr) + Group( lastExpr + OneOrMore( opExpr + lastExpr ) )

I am not sure what the first term `_FB(lastExpr + opExpr + lastExpr) ` is doing, I am assuming it optimizes. the second term does the parsing. Here, I have simplified it:

    matchExpr = Group( lastExpr + OneOrMore( opExpr + lastExpr ) )

Notice that the `lastExpr` is from the last iteration of the loop: 

    thisExpr <<= ( matchExpr.setName(termName) | lastExpr )
    lastExpr = thisExpr

which means `lastExpr` is recursive, and as deep as there are operators (moz-sql-parser has ?19? operators)  Let me simplify the expression more, and put it in terms of `N` (`N` is the the number of operators available in SQL expressions).

    lastExpr[N] = lastExpr[N-1] + OneOrMore( opExpr + lastExpr[N-1] )

For any operator, this parser will explore the possible 2^19 branches**. If you also consider that each step consumes some amount of stack space to parse the base expressions, then you can imagine why the parser runs out of stack space easily.

** Until now, I had thought the parser tried all 2^19 branches, but that may not be the case; The parser is depth-first, so logarithmic with respect to the search space. Therefore, the runtime may be a respectable `O(N*E)`  where `N` is the number of possible operators, and `E` is the number of observed operators. The main problem is still the heavy use of stack space to parse the possible expressions.


**February 14th, 2019**


**Any tasks you want me to complete?**

The first step in that project is to refactor the `pyparsing.py` code. By refactoring you will get a sense of the codebase. Your first couple of attempts may fail as you learn what the code is about. In those cases, go back to master branch and start a new branch.

* A simple refactoring is splitting the single file into many, and ensuring it still works; this helps with understanding what parts depend on what other parts, and may reveal where the complicated logic is. Some things will move to new files easily; those are not usually not the core of the program. As you split into files you will notice a certain number of methods are tangled up with each other: that is where the real logic of the program is
* Another refactoring is cleaning up the token attributes: right now they are dynamically assigned (because Python is awesome) but standardizing them and turning them into __slots__ will make the program a bit faster 
* There are a number of "examples"; they should be converted to tests so they are easy to run. Plus, the tests will help ensure your refactorings are good.


**What will the selection be based on?** 

Beyond the GSOC requirements, the student will be accepted if they can convince me they can make the parser faster. I expect you to show knowledge of *the theory of computation*, and demonstrate you can handle complicated code. If your plan points out details that I did not think of, then you will have proven you understand the domain.


**Is there anything I can do now so as to improve my chances**

Refactoring is probably best; it will give you a head start in the `pyparsing` itself, and may reveal the details of implementation you can put in your proposal to impress me.


**What are the very first steps?** 

1. fork the `pyparsing` repo, 
2. clone it locally, 
3. make a branch, and 
4. start a refactoring
5. make a pull request **on your own repo**, and ask me for a "review"
 
The Github Pull Request will allow us to discuss code easier.


**What if I get the project done early?**

GSOC demands you work all summer. So, no matter how far you get, you still must put in summer hours. **if** you get accepted to GSOC, and **if** you are successful, then I will just give you more work.


**Can you guide me, even before the application process?**

Yes, I am happy to provide feedback on any effort you put towards this project, or any others. So, even if you do not get admitted to GSOC, I can provide you with code reviews, suggestions, and evaluations. 


