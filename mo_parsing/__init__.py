# encoding: utf-8
# module mo_parsing.py
#
# Copyright (c) 2003-2019  Paul T. McGuire
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#

__doc__ = """
mo_parsing module - Classes and methods to define and execute parsing grammars
=============================================================================

The mo_parsing module is an alternative approach to creating and
executing simple grammars, vs. the traditional lex/yacc approach, or the
use of regular expressions.  With mo_parsing, you don't need to learn
a new syntax for defining grammars or matching expressions - the parsing
module provides a library of classes that you use to construct the
grammar directly in Python.

Here is a program to parse "Hello, World!" (or any greeting of the form
``"<salutation>, <addressee>!"``), built up using `Word`,
`Literal`, and `And` elements
(the `'+'<ParserElement.__add__>` operators create `And` expressions,
and the strings are auto-converted to `Literal` expressions)::

    from mo_parsing import Word, alphas

    # define grammar of a greeting
    greet = Word(alphas) + "," + Word(alphas) + "!"

    hello = "Hello, World!"
    print (hello, "->", greet.parseString(hello))

The program outputs the following::

    Hello, World! -> ['Hello', ',', 'World', '!']

The Python representation of the grammar is quite readable, owing to the
self-explanatory class names, and the use of '+', '|' and '^' operators.

The `ParseResults` object returned from
`ParserElement.parseString` can be
accessed as a nested list, a dictionary, or an object with named
attributes.

The mo_parsing module handles some of the problems that are typically
vexing when writing text parsers:

  - extra or missing whitespace (the above program will also handle
    "Hello,World!", "Hello  ,  World  !", etc.)
  - quoted strings
  - embedded comments


Getting Started -
-----------------
Visit the classes `ParserElement` and `ParseResults` to
see the base classes that most other mo_parsing
classes inherit from. Use the docstrings for examples of how to:

 - construct literal match expressions from `Literal` and
   `CaselessLiteral` classes
 - construct character word-group expressions using the `Word`
   class
 - see how to create repetitive expressions using `ZeroOrMore`
   and `OneOrMore` classes
 - use `'+'<And>`, `'|'<MatchFirst>`, `'^'<Or>`,
   and `'&'<Each>` operators to combine simple expressions into
   more complex ones
 - associate names with your parsed results using
   `ParserElement.set_token_name`
 - access the parsed data, which is returned as a `ParseResults`
   object
 - find some helpful expression short-cuts like `delimitedList`
   and `oneOf`
 - find more useful common expressions in the `parsing_common`
   namespace class
"""

from mo_parsing.core import ParserElement, _PendingSkip
from mo_parsing.enhancement import (
    Combine,
    Dict,
    FollowedBy,
    Forward,
    Group,
    NotAny,
    OneOrMore,
    Optional,
    ParseElementEnhance,
    PrecededBy,
    SkipTo,
    Suppress,
    TokenConverter,
    ZeroOrMore,
)
from mo_parsing.exceptions import (
    ParseException,
    ParseException,
    ParseFatalException,
    ParseSyntaxException,
    RecursiveGrammarException,
    conditionAsParseAction,
)
from mo_parsing.expressions import And, Each, MatchFirst, Or, ParseExpression
from mo_parsing.helpers import (
    alphas8bit,
    anyCloseTag,
    anyOpenTag,
    cStyleComment,
    commaSeparatedList,
    commonHTMLEntity,
    countedArray,
    cppStyleComment,
    dblQuotedString,
    dblSlashComment,
    delimitedList,
    dictOf,
    downcaseTokens,
    hexnums,
    htmlComment,
    indentedBlock,
    infixNotation,
    javaStyleComment,
    locatedExpr,
    makeHTMLTags,
    matchOnlyAtCol,
    matchPreviousExpr,
    matchPreviousLiteral,
    nestedExpr,
    nums,
    oneOf,
    LEFT_ASSOC,
    RIGHT_ASSOC,
    originalTextFor,
    printables,
    punc8bit,
    pythonStyleComment,
    quotedString,
    removeQuotes,
    replaceHTMLEntity,
    replaceWith,
    restOfLine,
    sglQuotedString,
    srange,
    ungroup,
    unicodeString,
    upcaseTokens,
    withAttribute,
    withClass,
    commaSeparatedList,
)
from mo_parsing.results import ParseResults
from mo_parsing.tokens import (
    CaselessKeyword,
    CaselessLiteral,
    Char,
    CloseMatch,
    Empty,
    GoToColumn,
    LineEnd,
    LineStart,
    NoMatch,
    QuotedString,
    Regex,
    StringStart,
    White,
    Word,
    WordEnd,
    WordStart,
    CharsNotIn,
    Keyword,
    Literal,
    StringEnd,
    Token,
)
from mo_parsing.utils import (
    MAX_INT,
    wrap_parse_action,
    alphanums,
    alphas,
    col,
    hexnums,
    line,
    lineno,
    nums,
    printables,
    parsing_unicode,
    singleArgBuiltins,
    traceParseAction,
    unicode_set,
)

__all__ = [
    "And",
    "CaselessKeyword",
    "CaselessLiteral",
    "CharsNotIn",
    "Combine",
    "Dict",
    "Each",
    "Empty",
    "FollowedBy",
    "Forward",
    "GoToColumn",
    "Group",
    "Keyword",
    "LineEnd",
    "LineStart",
    "Literal",
    "PrecededBy",
    "MatchFirst",
    "NoMatch",
    "NotAny",
    "OneOrMore",
    "Optional",
    "Or",
    "ParseException",
    "ParseElementEnhance",
    "ParseException",
    "ParseExpression",
    "ParseFatalException",
    "ParseResults",
    "ParseSyntaxException",
    "ParserElement",
    "QuotedString",
    "RecursiveGrammarException",
    "Regex",
    "SkipTo",
    "StringEnd",
    "StringStart",
    "Suppress",
    "Token",
    "TokenConverter",
    "White",
    "Word",
    "WordEnd",
    "WordStart",
    "ZeroOrMore",
    "Char",
    "alphanums",
    "alphas",
    "alphas8bit",
    "anyCloseTag",
    "anyOpenTag",
    "cStyleComment",
    "col",
    "commaSeparatedList",
    "commonHTMLEntity",
    "countedArray",
    "cppStyleComment",
    "dblQuotedString",
    "dblSlashComment",
    "delimitedList",
    "dictOf",
    "downcaseTokens",
    "hexnums",
    "htmlComment",
    "javaStyleComment",
    "line",
    "lineno",
    "makeHTMLTags",
    "matchOnlyAtCol",
    "matchPreviousExpr",
    "matchPreviousLiteral",
    "nestedExpr",
    "nums",
    "oneOf",
    "LEFT_ASSOC",
    "RIGHT_ASSOC",
    "printables",
    "punc8bit",
    "pythonStyleComment",
    "quotedString",
    "removeQuotes",
    "replaceHTMLEntity",
    "replaceWith",
    "restOfLine",
    "sglQuotedString",
    "srange",
    "traceParseAction",
    "unicodeString",
    "upcaseTokens",
    "withAttribute",
    "indentedBlock",
    "originalTextFor",
    "ungroup",
    "infixNotation",
    "locatedExpr",
    "withClass",
    "CloseMatch",
    "parsing_unicode",
    "unicode_set",
    "conditionAsParseAction",
]
