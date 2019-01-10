# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Author: Kyle Lahnakoski (kyle@lahnakoski.com)
# Modifed by: Erwin de Haan (http://github.com/EraYaN)
#
# Made the returned structure be a proper AST.
#

from __future__ import absolute_import
from __future__ import division
from __future__ import unicode_literals

from . import ast_nodes as an

import ast
import sys

from pyparsing import Word, delimitedList, Optional, Combine, Group, alphas, alphanums, Forward, restOfLine, Keyword, Literal, ParserElement, infixNotation, opAssoc, Regex, MatchFirst, ZeroOrMore

ParserElement.enablePackrat()

# THE PARSING DEPTH IS NASTY
sys.setrecursionlimit(2000)


DEBUG = False
#END = None

all_exceptions = {}
def record_exception(instring, loc, expr, exc):
    # if DEBUG:
    #     print ("Exception raised:" + _ustr(exc))
    es = all_exceptions.setdefault(loc, [])
    es.append(exc)


def nothing(*args):
    pass

if DEBUG:
    debug = (None, None, None)
else:
    debug = (nothing, nothing, record_exception)

AND = Keyword("and", caseless=True).setName("and").addParseAction(an.AndKeyword.from_tokens).setDebugActions(*debug)
AS = Keyword("as", caseless=True).setName("as").addParseAction(an.AsKeyword.from_tokens).setDebugActions(*debug)
ASC = Keyword("asc", caseless=True).setName("asc").addParseAction(an.AscKeyword.from_tokens).setDebugActions(*debug)
BETWEEN = Keyword("between", caseless=True).setName("between").addParseAction(an.BetweenKeyword.from_tokens).setDebugActions(*debug)
CASE = Keyword("case", caseless=True).setName("case").addParseAction(an.CaseKeyword.from_tokens).setDebugActions(*debug)
COLLATENOCASE = Keyword("collate nocase", caseless=True).setName("collate nocase").addParseAction(an.CollateNoCaseKeyword.from_tokens).setDebugActions(*debug)
CROSSJOIN = Keyword("cross join", caseless=True).setName("cross join").addParseAction(an.CrossJoinKeyword.from_tokens).setDebugActions(*debug)
DESC = Keyword("desc", caseless=True).setName("desc").addParseAction(an.DescKeyword.from_tokens).setDebugActions(*debug)
ELSE = Keyword("else", caseless=True).setName("else").addParseAction(an.ElseKeyword.from_tokens).setDebugActions(*debug)
END = Keyword("end", caseless=True).setName("end").addParseAction(an.EndKeyword.from_tokens).setDebugActions(*debug)
FROM = Keyword("from", caseless=True).setName("from").addParseAction(an.FromKeyword.from_tokens).setDebugActions(*debug)
FULLJOIN = Keyword("full join", caseless=True).setName("full join").addParseAction(an.FullJoinKeyword.from_tokens).setDebugActions(*debug)
FULLOUTERJOIN = Keyword("full outer join", caseless=True).setName("full outer join").addParseAction(an.FullOuterJoinKeyword.from_tokens).setDebugActions(*debug)
GROUPBY = Keyword("group by", caseless=True).setName("group by").addParseAction(an.GroupByKeyword.from_tokens).setDebugActions(*debug)
HAVING = Keyword("having", caseless=True).setName("having").addParseAction(an.HavingKeyword.from_tokens).setDebugActions(*debug)
IN = Keyword("in", caseless=True).setName("in").addParseAction(an.InKeyword.from_tokens).setDebugActions(*debug)
NOTIN = Keyword("not in", caseless=True).setName("not in").addParseAction(an.NotInKeyword.from_tokens).setDebugActions(*debug)
INNERJOIN = Keyword("inner join", caseless=True).setName("inner join").addParseAction(an.InnerJoinKeyword.from_tokens).setDebugActions(*debug)
IS = Keyword("is", caseless=True).setName("is").addParseAction(an.IsKeyword.from_tokens).setDebugActions(*debug)
JOIN = Keyword("join", caseless=True).setName("join").addParseAction(an.JoinKeyword.from_tokens).setDebugActions(*debug)
LEFTJOIN = Keyword("left join", caseless=True).setName("left join").addParseAction(an.LeftJoinKeyword.from_tokens).setDebugActions(*debug)
LEFTOUTERJOIN = Keyword("left outer join", caseless=True).setName("left outer join").addParseAction(an.LeftOuterJoinKeyword.from_tokens).setDebugActions(*debug)
LIMIT = Keyword("limit", caseless=True).setName("limit").addParseAction(an.LimitKeyword.from_tokens).setDebugActions(*debug)
OFFSET = Keyword("offset", caseless=True).setName("offset").addParseAction(an.OffsetKeyword.from_tokens).setDebugActions(*debug)
LIKE = Keyword("like", caseless=True).setName("like").addParseAction(an.LikeKeyword.from_tokens).setDebugActions(*debug)
NOTLIKE = Keyword("not like", caseless=True).setName("not like").addParseAction(an.NotLikeKeyword.from_tokens).setDebugActions(*debug)
ON = Keyword("on", caseless=True).setName("on").addParseAction(an.OnKeyword.from_tokens).setDebugActions(*debug)
USING = Keyword("using", caseless=True).setName("using").addParseAction(an.UsingKeyword.from_tokens).setDebugActions(*debug)
OR = Keyword("or", caseless=True).setName("or").addParseAction(an.OrKeyword.from_tokens).setDebugActions(*debug)
ORDERBY = Keyword("order by", caseless=True).setName("order by").addParseAction(an.OrderByKeyword.from_tokens).setDebugActions(*debug)
RIGHTJOIN = Keyword("right join", caseless=True).setName("right join").addParseAction(an.RightJoinKeyword.from_tokens).setDebugActions(*debug)
RIGHTOUTERJOIN = Keyword("right outer join", caseless=True).setName("right outer join").addParseAction(an.RightOuterJoinKeyword.from_tokens).setDebugActions(*debug)
SELECT = Keyword("select", caseless=True).setName("select").addParseAction(an.SelectKeyword.from_tokens).setDebugActions(*debug)
THEN = Keyword("then", caseless=True).setName("then").addParseAction(an.ThenKeyword.from_tokens).setDebugActions(*debug)
UNION = Keyword("union", caseless=True).setName("union").addParseAction(an.UnionKeyword.from_tokens).setDebugActions(*debug)
UNIONALL = Keyword("union all", caseless=True).setName("union all").addParseAction(an.UnionAllKeyword.from_tokens).setDebugActions(*debug)
WHEN = Keyword("when", caseless=True).setName("when").addParseAction(an.WhenKeyword.from_tokens).setDebugActions(*debug)
WHERE = Keyword("where", caseless=True).setName("where").addParseAction(an.WhereKeyword.from_tokens).setDebugActions(*debug)
WITH = Keyword("with", caseless=True).setName("with").addParseAction(an.WithKeyword.from_tokens).setDebugActions(*debug)


KEYWORDS = [
    AND,
    AS,
    ASC,
    BETWEEN,
    CASE,
    COLLATENOCASE,
    CROSSJOIN,
    DESC,
    ELSE,
    END,
    FROM,
    FULLJOIN,
    FULLOUTERJOIN,
    GROUPBY,
    HAVING,
    IN,
    NOTIN,
    INNERJOIN,
    IS,
    JOIN,
    LEFTJOIN,
    LEFTOUTERJOIN,
    LIMIT,
    OFFSET,
    LIKE,
    NOTLIKE,
    ON,
    USING,
    OR,
    ORDERBY,
    RIGHTJOIN,
    RIGHTOUTERJOIN,
    SELECT,
    THEN,
    UNION,
    UNIONALL,
    WHEN,
    WHERE,
    WITH
]

RESERVED = MatchFirst(KEYWORDS)

KNOWN_OPS = [
    {"op":Literal("||").setName("concat").setParseAction(an.ConcatOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.BinOp.from_tokens},
    {"op":Literal("*").setName("mul").setParseAction(an.MulOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.BinOp.from_tokens},
    {"op":Literal("/").setName("div").setParseAction(an.DivOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.BinOp.from_tokens},
    {"op":Literal("+").setName("add").setParseAction(an.AddOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.BinOp.from_tokens},
    {"op":Literal("-").setName("sub").setParseAction(an.SubOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.BinOp.from_tokens},
    {"op":Literal("<<").setName("lshift").setParseAction(an.LShiftOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.BinOp.from_tokens},
    {"op":Literal(">>").setName("rshift").setParseAction(an.RShiftOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.BinOp.from_tokens},
    {"op":Literal("&").setName("bitand").setParseAction(an.BitAndOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.BinOp.from_tokens},
    {"op":Literal("|").setName("bitor").setParseAction(an.BitOrOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.BinOp.from_tokens},
    {"op":Literal("^").setName("bitxor").setParseAction(an.BitXorOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.BinOp.from_tokens},

    {"op":Literal("<>").setName("neq").setParseAction(an.NEqOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.CompOp.from_tokens},
    {"op":Literal(">").setName("gt").setParseAction(an.GtOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.CompOp.from_tokens},
    {"op":Literal("<").setName("lt").setParseAction(an.LtOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.CompOp.from_tokens},
    {"op":Literal(">=").setName("gte").setParseAction(an.GtEOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.CompOp.from_tokens},
    {"op":Literal("<=").setName("lte").setParseAction(an.LtEOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.CompOp.from_tokens},
    {"op":Literal("=").setName("eq").setParseAction(an.EqOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.CompOp.from_tokens},
    {"op":Literal("==").setName("eq").setParseAction(an.EqOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.CompOp.from_tokens},
    {"op":Literal("!=").setName("neq").setParseAction(an.NEqOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.CompOp.from_tokens},
    {"op":IN.setName("in").setParseAction(an.InOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.CompOp.from_tokens},
    {"op":NOTIN.setName("notin").setParseAction(an.NotInOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.CompOp.from_tokens},
    {"op":IS.setName("is").setParseAction(an.IsOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.CompOp.from_tokens},
    {"op":LIKE.setName("like").setParseAction(an.LikeOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.CompOp.from_tokens},
    {"op":NOTLIKE.setName("not like").setParseAction(an.NotLikeOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.CompOp.from_tokens},

    {"op":OR.setName("or").setParseAction(an.OrOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.BinOp.from_tokens},
    {"op":AND.setName("and").setParseAction(an.AndOperator.from_tokens).setDebugActions(*debug),"ast_elem":an.BinOp.from_tokens},
]

# NUMBERS
realNum = Regex(r"[+-]?(\d+\.\d*|\.\d+)([eE][+-]?\d+)?").addParseAction(an.DoubleValue.from_tokens)
intNum = Regex(r"[+-]?\d+([eE]\+?\d+)?").addParseAction(an.IntValue.from_tokens)

# STRINGS, NUMBERS, VARIABLES
sqlString = Regex(r"\'(\'\'|\\.|[^'])*\'").addParseAction(an.StringValue.from_tokens)
wildcard = Literal("*").setName("wildcard").addParseAction(an.Wildcard.from_tokens)
identString = Regex(r'\"(\"\"|\\.|[^"])*\"')
mysqlidentString = Regex(r'\`(\`\`|\\.|[^`])*\`')

tableident = Group(~RESERVED + (delimitedList(Word(alphas + "_", alphanums + "_$") | identString | mysqlidentString, delim=".", combine=False))).setName("table identifier").addParseAction(an.TableReference.from_tokens)

colident = Group(~RESERVED + (delimitedList(wildcard | Word(alphas + "_", alphanums + "_$") | identString | mysqlidentString, delim=".", combine=False))).setName("column identifier").addParseAction(an.ColumnReference.from_tokens)

alias = Group(~RESERVED + (delimitedList(wildcard | Word(alphas + "_", alphanums + "_$") | identString | mysqlidentString, delim=".", combine=False))).setName("alias identifier").addParseAction(an.Alias.from_tokens)


# EXPRESSIONS
expr = Forward()

# CASE
case = (
    CASE +
    Group(ZeroOrMore((WHEN + expr("when") + THEN + expr("then")).addParseAction(an.When.from_tokens)))("case") +
    Optional(ELSE + expr("else")) +
    END
).addParseAction(an.Case.from_tokens)

selectStmt = Forward()
compound = (
    (Keyword("not", caseless=True)("op").setDebugActions(*debug) + expr("params")).addParseAction(an.Not.from_tokens) |
    (Keyword("distinct", caseless=True).suppress().setDebugActions(*debug) + expr("params")).addParseAction(an.Distinct.from_tokens) |
    Keyword("null", caseless=True).setName("null").addParseAction(an.NullValue.from_tokens).setDebugActions(*debug) |
    Keyword("true", caseless=True).setName("true").addParseAction(an.TrueValue.from_tokens).setDebugActions(*debug) |
    Keyword("false", caseless=True).setName("true").addParseAction(an.FalseValue.from_tokens).setDebugActions(*debug) |
    case |
    (Literal("(").setDebugActions(*debug).suppress() + selectStmt + Literal(")").suppress()) |
    (Literal("(").setDebugActions(*debug).suppress() + Group(delimitedList(expr)) + Literal(")").suppress()) |
    realNum.setName("float").setDebugActions(*debug) |
    intNum.setName("int").setDebugActions(*debug) |
    (Literal("-")("op").addParseAction(an.USubOperator.from_tokens).setDebugActions(*debug) + expr("params")).addParseAction(an.UnOp.from_tokens) |
    (Literal("+")("op").addParseAction(an.UAddOperator.from_tokens).setDebugActions(*debug) + expr("params")).addParseAction(an.UnOp.from_tokens) |
    (Literal("~")("op").addParseAction(an.BitNotOperator.from_tokens).setDebugActions(*debug) + expr("params")).addParseAction(an.UnOp.from_tokens) |
    sqlString.setName("string").setDebugActions(*debug) |
    (
        Word(alphas)("op").setName("function name").setDebugActions(*debug) +
        Literal("(").setName("func_param").suppress().setDebugActions(*debug) +
        Optional(selectStmt | Group(delimitedList(expr)))("params") +
        Literal(")").setName("func_param_end").suppress().setDebugActions(*debug)
    ).addParseAction(an.FunctionCall.from_tokens).setDebugActions(*debug) |
    colident.copy().setName("variable").setDebugActions(*debug)
)
expr << Group(infixNotation(
    compound,
    [
        (
            (BETWEEN, AND),
            3,
            opAssoc.LEFT,
            an.Between.from_tokens
        )
    ]+[
        (
            o["op"],
            2,
            opAssoc.LEFT,
            o["ast_elem"]
        )
        for o in KNOWN_OPS
    ]+[
        (
            COLLATENOCASE,
            1,
            opAssoc.LEFT,
            an.BinOp.from_tokens
        )
    ]
).setName("expression").setDebugActions(*debug))

# SQL STATEMENT
selectColumn = Group(
    Group(expr).setName("expression1")("value").setDebugActions(*debug) + Optional(Optional(AS) + alias("name").setDebugActions(*debug))
).setName("column").addParseAction(an.SelectColumn.from_tokens)

table_source = (
    (
        (
            Literal("(").setDebugActions(*debug).suppress() +
            selectStmt +
            Literal(")").setDebugActions(*debug).suppress()
        ).setName("table source").setDebugActions(*debug)
    )("value") +
    Optional(
        Optional(AS) +
        alias("name").setName("table alias").setDebugActions(*debug)
    )
    |
    (
        tableident("value").setName("table name").setDebugActions(*debug) +
        Optional(AS) +
        alias("name").setName("table alias").setDebugActions(*debug)
    )
    |
    tableident.setName("table name").setDebugActions(*debug)
).addParseAction(an.FromSource.from_tokens)

join = (
    (
        CROSSJOIN.addParseAction(an.JoinCross.from_tokens) |
        FULLJOIN.addParseAction(an.JoinFull.from_tokens) |
        FULLOUTERJOIN.addParseAction(an.JoinFullOuter.from_tokens) |
        INNERJOIN.addParseAction(an.JoinInner.from_tokens) |
        JOIN.addParseAction(an.JoinOuter.from_tokens) |
        LEFTJOIN.addParseAction(an.JoinLeft.from_tokens) |
        LEFTOUTERJOIN.addParseAction(an.JoinLeftOuter.from_tokens) |
        RIGHTJOIN.addParseAction(an.JoinRight.from_tokens) |
        RIGHTOUTERJOIN.addParseAction(an.JoinRightOuter.from_tokens)
        )("op") +
        Group(table_source)("join") +
         Optional(
             (
                 ON.addParseAction(an.JoinConstraintTypeOn.from_tokens)|
                 USING.addParseAction(an.JoinConstraintTypeUsing.from_tokens)
            )("constrainttype") +
             expr("constraints")
             )
    ).addParseAction(an.Join.from_tokens)

sortColumn = (expr("value").setName("sort1").setDebugActions(*debug) +
              Optional(DESC("sort").addParseAction(an.OrderDescending.from_tokens) | ASC("sort").addParseAction(an.OrderAscending.from_tokens)) |
              expr("value").setName("sort2").setDebugActions(*debug)).addParseAction(an.OrderByColumn.from_tokens)

unions = (UNION.addParseAction(an.UnionDistinct.from_tokens) ^ UNIONALL.addParseAction(an.UnionAll.from_tokens))

selectCore = Group(
                SELECT.suppress().setDebugActions(*debug) + delimitedList(selectColumn)("select").addParseAction(an.Select.from_tokens) +
                Optional(
                    FROM.suppress().setDebugActions(*debug) + (delimitedList(Group(table_source)) + ZeroOrMore(join))("from").addParseAction(an.From.from_tokens) +
                    Optional(WHERE.suppress().setDebugActions(*debug) + expr.setName("where"))("where").addParseAction(an.Where.from_tokens) +
                    Optional(GROUPBY.suppress().setDebugActions(*debug) + delimitedList(Group(selectColumn))("groupby").setName("groupby").addParseAction(an.GroupBy.from_tokens)) +
                    Optional(HAVING.suppress().setDebugActions(*debug) + expr("having").setName("having").addParseAction(an.Having.from_tokens)) +
                    Optional(LIMIT.suppress().setDebugActions(*debug) + expr("limit").addParseAction(an.Limit.from_tokens)) +
                    Optional(OFFSET.suppress().setDebugActions(*debug) + expr("offset").addParseAction(an.Offset.from_tokens))
                )
            )

# define SQL tokens
selectStmt << Group(
    Group(Group(selectCore)^Group(
        (selectCore + ZeroOrMore(unions + selectCore)
        )
    )("union").addParseAction(an.Union.from_tokens))("from") +
    Optional(ORDERBY.suppress().setDebugActions(*debug) + delimitedList(Group(sortColumn))("orderby").setName("orderby").addParseAction(an.OrderBy.from_tokens)) +
    Optional(LIMIT.suppress().setDebugActions(*debug) + expr("limit").addParseAction(an.Limit.from_tokens)) +
    Optional(OFFSET.suppress().setDebugActions(*debug) + expr("offset").addParseAction(an.Offset.from_tokens))
).addParseAction(an.Query.from_tokens)

SQLParser = delimitedList(selectStmt, delim=";")

# IGNORE SOME COMMENTS
oracleSqlComment = Literal("--") + restOfLine
mySqlComment = Literal("#") + restOfLine
SQLParser.ignore(oracleSqlComment | mySqlComment)
