# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Author: Kyle Lahnakoski (kyle@lahnakoski.com)
#

from __future__ import absolute_import
from __future__ import division
from __future__ import unicode_literals

import ast

from pyparsing import \
    CaselessLiteral, Word, delimitedList, Optional, Combine, Group, alphas, \
    nums, alphanums, Forward, restOfLine, Keyword, sglQuotedString, dblQuotedString, \
    Literal, ParserElement, infixNotation, oneOf, opAssoc, Regex

ParserElement.enablePackrat()
DEBUG = False

SELECT = Keyword("select", caseless=True)
FROM = Keyword("from", caseless=True)
WHERE = Keyword("where", caseless=True)
GROUPBY = Keyword("group by", caseless=True)
AS = Keyword("as", caseless=True)
AND = Keyword("and", caseless=True)
OR = Keyword("or", caseless=True)
NOT = Keyword("not", caseless=True)
IN = Keyword("in", caseless=True)

RESERVED = SELECT | FROM | WHERE | GROUPBY | AS | AND | OR | NOT | IN

KNOWN_OPS = {
    "=": "eq",
    "!=": "neq",
    ">": "gt",
    "<": "lt",
    ">=": "gte",
    "<=": "lte",
    "and": "and",
    "or": "or",
    "not": "not"
}


def fix(instring, tokensStart, retTokens):
    tok = retTokens[0]
    op = KNOWN_OPS[tok[1]]
    return {op: [tok[0], tok[2]]}


def json2value(instring, tokensStart, retTokens):
    val = retTokens[0]
    if val.startswith("'") and val.endswith("'"):
        val = "'"+val[1:-1].replace("''", "\\'")+"'"

    return ast.literal_eval(val)

# NUMBERS
E = CaselessLiteral("E")
# binop = oneOf("= != < > >= <= eq ne lt le gt ge", caseless=True)
arithSign = Word("+-", exact=1)
realNum = Combine(
    Optional(arithSign) +
    (Word(nums) + "." + Optional(Word(nums)) | ("." + Word(nums))) +
    Optional(E + Optional(arithSign) + Word(nums))
).addParseAction(json2value)
intNum = Combine(
    Optional(arithSign) +
    Word(nums) +
    Optional(E + Optional("+") + Word(nums))
).addParseAction(json2value)
sqlString = Combine(Regex(r"\'(\'\'|\\.|[^'])*\'")).addParseAction(json2value)

# EXPRESSIONS
expr = Forward()

ident = (~RESERVED + (delimitedList(Word(alphas, alphanums + "_$") | dblQuotedString, ".", combine=True))).setName("identifier")
primitive = realNum("literal") | intNum("literal") | sglQuotedString("literal") | ident
selectStmt = Forward()
compound = Group(
    (Literal("(") + expr + ")").setDebug(DEBUG) |
    realNum("literal").setName("float").setDebug(DEBUG) |
    intNum("literal").setName("int").setDebug(DEBUG) |
    sqlString("literal").setName("string").setDebug(DEBUG) |
    Group(ident("var") + IN + "(" + Group(delimitedList(Group(expr).setName("expression")))("set") + ")")("in").setDebug(DEBUG) |
    Group(ident("var") + IN + "(" + Group(selectStmt)("set") + ")")("in").setDebug(DEBUG) |
    (Word(alphas)("op").setName("function name") + Literal("(") + Group(delimitedList(expr))("params") + ")").setDebug(DEBUG) |
    ident
)
expr << Group(infixNotation(
    compound,
    [
        (oneOf('* /'), 2, opAssoc.LEFT, fix),
        (oneOf('+ -'), 2, opAssoc.LEFT, fix),
        (oneOf('= != > >= < <='), 2, opAssoc.LEFT, fix),
        (NOT, 1, opAssoc.RIGHT, fix),
        (AND, 2, opAssoc.LEFT, fix),
        (OR, 2, opAssoc.LEFT, fix)
    ]
).setName("expression"))

# SQL STATEMENT
column = Group(
    Group(expr).setName("expression")("value") + AS + ident.setName("column name")("name").setDebug(DEBUG) |
    Group(expr).setName("expression")("value") + ident.setName("column name")("name").setDebug(DEBUG) |
    Group(expr).setName("expression")("value").setDebug(DEBUG) |
    Literal('*')("value").setDebug(DEBUG)
).setName("column")
tableName = delimitedList(ident, ".", combine=True).setName("table name")

# define SQL tokens
selectStmt << (
    SELECT.suppress() + delimitedList(column)("select") +
    FROM.suppress() + delimitedList(tableName)("from") +
    Optional(WHERE.suppress() + Group(expr).setName("expression"))("where") +
    Optional(GROUPBY.suppress() + Group(delimitedList(column)).setName("columns"))("groupby")
)

SQLParser = selectStmt

# IGNORE SOME COMMENTS
oracleSqlComment = Literal("--") + restOfLine
mySqlComment = Literal("#") + restOfLine
SQLParser.ignore(oracleSqlComment | mySqlComment)

