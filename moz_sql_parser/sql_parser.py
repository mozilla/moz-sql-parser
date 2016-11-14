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
DEBUG = True

SELECT = Keyword("select", caseless=True)
FROM = Keyword("from", caseless=True)
WHERE = Keyword("where", caseless=True)
GROUPBY = Keyword("group by", caseless=True)
ORDERBY = Keyword("order by", caseless=True)
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
    "+": "add",
    "-": "sub",
    "*": "mult",
    "/": "div",
    "and": "and",
    "or": "or",
    "not": "not"
}


def to_json_operator(instring, tokensStart, retTokens):
    # ARRANGE INTO {op: params} FORMAT
    tok = retTokens[0]
    op = KNOWN_OPS[tok[1]]
    return {op: [tok[i * 2] for i in range(int((len(tok) + 1) /2))]}


def to_json_call(instring, tokensStart, retTokens):
    # ARRANGE INTO {op: params} FORMAT
    tok = retTokens
    op = tok.op
    params = tok.params[0]
    if not params:
        params = None
    elif len(params) == 1:
        params = params[0]
    return {op: params}


def unquote(instring, tokensStart, retTokens):
    val = retTokens[0]
    if val.startswith("'") and val.endswith("'"):
        val = "'"+val[1:-1].replace("''", "\\'")+"'"
        val = val.replace(".", "\\.")
    elif val.startswith('"') and val.endswith('"'):
        val = '"'+val[1:-1].replace('""', '\\"')+'"'
        val = val.replace(".", "\\.")

    un = ast.literal_eval(val)
    return un

# NUMBERS
E = CaselessLiteral("E")
# binop = oneOf("= != < > >= <= eq ne lt le gt ge", caseless=True)
arithSign = Word("+-", exact=1)
realNum = Combine(
    Optional(arithSign) +
    (Word(nums) + "." + Optional(Word(nums)) | ("." + Word(nums))) +
    Optional(E + Optional(arithSign) + Word(nums))
).addParseAction(unquote)
intNum = Combine(
    Optional(arithSign) +
    Word(nums) +
    Optional(E + Optional("+") + Word(nums))
).addParseAction(unquote)

# SQL STRINGS
sqlString = Combine(Regex(r"\'(\'\'|\\.|[^'])*\'")).addParseAction(unquote)
identString = Combine(Regex(r'\"(\"\"|\\.|[^"])*\"')).addParseAction(unquote)

# EXPRESSIONS
expr = Forward()

ident = Combine(~RESERVED + (delimitedList(Word(alphas, alphanums + "_$") | identString, ".", combine=True))).setName("identifier")
primitive = realNum("literal") | intNum("literal") | sglQuotedString("literal") | ident
selectStmt = Forward()
compound = Group(
    realNum("literal").setName("float").setDebug(DEBUG) |
    intNum("literal").setName("int").setDebug(DEBUG) |
    sqlString("literal").setName("string").setDebug(DEBUG) |
    Group(ident("var") + IN + "(" + Group(delimitedList(Group(expr).setName("expression")))("set") + ")")("in").setDebug(DEBUG) |
    Group(ident("var") + IN + "(" + Group(selectStmt)("set") + ")")("in").setDebug(DEBUG) |
    (Word(alphas)("op").setName("function name") + Literal("(") + Group(delimitedList(expr))("params") + ")").addParseAction(to_json_call).setDebug(DEBUG) |
    ident
)
expr << Group(infixNotation(
    compound,
    [
        (oneOf('* /'), 2, opAssoc.LEFT, to_json_operator),
        (oneOf('+ -'), 2, opAssoc.LEFT, to_json_operator),
        (oneOf('= != > >= < <='), 2, opAssoc.LEFT, to_json_operator),
        (NOT, 1, opAssoc.RIGHT, to_json_operator),
        (AND, 2, opAssoc.LEFT, to_json_operator),
        (OR, 2, opAssoc.LEFT, to_json_operator)
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
    Optional(GROUPBY.suppress() + Group(delimitedList(column)).setName("columns"))("groupby") +
    Optional(ORDERBY.suppress() + Group(delimitedList(column)).setName("columns"))("orderby")
)

SQLParser = selectStmt

# IGNORE SOME COMMENTS
oracleSqlComment = Literal("--") + restOfLine
mySqlComment = Literal("#") + restOfLine
SQLParser.ignore(oracleSqlComment | mySqlComment)

