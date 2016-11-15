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
    nums, alphanums, Forward, restOfLine, Keyword, Literal, ParserElement, infixNotation, opAssoc, Regex, MatchFirst

ParserElement.enablePackrat()
DEBUG = False

keywords = ["select", "from", "where", "group by", "order by", "with", "as"]

KNOWN_OPS = [
    {"op": "*", "name": "mult"},
    {"op": "/", "name": "div"},
    {"op": "+", "name": "add"},
    {"op": "-", "name": "sub"},
    {"op": "=", "name": "eq"},
    {"op": "!=", "name": "neq"},
    {"op": "<>", "name": "neq"},
    {"op": ">", "name": "gt"},
    {"op": "<", "name": "lt"},
    {"op": ">=", "name": "gte"},
    {"op": "<=", "name": "lte"},
    {"op": "in", "name": "in"},
    {"op": "not", "name": "not", "arity": 1},
    {"op": "and", "name": "and"},
    {"op": "or", "name": "or"}
]

locs = locals()
reserved = []
for k in keywords:
    name, value = k.upper().replace(" ", ""), Keyword(k, caseless=True)
    locs[name] = value
    reserved.append(value)
for o in KNOWN_OPS:
    name = o['op'].upper()
    value = locs[name] = o['literal'] = CaselessLiteral(o['op'])
    reserved.append(value)

RESERVED = MatchFirst(reserved)


def to_json_operator(instring, tokensStart, retTokens):
    # ARRANGE INTO {op: params} FORMAT
    tok = retTokens[0]
    op = filter(lambda o: o['op'] == tok[1], KNOWN_OPS)[0]['name']
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
primitive = realNum("literal") | intNum("literal") | sqlString | ident
selectStmt = Forward()
compound = Group(
    realNum("literal").setName("float").setDebug(DEBUG) |
    intNum("literal").setName("int").setDebug(DEBUG) |
    sqlString("literal").setName("string").setDebug(DEBUG) |
    (Literal("(").suppress() + Group(delimitedList(expr)) + Literal(")").suppress()).setDebug(DEBUG) |
    (Word(alphas)("op").setName("function name") + Literal("(") + Group(delimitedList(expr))("params") + ")").addParseAction(to_json_call).setDebug(DEBUG) |
    ident
)
expr << Group(infixNotation(
    compound,
    [(o['literal'], o.get('arity', 2), opAssoc.LEFT, to_json_operator) for o in KNOWN_OPS]
).setName("expression"))

# SQL STATEMENT
column = Group(
    Group(expr).setName("expression")("value") + AS + ident.setName("column name")("name").setDebug(DEBUG) |
    Group(expr).setName("expression")("value") + ident.setName("column name")("name").setDebug(DEBUG) |
    Group(expr).setName("expression")("value").setDebug(DEBUG) |
    Literal('*')("value").setDebug(DEBUG)
).setName("column")
tableName = ident.setName("table name")

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

