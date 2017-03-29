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
DEBUG = True

keywords = ["select", "from", "where", "group by", "order by", "with", "as", "desc"]

KNOWN_OPS = [
    {"op": "*", "name": "mult", "type": Literal},
    {"op": "/", "name": "div", "type": Literal},
    {"op": "+", "name": "add", "type": Literal},
    {"op": "-", "name": "sub", "type": Literal},
    {"op": "=", "name": "eq", "type": Literal},
    {"op": "==", "name": "eq", "type": Literal},
    {"op": "!=", "name": "neq", "type": Literal},
    {"op": "<>", "name": "neq", "type": Literal},
    {"op": ">", "name": "gt", "type": Literal},
    {"op": "<", "name": "lt", "type": Literal},
    {"op": ">=", "name": "gte", "type": Literal},
    {"op": "<=", "name": "lte", "type": Literal},
    {"op": "in", "name": "in", "type": Keyword},
    {"op": "and", "name": "and", "type": Keyword},
    {"op": "or", "name": "or", "type": Keyword}
]

locs = locals()
reserved = []
for k in keywords:
    name, value = k.upper().replace(" ", ""), Keyword(k, caseless=True)
    locs[name] = value
    reserved.append(value)
for o in KNOWN_OPS:
    name = o['op'].upper()
    value = locs[name] = o['literal'] = o['type'](o['op']).setName(o['op'])
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
    (Literal("-").setResultsValue("neg").setResultsName("op").setDebug(DEBUG) + expr("params")).addParseAction(to_json_call) |
    (Literal("not").setResultsName("op").setDebug(DEBUG) + expr("params")).addParseAction(to_json_call) |
    realNum.setName("float").setDebug(DEBUG) |
    intNum.setName("int").setDebug(DEBUG) |
    sqlString("literal").setName("string").setDebug(DEBUG) |
    (Literal("(").suppress() + Group(delimitedList(expr)) + Literal(")").suppress()).setDebug(DEBUG) |
    (Word(alphas)("op").setName("function name") + Literal("(") + Group(delimitedList(expr))("params") + ")").addParseAction(to_json_call).setDebug(DEBUG) |
    ident.copy().setName("variable").setDebug(DEBUG)
)
expr << Group(infixNotation(
    compound,
    [
        (
            o['literal'],
            2,
            opAssoc.LEFT,
            to_json_operator
        )
        for o in KNOWN_OPS
    ]
).setName("expression").setDebug(DEBUG))

# SQL STATEMENT
selectColumn = Group(
    Group(expr).setName("expression1")("value").setDebug(DEBUG) + Optional(Optional(AS) + ident.copy().setName("column_name1")("name").setDebug(DEBUG)) |
    Literal('*')("value").setDebug(DEBUG)
).setName("column")


tableName = ident("value").setName("table_name1").setDebug(DEBUG) + Optional(AS) + ident("name").setName("table_alias1").setDebug(DEBUG) | \
            ident.setName("table_name2").setDebug(DEBUG)

sortColumn = expr("value").setName("sort1").setDebug(DEBUG) + Optional(DESC("sort")) | \
             expr("value").setName("sort2").setDebug(DEBUG)

# define SQL tokens
selectStmt << (
    SELECT.suppress().setDebug(DEBUG) + delimitedList(selectColumn)("select") +
    Optional(
        FROM.suppress().setDebug(DEBUG) + delimitedList(tableName)("from") +
        Optional(WHERE.suppress().setDebug(DEBUG) + Group(expr).setName("expression"))("where") +
        Optional(GROUPBY.suppress().setDebug(DEBUG) + Group(delimitedList(selectColumn)).setName("columns"))("groupby") +
        Optional(ORDERBY.suppress().setDebug(DEBUG) + Group(delimitedList(sortColumn)).setName("columns"))("orderby")
    )
)
selectStmt

SQLParser = selectStmt

# IGNORE SOME COMMENTS
oracleSqlComment = Literal("--") + restOfLine
mySqlComment = Literal("#") + restOfLine
SQLParser.ignore(oracleSqlComment | mySqlComment)

