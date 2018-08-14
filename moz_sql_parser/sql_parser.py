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
import sys

from pyparsing import CaselessLiteral, Word, delimitedList, Optional, Combine, Group, alphas, nums, alphanums, Forward, restOfLine, Keyword, Literal, ParserElement, infixNotation, opAssoc, Regex, MatchFirst, ZeroOrMore, _ustr

ParserElement.enablePackrat()

# THE PARSING DEPTH IS NASTY
sys.setrecursionlimit(1500)


DEBUG = False
END = None

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


keywords = [
    "and",
    "as",
    "asc",
    "between",
    "case",
    "collate nocase",
    "cross join",
    "desc",
    "else",
    "end",
    "from",
    "group by",
    "having",
    "in",
    "inner join",
    "is",
    "join",
    "like",
    "limit",
    "like",
    "on",
    "or",
    "order by",
    "select",
    "then",
    "union",
    "when",
    "where",
    "with"
]
locs = locals()
reserved = []
for k in keywords:
    name = k.upper().replace(" ", "")
    locs[name] = value = Keyword(k, caseless=True).setName(k.lower()).setDebugActions(*debug)
    reserved.append(value)
RESERVED = MatchFirst(reserved)

KNOWN_OPS = [
    (BETWEEN, AND),
    Literal("||").setName("concat").setDebugActions(*debug),
    Literal("*").setName("mult").setDebugActions(*debug),
    Literal("/").setName("div").setDebugActions(*debug),
    Literal("+").setName("add").setDebugActions(*debug),
    Literal("-").setName("sub").setDebugActions(*debug),
    Literal("<>").setName("neq").setDebugActions(*debug),
    Literal(">").setName("gt").setDebugActions(*debug),
    Literal("<").setName("lt").setDebugActions(*debug),
    Literal(">=").setName("gte").setDebugActions(*debug),
    Literal("<=").setName("lte").setDebugActions(*debug),
    LIKE.setName("like").setDebugActions(*debug),
    IN.setName("in").setDebugActions(*debug),
    IS.setName("is").setDebugActions(*debug),
    Literal("=").setName("eq").setDebugActions(*debug),
    Literal("==").setName("eq").setDebugActions(*debug),
    Literal("!=").setName("neq").setDebugActions(*debug),
    OR.setName("or").setDebugActions(*debug),
    AND.setName("and").setDebugActions(*debug),
    LIKE.setName("like").setDebugActions(*debug)
]


def to_json_operator(instring, tokensStart, retTokens):
    # ARRANGE INTO {op: params} FORMAT
    tok = retTokens[0]
    for o in KNOWN_OPS:
        if isinstance(o, tuple):
            if o[0].match == tok[1]:
                op = o[0].name
                break
        elif o.match == tok[1]:
            op = o.name
            break
    else:
        if tok[1] == COLLATENOCASE.match:
            op = COLLATENOCASE.name
            return {op: tok[0]}
        else:
            raise "not found"

    if op == "eq":
        if tok[2] == "null":
            return {"missing": tok[0]}
        elif tok[0] == "null":
            return {"missing": tok[2]}
    elif op == "neq":
        if tok[2] == "null":
            return {"exists": tok[0]}
        elif tok[0] == "null":
            return {"exists": tok[2]}
    elif op == "is":
        if tok[2] == 'null':
            return {"missing": tok[0]}
        else:
            return {"exists": tok[0]}

    if op == 'like':
        return get_like_op(tok, op)

    return {op: [tok[i * 2] for i in range(int((len(tok) + 1) / 2))]}

def get_like_op(tok, op):
    val_list = []
    for i in range(int((len(tok) + 1) / 2)):
        el = tok[i * 2]
        if 'literal' in el:
            val_like = el['literal']
            if val_like.find('%') == 0:
                if val_like.find('%', 1) == -1:
                    val_list.append({'symbol': 'left'})
                else:
                    val_list.append({'symbol': 'both'})
            else:
                val_list.append({'symbol': 'right'})
            val_list.append(val_like.strip('%'))

        else:
            val_list.append({'literal': tok[i * 2]})
    return {op: val_list}



def to_json_call(instring, tokensStart, retTokens):
    # ARRANGE INTO {op: params} FORMAT
    tok = retTokens
    op = tok.op.lower()

    if op == "-":
        op = "neg"

    params = tok.params
    if not params:
        params = None
    elif len(params) == 1:
        params = params[0]
    return {op: params}


def to_case_call(instring, tokensStart, retTokens):
    tok = retTokens
    cases = list(tok.case)
    elze = getattr(tok, "else", None)
    if elze:
        cases.append(elze)
    return {"case": cases}


def to_when_call(instring, tokensStart, retTokens):
    tok = retTokens
    return {"when": tok.when, "then":tok.then}


def to_join_call(instring, tokensStart, retTokens):
    tok = retTokens

    if tok.join.name:
        output = {tok.op: {"name": tok.join.name, "value": tok.join.value}}
    else:
        output = {tok.op: tok.join}

    if tok.on:
        output['on'] = tok.on
    return output


def to_select_call(instring, tokensStart, retTokens):
    tok = retTokens[0].asDict()

    if tok.get('value')[0][0] == '*':
        return '*'
    else:
        return tok


def to_union_call(instring, tokensStart, retTokens):
    tok = retTokens[0].asDict()
    unions = tok['from']['union']
    if len(unions) == 1:
        output = unions[0]
        if tok.get('orderby'):
            output["orderby"] = tok.get('orderby')
        if tok.get('limit'):
            output["limit"] = tok.get('limit')
        return output
    else:
        if not tok.get('orderby') and not tok.get('limit'):
            return tok['from']
        else:
            return {
                "from": {"union": unions},
                "orderby": tok.get('orderby') if tok.get('orderby') else None,
                "limit": tok.get('limit') if tok.get('limit') else None
            }


def unquote(instring, tokensStart, retTokens):
    val = retTokens[0]
    if val.startswith("'") and val.endswith("'"):
        val = "'"+val[1:-1].replace("''", "\\'")+"'"
        # val = val.replace(".", "\\.")
    elif val.startswith('"') and val.endswith('"'):
        val = '"'+val[1:-1].replace('""', '\\"')+'"'
        # val = val.replace(".", "\\.")
    elif val.startswith("+"):
        val = val[1:]
    un = ast.literal_eval(val)
    return un


def to_string(instring, tokensStart, retTokens):
    val = retTokens[0]
    val = "'"+val[1:-1].replace("''", "\\'")+"'"
    return {"literal": ast.literal_eval(val)}

# NUMBERS
realNum = Regex(r"[+-]?(\d+\.\d*|\.\d+)([eE][+-]?\d+)?").addParseAction(unquote)
intNum = Regex(r"[+-]?\d+([eE]\+?\d+)?").addParseAction(unquote)

# STRINGS, NUMBERS, VARIABLES
sqlString = Regex(r"\'(\'\'|\\.|[^'])*\'").addParseAction(to_string)
identString = Regex(r'\"(\"\"|\\.|[^"])*\"').addParseAction(unquote)
ident = Combine(~RESERVED + (delimitedList(Literal("*") | Word(alphas + "_", alphanums + "_$") | identString, delim=".", combine=True))).setName("identifier")

# EXPRESSIONS
expr = Forward()

# CASE
case = (
    CASE +
    Group(ZeroOrMore((WHEN + expr("when") + THEN + expr("then")).addParseAction(to_when_call)))("case") +
    Optional(ELSE + expr("else")) +
    END
).addParseAction(to_case_call)

selectStmt = Forward()
compound = (
    (Keyword("not", caseless=True)("op").setDebugActions(*debug) + expr("params")).addParseAction(to_json_call) |
    (Keyword("distinct", caseless=True)("op").setDebugActions(*debug) + expr("params")).addParseAction(to_json_call) |
    Keyword("null", caseless=True).setName("null").setDebugActions(*debug) |
    case |
    (Literal("(").setDebugActions(*debug).suppress() + selectStmt + Literal(")").suppress()) |
    (Literal("(").setDebugActions(*debug).suppress() + Group(delimitedList(expr)) + Literal(")").suppress()) |
    realNum.setName("float").setDebugActions(*debug) |
    intNum.setName("int").setDebugActions(*debug) |
    (Literal("-")("op").setDebugActions(*debug) + expr("params")).addParseAction(to_json_call) |
    sqlString.setName("string").setDebugActions(*debug) |
    (
        Word(alphas)("op").setName("function name").setDebugActions(*debug) +
        Literal("(").setName("func_param").setDebugActions(*debug) +
        Optional(selectStmt | Group(delimitedList(expr)))("params") +
        ")"
    ).addParseAction(to_json_call).setDebugActions(*debug) |
    ident.copy().setName("variable").setDebugActions(*debug)
)
expr << Group(infixNotation(
    compound,
    [
        (
            o,
            3 if isinstance(o, tuple) else 2,
            opAssoc.LEFT,
            to_json_operator
        )
        for o in KNOWN_OPS
    ]+[
        (
            COLLATENOCASE,
            1,
            opAssoc.LEFT,
            to_json_operator
        )
    ]
).setName("expression").setDebugActions(*debug))

# SQL STATEMENT
selectColumn = Group(
    Group(expr).setName("expression1")("value").setDebugActions(*debug) + Optional(Optional(AS) + ident.copy().setName("column_name1")("name").setDebugActions(*debug)) |
    Literal('*')("value").setDebugActions(*debug)
).setName("column").addParseAction(to_select_call)


tableName = (
    ident("value").setName("table name").setDebugActions(*debug) +
    Optional(AS) +
    ident("name").setName("table alias").setDebugActions(*debug) |
    ident.setName("table name").setDebugActions(*debug)
)

join = ((CROSSJOIN | INNERJOIN | JOIN)("op") + Group(tableName)("join") + Optional(ON + expr("on"))).addParseAction(to_join_call)

sortColumn = expr("value").setName("sort1").setDebugActions(*debug) + Optional(DESC("sort") | ASC("sort")) | \
             expr("value").setName("sort2").setDebugActions(*debug)

# define SQL tokens
selectStmt << Group(
    Group(Group(
        delimitedList(
            Group(
                SELECT.suppress().setDebugActions(*debug) + delimitedList(selectColumn)("select") +
                Optional(
                    FROM.suppress().setDebugActions(*debug) + (delimitedList(Group(tableName)) + ZeroOrMore(join))("from") +
                    Optional(WHERE.suppress().setDebugActions(*debug) + expr.setName("where"))("where") +
                    Optional(GROUPBY.suppress().setDebugActions(*debug) + delimitedList(Group(selectColumn))("groupby").setName("groupby")) +
                    Optional(HAVING.suppress().setDebugActions(*debug) + expr("having").setName("having")) +
                    Optional(LIMIT.suppress().setDebugActions(*debug) + expr("limit"))
                )
            ),
            delim=UNION
        )
    )("union"))("from") +
    Optional(ORDERBY.suppress().setDebugActions(*debug) + delimitedList(Group(sortColumn))("orderby").setName("orderby")) +
    Optional(LIMIT.suppress().setDebugActions(*debug) + expr("limit"))
).addParseAction(to_union_call)


SQLParser = selectStmt

# IGNORE SOME COMMENTS
oracleSqlComment = Literal("--") + restOfLine
mySqlComment = Literal("#") + restOfLine
SQLParser.ignore(oracleSqlComment | mySqlComment)

