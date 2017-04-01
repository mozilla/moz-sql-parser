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
    nums, alphanums, Forward, restOfLine, Keyword, Literal, ParserElement, infixNotation, opAssoc, Regex, MatchFirst, ZeroOrMore, Suppress

ParserElement.enablePackrat()
DEBUG = True
END = None

keywords = [
    "and",
    "as",
    "between",
    "case",
    "cross join",
    "desc",
    "else",
    "end",
    "from",
    "group by",
    "having",
    "in",
    "inner join",
    "join",
    "limit",
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
    locs[name] = value = Keyword(k, caseless=True).setName(k.lower())
    reserved.append(value)
RESERVED = MatchFirst(reserved)

KNOWN_OPS = [
    (BETWEEN, AND),
    Literal("*").setName("mult").setDebug(DEBUG),
    Literal("/").setName("div").setDebug(DEBUG),
    Literal("+").setName("add").setDebug(DEBUG),
    Literal("-").setName("sub").setDebug(DEBUG),
    Literal("=").setName("eq").setDebug(DEBUG),
    Literal("==").setName("eq").setDebug(DEBUG),
    Literal("!=").setName("neq").setDebug(DEBUG),
    Literal("<>").setName("neq").setDebug(DEBUG),
    Literal(">").setName("gt").setDebug(DEBUG),
    Literal("<").setName("lt").setDebug(DEBUG),
    Literal(">=").setName("gte").setDebug(DEBUG),
    Literal("<=").setName("lte").setDebug(DEBUG),
    IN.setName("in").setDebug(DEBUG),
    OR.setName("or").setDebug(DEBUG),
    AND.setName("and").setDebug(DEBUG),
    Literal("||").setName("concat").setDebug(DEBUG)
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
        raise "not found"
    return {op: [tok[i * 2] for i in range(int((len(tok) + 1) / 2))]}


def to_json_call(instring, tokensStart, retTokens):
    # ARRANGE INTO {op: params} FORMAT
    tok = retTokens
    op = tok.op.lower()
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

    output = {tok.op: tok.join}
    if tok.on:
        output['on'] = tok.on
    return output


def to_select_call(instring, tokensStart, retTokens):
    return retTokens


def to_union_call(instring, tokensStart, retTokens):
    if len(retTokens.union)==1:
        output = retTokens.union[0]
        output["orderby"] = retTokens.orderby if retTokens.orderby else None
        output["limit"] = retTokens.limit if retTokens.limit else None
        return output
    else:
        return {
            "from": {"union": retTokens.union},
            "orderby": retTokens.orderby,
            "limit": retTokens.limit
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
    return {"literal": ast.literal_eval(retTokens[0])}

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

# STRINGS, NUMBERS, VARIABLES
sqlString = Combine(Regex(r"\'(\'\'|\\.|[^'])*\'")).addParseAction(to_string)
identString = Combine(Regex(r'\"(\"\"|\\.|[^"])*\"')).addParseAction(unquote)
ident = Combine(~RESERVED + (delimitedList(Literal("*") | Word(alphas, alphanums + "_$") | identString, delim=".", combine=True))).setName("identifier")
primitive = realNum | intNum | sqlString | ident

# EXPRESSIONS
expr = Forward()

# CASE
case = (CASE + Group(ZeroOrMore((WHEN + expr("when") + THEN + expr("then")).addParseAction(to_when_call)))("case") + Optional(ELSE+expr("else")) + END).addParseAction(to_case_call)


selectStmt = Forward()
compound = (
    (Literal("-").setResultsValue("neg").setResultsName("op").setDebug(DEBUG) + expr("params")).addParseAction(to_json_call) |
    (Keyword("not", caseless=True).setResultsName("op").setDebug(DEBUG) + expr("params")).addParseAction(to_json_call) |
    (Keyword("distinct", caseless=True).setResultsName("op").setDebug(DEBUG) + expr("params")).addParseAction(to_json_call) |
    Keyword("null", caseless=True).setDebug(DEBUG) |
    case |
    (Literal("(").setDebug(DEBUG).suppress() + selectStmt + Literal(")").suppress()).addParseAction(to_select_call) |
    (Literal("(").setDebug(DEBUG).suppress() + Group(delimitedList(expr)) + Literal(")").suppress()) |
    realNum.setName("float").setDebug(DEBUG) |
    intNum.setName("int").setDebug(DEBUG) |
    sqlString.setName("string").setDebug(DEBUG) |
    (
        Word(alphas)("op").setName("function name").setDebug(DEBUG) +
        Literal("(").setName("func_param").setDebug(DEBUG) +
        Optional(selectStmt.addParseAction(to_select_call) | Group(delimitedList(expr)))("params") +
        ")"
    ).addParseAction(to_json_call).setDebug(DEBUG) |
    ident.copy().setName("variable").setDebug(DEBUG)
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
    ]
).setName("expression").setDebug(DEBUG))

# SQL STATEMENT
selectColumn = Group(
    Group(expr).setName("expression1")("value").setDebug(DEBUG) + Optional(Optional(AS) + ident.copy().setName("column_name1")("name").setDebug(DEBUG)) |
    Literal('*')("value").setDebug(DEBUG)
).setName("column")


tableName = ident("value").setName("table_name1").setDebug(DEBUG) + Optional(AS) + ident("name").setName("table_alias1").setDebug(DEBUG) | \
            ident.setName("table_name2").setDebug(DEBUG)

join = ((CROSSJOIN | INNERJOIN | JOIN)("op") + tableName("join") + Optional(ON + expr("on"))).addParseAction(to_join_call)

sortColumn = expr("value").setName("sort1").setDebug(DEBUG) + Optional(DESC("sort")) | \
             expr("value").setName("sort2").setDebug(DEBUG)

# define SQL tokens
selectStmt << (
    (
        delimitedList(
            Group(
                SELECT.suppress().setDebug(DEBUG) + delimitedList(selectColumn)("select") +
                Optional(
                    FROM.suppress().setDebug(DEBUG) + (delimitedList(Group(tableName)) + ZeroOrMore(join))("from") +
                    Optional(WHERE.suppress().setDebug(DEBUG) + expr.setName("where"))("where") +
                    Optional(GROUPBY.suppress().setDebug(DEBUG) + delimitedList(Group(selectColumn))("groupby").setName("groupby")) +
                    Optional(HAVING.suppress().setDebug(DEBUG) + expr("having").setName("having"))+
                    Optional(LIMIT.suppress().setDebug(DEBUG) + expr("limit"))
                )
            ).addParseAction(to_select_call),
            delim=UNION
        )
    )("union") +
    Optional(ORDERBY.suppress().setDebug(DEBUG) + delimitedList(Group(sortColumn))("orderby").setName("orderby")) +
    Optional(LIMIT.suppress().setDebug(DEBUG) + expr("limit"))
).addParseAction(to_union_call)


SQLParser = selectStmt

# IGNORE SOME COMMENTS
oracleSqlComment = Literal("--") + restOfLine
mySqlComment = Literal("#") + restOfLine
SQLParser.ignore(oracleSqlComment | mySqlComment)

