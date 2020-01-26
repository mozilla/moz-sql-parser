# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Author: Kyle Lahnakoski (kyle@lahnakoski.com)
#

from __future__ import absolute_import, division, unicode_literals

import ast
import sys

from pyparsing import Combine, Forward, Group, Keyword, Literal, Optional, ParserElement, Regex, Word, ZeroOrMore, \
    alphanums, alphas, delimitedList, infixNotation, opAssoc, restOfLine

from moz_sql_parser.debugs import debug
from moz_sql_parser.keywords import AND, AS, ASC, BETWEEN, CASE, COLLATE_NOCASE, CROSS_JOIN, DESC, ELSE, END, FROM, \
    FULL_JOIN, FULL_OUTER_JOIN, GROUP_BY, HAVING, IN, INNER_JOIN, IS, IS_NOT, JOIN, LEFT_JOIN, LEFT_OUTER_JOIN, LIKE, \
    LIMIT, NOT_BETWEEN, NOT_IN, NOT_LIKE, OFFSET, ON, OR, ORDER_BY, RESERVED, RIGHT_JOIN, RIGHT_OUTER_JOIN, SELECT, \
    THEN, UNION, UNION_ALL, USING, WHEN, WHERE, binary_ops, unary_ops, WITH

ParserElement.enablePackrat()

# PYPARSING USES A LOT OF STACK SPACE
sys.setrecursionlimit(1500)

IDENT_CHAR = alphanums + "@_$"

KNOWN_OPS = [
    # https://www.sqlite.org/lang_expr.html
    Literal("||").setName("concat").setDebugActions(*debug),
    (
        Literal("*").setName("mul") |
        Literal("/").setName("div") |
        Literal("%").setName("mod")
    ).setDebugActions(*debug),
    (
        Literal("+").setName("add") |
        Literal("-").setName("sub")
    ).setDebugActions(*debug),
    Literal("&").setName("binary_and").setDebugActions(*debug),
    Literal("|").setName("binary_or").setDebugActions(*debug),
    (
        Literal(">=").setName("gte") |
        Literal("<=").setName("lte") |
        Literal("<").setName("lt") |
        Literal(">").setName("gt")
    ).setDebugActions(*debug),
    (
        Literal("==").setName("eq") |
        Literal("!=").setName("neq") |
        Literal("<>").setName("neq") |
        Literal("=").setName("eq")
    ).setDebugActions(*debug),

    (BETWEEN.setName("between").setDebugActions(*debug), AND),
    (NOT_BETWEEN.setName("not_between").setDebugActions(*debug), AND),
    IN.setName("in").setDebugActions(*debug),
    NOT_IN.setName("nin").setDebugActions(*debug),
    IS_NOT.setName("neq").setDebugActions(*debug),
    IS.setName("is").setDebugActions(*debug),
    LIKE.setName("like").setDebugActions(*debug),
    NOT_LIKE.setName("nlike").setDebugActions(*debug),
    AND.setName("and").setDebugActions(*debug),
    OR.setName("or").setDebugActions(*debug)
]

def to_json_operator(instring, tokensStart, retTokens):
    # ARRANGE INTO {op: params} FORMAT
    tok = retTokens[0]
    op = tok[1]
    clean_op = op.lower()
    clean_op = binary_ops.get(clean_op, clean_op)

    for o in KNOWN_OPS:
        if isinstance(o, tuple):
            # TRINARY OPS
            if o[0].matches(op):
                return {clean_op: [tok[0], tok[2], tok[4]]}
        elif o.matches(op):
            break
    else:
        if op == COLLATE_NOCASE.match:
            op = COLLATE_NOCASE.name
            return {op: tok[0]}
        else:
            raise Exception("not found")

    if clean_op == "eq":
        if tok[2] == "null":
            return {"missing": tok[0]}
        elif tok[0] == "null":
            return {"missing": tok[2]}
    elif clean_op == "neq":
        if tok[2] == "null":
            return {"exists": tok[0]}
        elif tok[0] == "null":
            return {"exists": tok[2]}
    elif clean_op == "is":
        if tok[2] == 'null':
            return {"missing": tok[0]}
        else:
            return {"exists": tok[0]}


    operands = [tok[0], tok[2]]
    simple = {clean_op: operands}
    if len(tok) <= 3:
        return simple

    if clean_op in {"add", "mul", "and", "or"}:
        # ACCUMULATE SUBSEQUENT, IDENTICAL OPS
        for i in range(3, len(tok), 2):
            if tok[i] != op:
                return to_json_operator(None, None, [[simple] + tok[i:]])
            else:
                operands.append(tok[i+1])
        return simple
    else:
        # SIMPLE BINARY
        return to_json_operator(None, None, [[simple] + tok[3:]])


def to_json_call(instring, tokensStart, retTokens):
    # ARRANGE INTO {op: params} FORMAT
    tok = retTokens
    op = tok.op.lower()
    op = unary_ops.get(op, op)

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

    if tok.using:
        output['using'] = tok.using
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
    else:
        sources = [unions[i] for i in range(0, len(unions), 2)]
        operators = [unions[i] for i in range(1, len(unions), 2)]
        op = operators[0].lower().replace(" ", "_")
        if any(o.lower().replace(" ", "_") != op for o in operators[1:]):
            raise Exception("Expecting all \"union all\" or all \"union\", not some combination")

        if not tok.get('orderby') and not tok.get('limit'):
            return {op: sources}
        else:
            output = {"from": {op: sources}}

    if tok.get('orderby'):
        output["orderby"] = tok.get('orderby')
    if tok.get('limit'):
        output["limit"] = tok.get('limit')
    return output


def to_with_clause(instring, tokensStart, retTokens):
    tok = retTokens[0]
    query = tok['query'][0]
    if tok['with']:
        assignments = [
            {"name": w.name, "value": w.value[0]}
            for w in tok['with']
        ]
        query['with'] = assignments
    return query


def unquote(instring, tokensStart, retTokens):
    val = retTokens[0]
    if val.startswith("'") and val.endswith("'"):
        val = "'"+val[1:-1].replace("''", "\\'")+"'"
        # val = val.replace(".", "\\.")
    elif val.startswith('"') and val.endswith('"'):
        val = '"'+val[1:-1].replace('""', '\\"')+'"'
        # val = val.replace(".", "\\.")
    elif val.startswith('`') and val.endswith('`'):
        val = '"' + val[1:-1].replace("``","`") + '"'
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
mysqlidentString = Regex(r'\`(\`\`|\\.|[^`])*\`').addParseAction(unquote)
ident = Combine(~RESERVED + (delimitedList(Literal("*") | identString | mysqlidentString | Word(IDENT_CHAR), delim=".", combine=True))).setName("identifier")

# EXPRESSIONS
expr = Forward()

# CASE
case = (
    CASE +
    Group(ZeroOrMore((WHEN + expr("when") + THEN + expr("then")).addParseAction(to_when_call)))("case") +
    Optional(ELSE + expr("else")) +
    END
).addParseAction(to_case_call)

ordered_sql = Forward()


call_function = (
        ident.copy()("op").setName("function name").setDebugActions(*debug) +
        Literal("(").suppress() +
        Optional(ordered_sql | Group(delimitedList(expr)))("params") +
        Literal(")").suppress()
).addParseAction(to_json_call).setDebugActions(*debug)

compound = (
    (Keyword("not", caseless=True)("op").setDebugActions(*debug) + expr("params")).addParseAction(to_json_call) |
    (Keyword("distinct", caseless=True)("op").setDebugActions(*debug) + expr("params")).addParseAction(to_json_call) |
    Keyword("null", caseless=True).setName("null").setDebugActions(*debug) |
    case |
    (Literal("(").suppress() + ordered_sql + Literal(")").suppress()) |
    (Literal("(").suppress() + Group(delimitedList(expr)) + Literal(")").suppress()) |
    realNum.setName("float").setDebugActions(*debug) |
    intNum.setName("int").setDebugActions(*debug) |
    (Literal("~")("op").setDebugActions(*debug) + expr("params")).addParseAction(to_json_call) |
    (Literal("-")("op").setDebugActions(*debug) + expr("params")).addParseAction(to_json_call) |
    sqlString.setName("string").setDebugActions(*debug) |
    call_function |
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
            COLLATE_NOCASE,
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

table_source = (
    (
        (Literal("(").suppress() + ordered_sql + Literal(")").suppress()).setDebugActions(*debug) |
        call_function
    )("value").setName("table source").setDebugActions(*debug) +
    Optional(
        Optional(AS) +
        ident("name").setName("table alias").setDebugActions(*debug)
    )
    |
    (
        ident("value").setName("table name").setDebugActions(*debug) +
        Optional(AS) +
        ident("name").setName("table alias").setDebugActions(*debug)
    )
    |
    ident.setName("table name").setDebugActions(*debug)
)

join = (
    (CROSS_JOIN | FULL_JOIN | FULL_OUTER_JOIN | INNER_JOIN | JOIN | LEFT_JOIN | LEFT_OUTER_JOIN | RIGHT_JOIN | RIGHT_OUTER_JOIN)("op") +
    Group(table_source)("join") +
    Optional((ON + expr("on")) | (USING + expr("using")))
).addParseAction(to_join_call)

sortColumn = expr("value").setName("sort1").setDebugActions(*debug) + Optional(DESC("sort") | ASC("sort")) | \
             expr("value").setName("sort2").setDebugActions(*debug)

unordered_sql = Group(
    SELECT.suppress().setDebugActions(*debug) + delimitedList(selectColumn)("select") +
    Optional(
        (FROM.suppress().setDebugActions(*debug) + delimitedList(Group(table_source)) + ZeroOrMore(join))("from") +
        Optional(WHERE.suppress().setDebugActions(*debug) + expr.setName("where"))("where") +
        Optional(GROUP_BY.suppress().setDebugActions(*debug) + delimitedList(Group(selectColumn))("groupby").setName("groupby")) +
        Optional(HAVING.suppress().setDebugActions(*debug) + expr("having").setName("having")) +
        Optional(LIMIT.suppress().setDebugActions(*debug) + expr("limit")) +
        Optional(OFFSET.suppress().setDebugActions(*debug) + expr("offset"))
    )
)

ordered_sql << Group(
    Group(Group(
        unordered_sql +
        ZeroOrMore((UNION_ALL | UNION) + unordered_sql)
    )("union"))("from") +
    Optional(ORDER_BY.suppress().setDebugActions(*debug) + delimitedList(Group(sortColumn))("orderby").setName("orderby")) +
    Optional(LIMIT.suppress().setDebugActions(*debug) + expr("limit")) +
    Optional(OFFSET.suppress().setDebugActions(*debug) + expr("offset"))
).addParseAction(to_union_call)

statement = Group(Group(Optional(
    WITH.suppress().setDebugActions(*debug) +
    delimitedList(
        Group(
            ident("name").setDebugActions(*debug) +
            AS.suppress().setDebugActions(*debug) +
            Literal("(").suppress().setDebugActions(*debug) +
            ordered_sql("value").setDebugActions(*debug) +
            Literal(")").suppress().setDebugActions(*debug)
        )
    )
))("with") + ordered_sql("query")).addParseAction(to_with_clause)

SQLParser = statement

# IGNORE SOME COMMENTS
oracleSqlComment = Literal("--") + restOfLine
mySqlComment = Literal("#") + restOfLine
SQLParser.ignore(oracleSqlComment | mySqlComment)
