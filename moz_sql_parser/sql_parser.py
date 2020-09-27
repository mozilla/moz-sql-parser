# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Contact: Kyle Lahnakoski (kyle@lahnakoski.com)
#

from __future__ import absolute_import, division, unicode_literals

import ast
import sys

from mo_parsing import Combine, Forward, Group, Keyword, Literal, Optional, Regex, Word, ZeroOrMore, \
    alphanums, delimitedList, infixNotation, opAssoc, restOfLine
from mo_parsing.engine import Engine
from moz_sql_parser.debugs import debug
from moz_sql_parser.keywords import AND, AS, ASC, BETWEEN, CASE, COLLATE_NOCASE, CROSS_JOIN, DESC, ELSE, END, FROM, \
    FULL_JOIN, FULL_OUTER_JOIN, GROUP_BY, HAVING, IN, INNER_JOIN, IS, IS_NOT, JOIN, LEFT_JOIN, LEFT_OUTER_JOIN, LIKE, \
    LIMIT, NOT_BETWEEN, NOT_IN, NOT_LIKE, OFFSET, ON, OR, ORDER_BY, RESERVED, RIGHT_JOIN, RIGHT_OUTER_JOIN, SELECT, \
    THEN, UNION, UNION_ALL, USING, WHEN, WHERE, binary_ops, unary_ops, WITH, durations

engine = Engine().use()
engine.set_debug_actions(*debug)

# PYPARSING USES A LOT OF STACK SPACE
sys.setrecursionlimit(3000)

IDENT_CHAR = alphanums + "@_$"

KNOWN_OPS = [
    # https://www.sqlite.org/lang_expr.html
    Literal("||").set_parser_name("concat"),
    (
        Literal("*").set_parser_name("mul") |
        Literal("/").set_parser_name("div") |
        Literal("%").set_parser_name("mod")
    ),
    (
        Literal("+").set_parser_name("add") |
        Literal("-").set_parser_name("sub")
    ),
    Literal("&").set_parser_name("binary_and"),
    Literal("|").set_parser_name("binary_or"),
    (
        Literal(">=").set_parser_name("gte") |
        Literal("<=").set_parser_name("lte") |
        Literal("<").set_parser_name("lt") |
        Literal(">").set_parser_name("gt")
    ),
    (
        Literal("==").set_parser_name("eq") |
        Literal("!=").set_parser_name("neq") |
        Literal("<>").set_parser_name("neq") |
        Literal("=").set_parser_name("eq")
    ),

    (BETWEEN.set_parser_name("between"), AND),
    (NOT_BETWEEN.set_parser_name("not_between"), AND),
    IN.set_parser_name("in"),
    NOT_IN.set_parser_name("nin"),
    IS_NOT.set_parser_name("neq"),
    IS.set_parser_name("is"),
    LIKE.set_parser_name("like"),
    NOT_LIKE.set_parser_name("nlike"),
    AND.set_parser_name("and"),
    OR.set_parser_name("or")
]

def to_json_operator(instring, tokensStart, retTokens):
    # ARRANGE INTO {op: params} FORMAT
    tok = retTokens
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


def to_date_call(instring, tokensStart, retTokens):
    return {"date": retTokens.params}


def to_interval_call(instring, tokensStart, retTokens):
    # ARRANGE INTO {interval: params} FORMAT
    return {"interval": [retTokens['count'], retTokens['duration'][:-1]]}


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
    tok = retTokens[0]

    if tok['value'][0][0] == '*':
        return ['*']
    else:
        return tok


def to_union_call(instring, tokensStart, retTokens):
    tok = retTokens
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
ident = Combine(~RESERVED + (delimitedList(Literal("*") | identString | mysqlidentString | Word(IDENT_CHAR), separator=".", combine=True))).set_parser_name("identifier")

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
    ident.copy()("op").set_parser_name("function name") +
    Literal("(").suppress() +
    Optional(ordered_sql | Group(delimitedList(expr)))("params") +
    Literal(")").suppress()
).addParseAction(to_json_call)


def _or(values):
    output = values[0]
    for v in values[1:]:
        output |= v
    return output


interval = (
    Keyword("interval", caseless=True).suppress() +
    (realNum | intNum)("count") +
    _or([Keyword(d, caseless=True)("duration") for d in durations])
).addParseAction(to_interval_call)

compound = (
    Keyword("null", caseless=True).set_parser_name("null") |
    (Keyword("not", caseless=True)("op") + expr("params")).addParseAction(to_json_call) |
    (Keyword("distinct", caseless=True)("op") + expr("params")).addParseAction(to_json_call) |
    (Keyword("date", caseless=True) + sqlString("params")).addParseAction(to_date_call) |
    interval |
    case |
    (Literal("(").suppress() + ordered_sql + Literal(")").suppress()) |
    (Literal("(").suppress() + Group(delimitedList(expr)) + Literal(")").suppress()) |
    realNum.set_parser_name("float") |
    intNum.set_parser_name("int") |
    (Literal("~")("op") + expr("params")).addParseAction(to_json_call) |
    (Literal("-")("op") + expr("params")).addParseAction(to_json_call) |
    sqlString.set_parser_name("string") |
    call_function |
    ident.copy().set_parser_name("variable")
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
).set_parser_name("expression"))

# SQL STATEMENT
selectColumn = Group(
    Group(expr).set_parser_name("expression1")("value") + Optional(Optional(AS) + ident.copy().set_parser_name("column_name1")("name")) |
    Literal('*')("value")
).set_parser_name("column").addParseAction(to_select_call)

table_source = (
    (
        (Literal("(").suppress() + ordered_sql + Literal(")").suppress()) |
        call_function
    )("value").set_parser_name("table source") +
    Optional(
        Optional(AS) +
        ident("name").set_parser_name("table alias")
    )
    |
    (
        ident("value").set_parser_name("table name") +
        Optional(AS) +
        ident("name").set_parser_name("table alias")
    )
    |
    ident.set_parser_name("table name")
)

join = (
    (CROSS_JOIN | FULL_JOIN | FULL_OUTER_JOIN | INNER_JOIN | JOIN | LEFT_JOIN | LEFT_OUTER_JOIN | RIGHT_JOIN | RIGHT_OUTER_JOIN)("op") +
    Group(table_source)("join") +
    Optional((ON + expr("on")) | (USING + expr("using")))
).addParseAction(to_join_call)

sortColumn = expr("value").set_parser_name("sort1") + Optional(DESC("sort") | ASC("sort")) | \
             expr("value").set_parser_name("sort2")

unordered_sql = Group(
    SELECT + delimitedList(selectColumn)("select") +
    Optional(
        (FROM + delimitedList(Group(table_source)) + ZeroOrMore(join))("from") +
        Optional(WHERE + expr.set_parser_name("where"))("where") +
        Optional(GROUP_BY + delimitedList(Group(selectColumn))("groupby").set_parser_name("groupby")) +
        Optional(HAVING + expr("having").set_parser_name("having")) +
        Optional(LIMIT + expr("limit")) +
        Optional(OFFSET + expr("offset"))
    )
)

ordered_sql << Group(
    Group(Group(
        unordered_sql +
        ZeroOrMore((UNION_ALL | UNION) + unordered_sql)
    )("union"))("from") +
    Optional(ORDER_BY + delimitedList(Group(sortColumn))("orderby").set_parser_name("orderby")) +
    Optional(LIMIT + expr("limit")) +
    Optional(OFFSET + expr("offset"))
).addParseAction(to_union_call)

statement = Group(Group(Optional(
    WITH +
    delimitedList(
        Group(
            ident("name") +
            AS +
            Literal("(").suppress() +
            ordered_sql("value") +
            Literal(")").suppress()
        )
    )
))("with") + ordered_sql("query")).addParseAction(to_with_clause)

SQLParser = statement

# IGNORE SOME COMMENTS
oracleSqlComment = Literal("--") + restOfLine
mySqlComment = Literal("#") + restOfLine

engine.add_ignore(oracleSqlComment)
engine.add_ignore(mySqlComment)

engine.release()
