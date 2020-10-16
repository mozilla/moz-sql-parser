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

from mo_future import is_text, text
from mo_math import is_number

from mo_parsing import Combine, Forward, Group, Keyword, Literal, Optional, Regex, Word, ZeroOrMore, \
    alphanums, delimitedList, infixNotation, restOfLine, RIGHT_ASSOC, LEFT_ASSOC, ParseResults
from mo_parsing.engine import Engine
from moz_sql_parser.debugs import debug
from moz_sql_parser.keywords import AS, ASC, CASE, CROSS_JOIN, DESC, ELSE, END, FROM, \
    FULL_JOIN, FULL_OUTER_JOIN, GROUP_BY, HAVING, INNER_JOIN, JOIN, LEFT_JOIN, LEFT_OUTER_JOIN, LIMIT, OFFSET, ON, \
    ORDER_BY, RIGHT_JOIN, RIGHT_OUTER_JOIN, SELECT, \
    THEN, UNION, UNION_ALL, USING, WHEN, WHERE, unary_ops, WITH, durations, NEG, NOT, KNOWN_OPS, RESERVED, BINARY_NOT, \
    binary_ops, NULL, NOCASE

engine = Engine().use()
engine.set_debug_actions(*debug)

# PYPARSING USES A LOT OF STACK SPACE
sys.setrecursionlimit(3000)

IDENT_CHAR = alphanums + "@_$"


def to_json_operator(tokens):
    # ARRANGE INTO {op: params} FORMAT
    length = len(tokens.tokens)
    if length ==2:
        op = tokens.tokens[0].type.parser_name
        if op == "neg" and is_number(tokens[1]):
            return -tokens[1]
        return {op: tokens[1]}
    elif length == 5:
        return {tokens.tokens[1].type.parser_name: [tokens[0], tokens[2], tokens[4]]}

    op = tokens[1]
    if not isinstance(op, text):
        op = op.type.parser_name
    op = binary_ops.get(op, op)
    if op == "eq":
        if tokens[2] == "null":
            return {"missing": tokens[0]}
        elif tokens[0] == "null":
            return {"missing": tokens[2]}
    elif op == "neq":
        if tokens[2] == "null":
            return {"exists": tokens[0]}
        elif tokens[0] == "null":
            return {"exists": tokens[2]}
    elif op == "is":
        if tokens[2] == 'null':
            return {"missing": tokens[0]}
        else:
            return {"exists": tokens[0]}
    elif op == "is_not":
        if tokens[2] == 'null':
            return {"exists": tokens[0]}
        else:
            return {"missing": tokens[0]}

    operands = [tokens[0], tokens[2]]
    simple = {op: operands}

    if op in {"add", "mul", "and", "or"}:
        operand0 = operands[0]
        if isinstance(operand0, ParseResults):
            prefix = operand0.tokens[0].get(op)
            if prefix:
                # ACCUMULATE SUBSEQUENT, IDENTICAL OPS
                return {op: prefix + [operands[1]]}
    return simple


def to_json_call(tokens):
    # ARRANGE INTO {op: params} FORMAT
    tok = tokens
    op = tok['op'].lower()
    op = binary_ops.get(op, op)

    params = tok['params']
    if not params:
        params = {}
    elif isinstance(params, list) and len(params) == 1:
        params = params[0]
    elif isinstance(params, ParseResults) and params.length() == 1:
        params = params[0]

    return {op: params}


def to_case_call(tokens):
    cases = list(tokens['case'])
    elze = tokens["else"]
    if elze:
        cases.append(elze)
    return {"case": cases}


def to_interval_call(tokens):
    # ARRANGE INTO {interval: params} FORMAT
    return {"interval": [tokens['count'], tokens['duration'][:-1]]}


def to_when_call(tokens):
    tok = tokens
    return {"when": tok['when'], "then": tok['then']}


def to_join_call(tokens):
    tok = tokens
    op = tok['op'].type.parser_name
    if tok['join']['name']:
        output = {op: {"name": tok['join']['name'], "value": tok['join']['value']}}
    else:
        output = {op: tok['join']}

    output['on'] = tok['on']
    output['using'] = tok['using']
    return output


def to_select_call(tokens):
    if tokens['value'][0][0] == '*':
        return ['*']


def to_union_call(tokens):
    unions = list(tokens['from']['union'])
    if len(unions) == 1:
        output = unions[0]
    else:
        sources = [unions[i] for i in range(0, len(unions), 2)]
        operators = [unions[i] for i in range(1, len(unions), 2)]
        if is_text(operators[0]):
            op = operators[0]
        else:
            op = operators[0].type.parser_name
        if any(o.type.parser_name != op for o in operators[1:]):
            raise Exception("Expecting all \"union all\" or all \"union\", not some combination")

        if not tokens['orderby'] and not tokens['limit']:
            return {op: sources}
        else:
            output = {"from": {op: sources}}

    output["orderby"] = tokens['orderby']
    output["limit"] = tokens['limit']
    return [output]


def to_with_clause(tokens):
    tok = tokens
    query = tok['query'][0]
    if tok['with']:
        assignments = [
            {"name": w['name'], "value": w['value'][0]}
            for w in tok['with']
        ]
        query['with'] = assignments
    return query


def unquote(tokens):
    val = tokens[0]
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


def to_string(tokens):
    val = tokens[0]
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
    ident("op") +
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
    NULL |
    NOCASE |
    (Keyword("distinct", caseless=True)("op") + expr("params")).addParseAction(to_json_call) |
    (Keyword("date", caseless=True)("op") + sqlString("params")).addParseAction(to_json_call) |
    interval |
    case |
    (Literal("(").suppress() + ordered_sql + Literal(")").suppress()) |
    (Literal("(").suppress() + Group(delimitedList(expr)) + Literal(")").suppress()) |
    realNum.set_parser_name("float") |
    intNum.set_parser_name("int") |
    sqlString.set_parser_name("string") |
    call_function |
    ident
)
expr << Group(infixNotation(
    compound,
    [
        (
            o,
            1 if o in unary_ops else (3 if isinstance(o, tuple) else 2),
            RIGHT_ASSOC if o in unary_ops else LEFT_ASSOC,
            to_json_operator
        )
        for o in KNOWN_OPS
    ]
).set_parser_name("expression"))

# SQL STATEMENT
selectColumn = Group(
    Group(expr).set_parser_name("expression1")("value") + Optional(Optional(AS) + ident("name").set_parser_name("column_name1")) |
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
        Optional(WHERE + expr("where")) +
        Optional(GROUP_BY + delimitedList(Group(selectColumn))("groupby")) +
        Optional(HAVING + expr("having")) +
        Optional(LIMIT + expr("limit")) +
        Optional(OFFSET + expr("offset"))
    )
)

ordered_sql << Group(
    Group(Group(
        unordered_sql +
        ZeroOrMore((UNION_ALL | UNION) + unordered_sql)
    )("union"))("from") +
    Optional(ORDER_BY + delimitedList(Group(sortColumn))("orderby")) +
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
