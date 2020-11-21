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

from mo_dots import is_data
from mo_future import text, number_types, binary_type
from mo_testing.fuzzytestcase import assertAlmostEqual

from mo_parsing import (
    Combine,
    Forward,
    Optional,
    Regex,
    Word,
    ZeroOrMore,
    OneOrMore,
    alphanums,
    delimitedList,
    infixNotation,
    restOfLine,
    RIGHT_ASSOC,
    LEFT_ASSOC,
    ParseResults,
)
from mo_parsing.engine import Engine
from mo_parsing.utils import is_number, listwrap
from moz_sql_parser.keywords import *

engine = Engine().use()

IDENT_CHAR = alphanums + "@_$"


def scrub(result):
    if result == None:
        return None
    elif isinstance(result, text):
        return result
    elif isinstance(result, binary_type):
        return result.decode("utf8")
    elif isinstance(result, number_types):
        return result
    elif isinstance(result, dict) and not result:
        return result
    elif isinstance(result, list):
        output = [rr for r in result for rr in [scrub(r)]]

        if not output:
            return None
        elif len(output) == 1:
            return output[0]
        else:
            return scrub_literal(output)
    else:
        # ATTEMPT A DICT INTERPRETATION
        kv_pairs = list(result.items())
        output = {
            k: vv
            for k, v in kv_pairs
            for vv in [scrub(v)]
            if vv != None
        }
        if output:
            return output
        temp = list(result)
        return scrub(temp)


def scrub_literal(candidate):
    # IF ALL MEMBERS OF A LIST ARE LITERALS, THEN MAKE THE LIST LITERAL
    if all(isinstance(r, number_types) for r in candidate):
        pass
    elif all(
        isinstance(r, number_types) or (is_data(r) and "literal" in r.keys())
        for r in candidate
    ):
        candidate = {"literal": [r["literal"] if is_data(r) else r for r in candidate]}
    return candidate


def to_json_operator(tokens):
    # ARRANGE INTO {op: params} FORMAT
    length = len(tokens.tokens)
    if length == 2:
        # UNARY OPERATOR
        op = tokens.tokens[0].type.parser_name
        if op == "neg" and is_number(tokens[1]):
            return -tokens[1]
        return {op: tokens[1]}
    elif length == 5:
        # TRINARY OPERATOR
        return {tokens.tokens[1].type.parser_name: [tokens[0], tokens[2], tokens[4]]}

    op = tokens[1]
    if not isinstance(op, text):
        op = op.type.parser_name
    op = binary_ops.get(op, op)
    if op == "eq":
        if tokens[2] == None:
            return {"missing": tokens[0]}
        elif tokens[0] == "null":
            return {"missing": tokens[2]}
    elif op == "neq":
        if tokens[2] == None:
            return {"exists": tokens[0]}
        elif tokens[0] == "null":
            return {"exists": tokens[2]}
    elif op == "is":
        if tokens[2] == None:
            return {"missing": tokens[0]}
        else:
            return {"exists": tokens[0]}
    elif op == "is_not":
        if tokens[2] == None:
            return {"exists": tokens[0]}
        else:
            return {"missing": tokens[0]}

    operands = [tokens[0], tokens[2]]
    binary_op = {op: operands}

    if op in {"add", "mul", "and", "or"}:
        # ASSOCIATIVE OPERATORS
        acc = []
        for operand in operands:
            if isinstance(operand, ParseResults):
                operand = operand[0]
                if isinstance(operand, list):
                    acc.append(operand)
                    continue
                prefix = operand.get(op)
                if prefix:
                    acc.extend(prefix)
                    continue
                else:
                    acc.append(operand)
            else:
                acc.append(operand)
        binary_op = {op: acc}
    return binary_op


def to_tuple_call(tokens):
    # IS THIS ONE VALUE IN (), OR MANY?
    if tokens.length() == 1:
        return [scrub(tokens)]
    return [scrub_literal(scrub(tokens))]


def to_json_call(tokens):
    # ARRANGE INTO {op: params} FORMAT
    op = tokens["op"].lower()
    op = binary_ops.get(op, op)

    params = scrub(tokens["params"])
    if not params:
        params = {}
    # elif isinstance(params, list) and len(params) == 1:
    #     params = params[0]
    # elif isinstance(params, ParseResults) and params.length() == 1:
    #     params = params[0]

    return ParseResults(tokens.type, tokens.start, tokens.end, [{op: params}])


def to_case_call(tokens):
    cases = list(tokens["case"])
    elze = tokens["else"]
    if elze:
        cases.append(elze)
    return {"case": cases}


def to_when_call(tokens):
    tok = tokens
    return {"when": tok["when"], "then": tok["then"]}


def to_join_call(tokens):
    op = " ".join(listwrap(scrub(tokens["op"])))
    if tokens["join"]["name"]:
        output = {op: {
            "name": tokens["join"]["name"],
            "value": tokens["join"]["value"],
        }}
    else:
        output = {op: tokens["join"]}

    output["on"] = tokens["on"]
    output["using"] = tokens["using"]
    return output


def to_alias(tokens):
    cols = scrub(tokens['col'])
    name = scrub(tokens[0])
    if cols:
        return {name: cols}
    return name


def to_select_call(tokens):
    if tokens["value"][0][0] == "*":
        return ["*"]


def to_union_call(tokens):
    unions = list(tokens["union"])
    if len(unions) == 1:
        output = scrub(unions[0].tokens)  # REMOVE THE Group()
    else:
        sources = scrub([unions[i] for i in range(0, len(unions), 2)])
        operators = [unions[i] for i in range(1, len(unions), 2)]
        op = "_".join(listwrap(scrub(operators)))

        if not tokens["orderby"] and not tokens["offset"] and not tokens["limit"]:
            return {op: sources}
        else:
            output = {"from": {op: sources}}

    output["orderby"] = tokens["orderby"]
    output["offset"] = tokens["offset"]
    output["limit"] = tokens["limit"]
    return output


def to_statement(tokens):
    output = scrub(tokens["query"])
    output["with"] = scrub(tokens["with"])
    return output


def unquote(tokens):
    val = tokens[0]
    if val.startswith("'") and val.endswith("'"):
        val = "'" + val[1:-1].replace("''", "\\'") + "'"
    elif val.startswith('"') and val.endswith('"'):
        val = '"' + val[1:-1].replace('""', '\\"') + '"'
    elif val.startswith("`") and val.endswith("`"):
        val = '"' + val[1:-1].replace("``", "`").replace('"', '\\"') + '"'
    elif val.startswith("[") and val.endswith("]"):
        val = '"' + val[1:-1].replace("]]", "]").replace('"', '\\"') + '"'
    elif val.startswith("+"):
        val = val[1:]
    un = ast.literal_eval(val)
    return un


def to_string(tokens):
    val = tokens[0]
    val = "'" + val[1:-1].replace("''", "\\'") + "'"
    return {"literal": ast.literal_eval(val)}


# NUMBERS
realNum = Regex(r"[+-]?(\d+\.\d*|\.\d+)([eE][+-]?\d+)?").addParseAction(unquote)
intNum = Regex(r"[+-]?\d+([eE]\+?\d+)?").addParseAction(unquote)

# STRINGS, NUMBERS, VARIABLES
sqlString = Regex(r"\'(\'\'|[^'])*\'").addParseAction(to_string)
identString = Regex(r'\"(\"\"|[^"])*\"').addParseAction(unquote)
mysql_ident = Regex(r"\`(\`\`|[^`])*\`").addParseAction(unquote)
sqlserver_ident = Regex(r"\[(\]\]|[^\]])*\]").addParseAction(unquote)
ident = Combine(
    ~RESERVED
    + (delimitedList(
        Literal("*") | identString | mysql_ident | sqlserver_ident | Word(IDENT_CHAR),
        separator=".",
        combine=True,
    ))
).set_parser_name("identifier")

# EXPRESSIONS
expr = Forward()

# CASE
case = (
    CASE
    + Group(ZeroOrMore(
        (WHEN + expr("when") + THEN + expr("then")).addParseAction(to_when_call)
    ))("case")
    + Optional(ELSE + expr("else"))
    + END
).addParseAction(to_case_call)


# MAYBE TOO FLEXIBLE?
datatype = Word(IDENT_CHAR).addParseAction(lambda t: t[0].lower())

# CAST
cast = Group(
    CAST("op") + LB + expr("params") + AS + datatype("params") + RB
).addParseAction(to_json_call)

ordered_sql = Forward()


call_function = (
    ident("op") + LB + Optional(Group(ordered_sql) | delimitedList(expr))("params") + RB
).addParseAction(to_json_call)


def _or(values):
    output = values[0]
    for v in values[1:]:
        output |= v
    return output


interval = (
    INTERVAL("op")
    + (realNum | intNum)("params")
    + _or([
        Keyword(d, caseless=True).addParseAction(lambda t: t.lower()[:-1])
        for d in durations
    ])("params")
).addParseAction(to_json_call)

compound = (
    NULL
    | TRUE
    | FALSE
    | NOCASE
    | (DATE("op") + sqlString("params")).addParseAction(to_json_call)
    | interval
    | case
    | cast
    | (LB + Group(ordered_sql) + RB)
    | (LB + Group(delimitedList(expr)).addParseAction(to_tuple_call) + RB)
    | realNum.set_parser_name("float")
    | intNum.set_parser_name("int")
    | sqlString.set_parser_name("string")
    | call_function
    | ident
)

expr << Group(
    infixNotation(
        compound,
        [
            (
                o,
                1 if o in unary_ops else (3 if isinstance(o, tuple) else 2),
                RIGHT_ASSOC if o in unary_ops else LEFT_ASSOC,
                to_json_operator,
            )
            for o in KNOWN_OPS
        ],
    ).set_parser_name("expression")
)

alias = (
    (Group(ident) + Optional(LB + delimitedList(ident("col")) + RB))("name")
    .set_parser_name("alias")
    .addParseAction(to_alias)
)


# SQL STATEMENT
sortColumn = expr("value").set_parser_name("sort1") + Optional(
    DESC("sort") | ASC("sort")
) | expr("value").set_parser_name("sort2")

selectColumn = (
    Group(
        Group(expr).set_parser_name("expression1")("value")
        + Optional(
            OVER
            + LB
            + Optional(PARTITION_BY + delimitedList(Group(expr))("partitionby"))
            + Optional(ORDER_BY + delimitedList(Group(expr))("orderby"))
            + RB
        )("over")
        + Optional(Optional(AS) + alias)
        | Literal("*")("value")
    )
    .set_parser_name("column")
    .addParseAction(to_select_call)
)

table_source = (
    ((LB + ordered_sql + RB) | call_function)("value").set_parser_name("table source")
    + Optional(Optional(AS) + alias)
    | (ident("value").set_parser_name("table name") + Optional(AS) + alias)
    | ident.set_parser_name("table name")
)

join = (
    (
        CROSS_JOIN
        | FULL_JOIN
        | FULL_OUTER_JOIN
        | INNER_JOIN
        | JOIN
        | LEFT_JOIN
        | LEFT_OUTER_JOIN
        | RIGHT_JOIN
        | RIGHT_OUTER_JOIN
    )("op")
    + Group(table_source)("join")
    + Optional((ON + expr("on")) | (USING + expr("using")))
).addParseAction(to_join_call)

unordered_sql = (
    (
        SELECT_DISTINCT + delimitedList(selectColumn)("select_distinct")
        | SELECT + delimitedList(selectColumn)("select")
    )
    + Optional(
        (FROM + delimitedList(Group(table_source)) + ZeroOrMore(join))("from")
        + Optional(WHERE + expr("where"))
        + Optional(GROUP_BY + delimitedList(Group(selectColumn))("groupby"))
        + Optional(HAVING + expr("having"))
    )
).set_parser_name("unordered sql")

ordered_sql << (
    (
        Group(unordered_sql)
        + (
            OneOrMore(UNION_ALL + Group(unordered_sql))
            | ZeroOrMore(Group(UNION) + Group(unordered_sql))
        )
    )("union")
    + Optional(ORDER_BY + delimitedList(Group(sortColumn))("orderby"))
    + Optional(LIMIT + expr("limit"))
    + Optional(OFFSET + expr("offset"))
).set_parser_name("ordered sql").addParseAction(to_union_call)


statement = (
    Optional(
        WITH
        + delimitedList(Group(
            ident("name") + AS + LB + Group(ordered_sql)("value") + RB
        ))
    )("with")
    + Group(ordered_sql)("query")
).addParseAction(to_statement)

SQLParser = statement

# IGNORE SOME COMMENTS
oracleSqlComment = Literal("--") + restOfLine
mySqlComment = Literal("#") + restOfLine

engine.add_ignore(oracleSqlComment)
engine.add_ignore(mySqlComment)

engine.release()
