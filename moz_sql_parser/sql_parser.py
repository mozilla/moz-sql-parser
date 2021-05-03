# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Contact: Kyle Lahnakoski (kyle@lahnakoski.com)
#

from __future__ import absolute_import, division, unicode_literals

from mo_parsing.engine import Engine
from mo_parsing.helpers import delimitedList, restOfLine
from moz_sql_parser.keywords import *
from moz_sql_parser.utils import *
from moz_sql_parser.windows import sortColumn, window

engine = Engine().use()
engine.add_ignore(Literal("--") + restOfLine)
engine.add_ignore(Literal("#") + restOfLine)

# IDENTIFIER
literal_string = Regex(r'\"(\"\"|[^"])*\"').addParseAction(unquote)
mysql_ident = Regex(r"\`(\`\`|[^`])*\`").addParseAction(unquote)
sqlserver_ident = Regex(r"\[(\]\]|[^\]])*\]").addParseAction(unquote)
ident = Combine(
    ~RESERVED
    + (delimitedList(
        literal_string
        | mysql_ident
        | sqlserver_ident
        | Word(IDENT_CHAR),
        separator=".",
        combine=True,
    ))
).set_parser_name("identifier")

# EXPRESSIONS

# CASE
case = (
    CASE
    + Group(ZeroOrMore(
        (WHEN + expr("when") + THEN + expr("then")).addParseAction(to_when_call)
    ))("case")
    + Optional(ELSE + expr("else"))
    + END
).addParseAction(to_case_call)

# SWITCH
switch = (
    CASE
    + expr("value")
    + Group(ZeroOrMore(
        (WHEN + expr("when") + THEN + expr("then")).addParseAction(to_when_call)
    ))("case")
    + Optional(ELSE + expr("else"))
    + END
).addParseAction(to_switch_call)

# CAST
cast = Group(
    CAST("op") + LB + expr("params") + AS + known_types("params") + RB
).addParseAction(to_json_call)

_standard_time_intervals = MatchFirst([
    Keyword(d, caseless=True).addParseAction(lambda t: durations[t[0].lower()])
    for d in durations.keys()
]).set_parser_name("duration")("params")

duration = (realNum | intNum)("params") + _standard_time_intervals

interval = (
    INTERVAL + ("'" + delimitedList(duration) + "'" | duration)
).addParseAction(to_interval_call)

timestamp = (
    time_functions("op")
    + (
        sqlString("params")
        | MatchFirst([
            Keyword(t, caseless=True).addParseAction(lambda t: t.lower()) for t in times
        ])("params")
    )
).addParseAction(to_json_call)

extract = (
    Keyword("extract", caseless=True)("op")
    + LB
    + (_standard_time_intervals | expr("params"))
    + FROM
    + expr("params")
    + RB
).addParseAction(to_json_call)

namedColumn = Group(
    Group(expr)("value") + Optional(Optional(AS) + Group(ident))("name")
)

distinct = (
    DISTINCT("op") + delimitedList(namedColumn)("params")
).addParseAction(to_json_call)

ordered_sql = Forward()

call_function = (
    ident("op")
    + LB
    + Optional(Group(ordered_sql) | delimitedList(expr))("params")
    + Optional(
        Keyword("ignore", caseless=True) + Keyword("nulls", caseless=True)
    )("ignore_nulls")
    + RB
).addParseAction(to_json_call)

with Engine(white=""):
    def scale(tokens):
        return {"mul":[tokens[0], tokens[1]]}

    scale_function = ((realNum | intNum) + call_function).addParseAction(scale)
    scale_ident = ((realNum | intNum) + ident).addParseAction(scale)

compound = (
    NULL
    | TRUE
    | FALSE
    | NOCASE
    | interval
    | timestamp
    | extract
    | case
    | switch
    | cast
    | distinct
    | (LB + Group(ordered_sql) + RB)
    | (LB + Group(delimitedList(expr)).addParseAction(to_tuple_call) + RB)
    | sqlString.set_parser_name("string")
    | hexNum.set_parser_name("hex")
    | scale_function
    | scale_ident
    | realNum.set_parser_name("float")
    | intNum.set_parser_name("int")
    | call_function
    | known_types
    | Combine(ident + Optional(".*"))
)

expr << (
    (
        Literal("*") |
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
    )("value")
    + Optional(window)
).addParseAction(to_expression_call)


alias = (
    (Group(ident) + Optional(LB + delimitedList(ident("col")) + RB))("name")
    .set_parser_name("alias")
    .addParseAction(to_alias)
)


selectColumn = (
    Group(
        Group(expr).set_parser_name("expression1")("value")
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

unordered_sql = Group(
    SELECT
    + Optional(
        TOP
        + expr("value")
        + Optional(Keyword("percent", caseless=True))("percent")
        + Optional(WITH + Keyword("ties", caseless=True))("ties")
    )("top").addParseAction(to_top_clause)
    + delimitedList(selectColumn)("select")
    + Optional(
        (FROM + delimitedList(Group(table_source)) + ZeroOrMore(join))("from")
        + Optional(WHERE + expr("where"))
        + Optional(GROUP_BY + delimitedList(Group(namedColumn))("groupby"))
        + Optional(HAVING + expr("having"))
    )
).set_parser_name("unordered sql")

ordered_sql << (
    (unordered_sql + ZeroOrMore((UNION_ALL | UNION) + unordered_sql))("union")
    + Optional(ORDER_BY + delimitedList(Group(sortColumn))("orderby"))
    + Optional(LIMIT + expr("limit"))
    + Optional(OFFSET + expr("offset"))
).set_parser_name("ordered sql").addParseAction(to_union_call)

statement = Forward()
statement << (
    Optional(
        WITH + delimitedList(Group(ident("name") + AS + LB + statement("value") + RB))
    )("with")
    + Group(ordered_sql)("query")
).addParseAction(to_statement)

SQLParser = statement
engine.release()
