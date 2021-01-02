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
from moz_sql_parser.keywords import *
from moz_sql_parser.utils import *
from moz_sql_parser.row_clause import row_clause

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
        Literal("*") | literal_string | mysql_ident | sqlserver_ident | Word(IDENT_CHAR),
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
    Keyword(d, caseless=True).addParseAction(lambda t: durations[t.lower()])
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
    Keyword("extract", caseless=True)("op") + LB + (_standard_time_intervals | expr("params")) + FROM + expr("params") + RB
).addParseAction(to_json_call)

namedColumn = Group(
    Group(expr)("value") + Optional(Optional(AS) + Group(ident))("name")
)

distinct = (
    DISTINCT("op") + delimitedList(namedColumn)("params")
).addParseAction(to_json_call)

ordered_sql = Forward()

call_function = (
    ident("op") + LB + Optional(Group(ordered_sql) | delimitedList(expr))("params") + RB
).addParseAction(to_json_call)

compound = (
    NULL
    | TRUE
    | FALSE
    | NOCASE
    | (DATE("op") + sqlString("params")).addParseAction(to_json_call)
    | interval
    | timestamp
    | extract
    | case
    | switch
    | cast
    | distinct
    | (LB + Group(ordered_sql) + RB)
    | (LB + Group(delimitedList(expr)).addParseAction(to_tuple_call) + RB)
    | realNum.set_parser_name("float")
    | intNum.set_parser_name("int")
    | sqlString.set_parser_name("string")
    | call_function
    | known_types
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

window = Optional(
    WITHIN_GROUP
    + LB
    + Optional(ORDER_BY + delimitedList(Group(sortColumn))("orderby"))
    + RB
)("within") + Optional(
    OVER
    + LB
    + Optional(PARTITION_BY + delimitedList(Group(expr))("partitionby"))
    + Optional(
        ORDER_BY
        + delimitedList(Group(sortColumn))("orderby")
        + Optional(row_clause)("range")
    )
    + RB
)("over")

selectColumn = (
    Group(
        Group(expr).set_parser_name("expression1")("value")
        + Optional(window)
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
    SELECT
    + delimitedList(selectColumn)("select")
    + Optional(
        (FROM + delimitedList(Group(table_source)) + ZeroOrMore(join))("from")
        + Optional(WHERE + expr("where"))
        + Optional(GROUP_BY + delimitedList(Group(namedColumn))("groupby"))
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
engine.release()
