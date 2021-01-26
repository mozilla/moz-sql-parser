# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Contact: Kyle Lahnakoski (kyle@lahnakoski.com)
#

from __future__ import absolute_import, division, unicode_literals

from mo_parsing.helpers import delimitedList

from moz_sql_parser.keywords import *
from moz_sql_parser.utils import *


# https://docs.microsoft.com/en-us/sql/t-sql/queries/select-over-clause-transact-sql?view=sql-server-ver15


def _to_bound_call(tokens):
    zero = tokens["zero"]
    if zero:
        return {"min": 0, "max": 0}

    direction = scrub(tokens["direction"])
    limit = scrub(tokens["limit"])
    if direction == "preceding":
        if limit == "unbounded":
            return {"max": 0}
        else:
            return {"min": -limit, "max": 0}
    else:  # following
        if limit == "unbounded":
            return {"min": 0}
        else:
            return {"min": 0, "max": limit}


def _to_between_call(tokens):
    minn = scrub(tokens["min"])
    maxx = scrub(tokens["max"])

    if maxx.get("max") == 0:
        # following
        return {
            "min": minn.get("min"),
            "max": maxx.get("min"),
        }
    elif minn.get("min") == 0:
        # preceding
        return {"min": minn.get("max"), "max": maxx.get("max")}
    else:
        return {
            "min": minn.get("min"),
            "max": maxx.get("max"),
        }


UNBOUNDED = Keyword("unbounded", caseless=True)
PRECEDING = Keyword("preceding", caseless=True)
FOLLOWING = Keyword("following", caseless=True)
CURRENT_ROW = Keyword("current", caseless=True) + Keyword("row", caseless=True)
ROWS = Keyword("rows", caseless=True)
RANGE = Keyword("range", caseless=True)

bound = (
    CURRENT_ROW("zero")
    | (UNBOUNDED | intNum)("limit") + (PRECEDING | FOLLOWING)("direction")
).addParseAction(_to_bound_call)
between = (BETWEEN + bound("min") + AND + bound("max")).addParseAction(_to_between_call)

row_clause = (ROWS | RANGE).suppress() + (between | bound)

# SQL STATEMENT
sortColumn = expr("value").set_parser_name("sort1") + Optional(
    DESC("sort") | ASC("sort")
) | expr("value").set_parser_name("sort2")

window = (
    # Optional((Keyword("ignore", caseless=True) + Keyword("nulls", caseless=True))("ignore_nulls").addParseAction(lambda: True))
    Optional(
        WITHIN_GROUP
        + LB
        + Optional(ORDER_BY + delimitedList(Group(sortColumn))("orderby"))
        + RB
    )("within")
    + Optional(
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
)
