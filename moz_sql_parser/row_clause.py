# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Contact: Kyle Lahnakoski (kyle@lahnakoski.com)
#

from __future__ import absolute_import, division, unicode_literals


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
    else: # following
        if limit == "unbounded":
            return {"min": 0}
        else:
            return {"min": 0, "max": limit}


def _min(a, b):
    if a is None:
        return b
    if b is None:
        return a
    return min(a, b)


def _max(a, b):
    if a is None:
        return b
    if b is None:
        return a
    return max(a, b)


def _to_between_call(tokens):
    minn = scrub(tokens["min"])
    maxx = scrub(tokens["max"])

    if maxx.get("max") == 0:
        # following
        minn['max'] = maxx.get('min')
        return minn
    if minn.get("min") == 0:
        # preceding
        maxx['min'] = minn.get('max')
        return maxx

    return {
        "min": _min(minn.get("min"), maxx.get("min")),
        "max": _max(minn.get("max"), maxx.get("max")),
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
