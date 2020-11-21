# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Contact: Kyle Lahnakoski (kyle@lahnakoski.com)
#

from __future__ import absolute_import, division, unicode_literals

import json
from threading import Lock

from mo_parsing import ParseException
from moz_sql_parser.debugs import all_exceptions
from moz_sql_parser.sql_parser import SQLParser, scrub_literal, scrub

parseLocker = Lock()  # ENSURE ONLY ONE PARSING AT A TIME


def parse(sql):
    with parseLocker:
        try:
            all_exceptions.clear()
            sql = sql.rstrip().rstrip(";")
            parse_result = SQLParser.parseString(sql, parseAll=True)
            return scrub(parse_result)
        except Exception as cause:
            if isinstance(cause, ParseException) and cause.msg == "Expected end of text":
                problems = all_exceptions.get(cause.loc, [])
                expecting = [
                    f
                    for f in (set(p.msg.lstrip("Expected").strip() for p in problems)-{"Found unwanted token"})
                    if not f.startswith("{")
                ]
                raise ParseException(sql, cause.loc, "Expecting one of (" + (", ".join(expecting)) + ")", cause=cause)
            raise

def format(json, **kwargs):
    from moz_sql_parser.formatting import Formatter
    return Formatter(**kwargs).format(json)

_ = json.dumps

__all__ = [
    'parse',
    'format'
]
