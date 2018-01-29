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

import json

from mo_future import text_type, number_types, binary_type
from pyparsing import ParseException

from moz_sql_parser.sql_parser import SQLParser, all_exceptions


def parse(sql):
    try:
        parse_result = SQLParser.parseString(sql, parseAll=True)
    except Exception as e:
        if isinstance(e, ParseException) and e.msg == "Expected end of text":
            problems = all_exceptions[e.loc]
            expecting = [
                f
                for f in (set(p.msg.lstrip("Expected").strip() for p in problems)-{"Found unwanted token"})
                if not f.startswith("{")
            ]
            raise ParseException(sql, e.loc, "Expecting one of (" + (", ".join(expecting)) + ")")
        raise
    return _scrub(parse_result)


def _scrub(result):
    if isinstance(result, text_type):
        return result
    elif isinstance(result, binary_type):
        return result.decode('utf8')
    elif isinstance(result, number_types):
        return result
    elif not result:
        return {}
    elif isinstance(result, list) or not list(result.items()):
        if not result:
            return None
        elif len(result) == 1:
            return _scrub(result[0])
        else:
            return [_scrub(r) for r in result]
    else:
        return {k: _scrub(v) for k, v in result.items()}


_ = json.dumps
