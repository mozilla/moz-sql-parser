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

from mo_dots import NullType
from mo_future import binary_type, number_types, text

from mo_parsing import ParseException
from moz_sql_parser.debugs import all_exceptions
from moz_sql_parser.sql_parser import SQLParser, scrub_literal


def __deploy__():
    # ONLY MEANT TO BE RUN FOR DEPLOYMENT
    from mo_files import File
    source_file = File("moz_sql_parser/sql_parser.py")
    lines = source_file.read().split("\n")
    lines = [
        "sys.setrecursionlimit(1500)" if line.startswith("sys.setrecursionlimit") else line
        for line in lines
    ]
    source_file.write("\n".join(lines))


parseLocker = Lock()  # ENSURE ONLY ONE PARSING AT A TIME


def parse(sql):
    with parseLocker:
        try:
            all_exceptions.clear()
            sql = sql.rstrip().rstrip(";")
            parse_result = SQLParser.parseString(sql, parseAll=True)
            return _scrub(parse_result)
        except Exception as e:
            if isinstance(e, ParseException) and e.msg == "Expected end of text":
                problems = all_exceptions.get(e.loc, [])
                expecting = [
                    f
                    for f in (set(p.msg.lstrip("Expected").strip() for p in problems)-{"Found unwanted token"})
                    if not f.startswith("{")
                ]
                raise ParseException(sql, e.loc, "Expecting one of (" + (", ".join(expecting)) + ")")
            raise


def format(json, **kwargs):
    from moz_sql_parser.formatting import Formatter
    return Formatter(**kwargs).format(json)


def _scrub(result):
    if isinstance(result, (text, NullType)):
        return result
    elif isinstance(result, binary_type):
        return result.decode('utf8')
    elif isinstance(result, number_types):
        return result
    elif isinstance(result, dict) and not result:
        return result
    elif isinstance(result, list):
        output = [
            rr
            for r in result
            for rr in [_scrub(r)]
            if rr is not None
        ]

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
            if v is not None
            for vv in [_scrub(v)]
            if vv is not None
        }
        if output:
            return output
        return _scrub(list(result))


_ = json.dumps

__all__ = [
    'parse',
    'format'
]
