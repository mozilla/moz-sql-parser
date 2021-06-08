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

from mo_sql_parsing.sql_parser import SQLParser, scrub_literal, scrub

parseLocker = Lock()  # ENSURE ONLY ONE PARSING AT A TIME


def parse(sql):
    with parseLocker:
        sql = sql.rstrip().rstrip(";")
        parse_result = SQLParser.parseString(sql, parseAll=True)
        return scrub(parse_result)


def format(json, **kwargs):
    from mo_sql_parsing.formatting import Formatter

    return Formatter(**kwargs).format(json)


_ = json.dumps

__all__ = ["parse", "format"]
