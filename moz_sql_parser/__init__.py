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

from moz_sql_parser.sql_parser import SQLParser


def parse(sql):
    parse_result = SQLParser.parseString(sql, parseAll=True)
    return _scrub(parse_result)


def _scrub(result):
    if isinstance(result, (str, unicode, int, float)):
        return result
    elif not result:
        return {}
    elif isinstance(result, list) or not result.items():
        if not result:
            return None
        elif len(result) == 1:
            return _scrub(result[0])
        else:
            return [_scrub(r) for r in result]
    else:
        return {k: _scrub(v) for k, v in result.items()}


_ = json.dumps
