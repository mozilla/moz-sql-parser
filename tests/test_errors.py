# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Author: Kyle Lahnakoski (kyle@lahnakoski.com)
#

from __future__ import absolute_import, division, unicode_literals

from unittest import TestCase

from moz_sql_parser import parse, format
from .util import assertRaises  # RELATIVE IMPORT SO WE CAN RUN IN pyLibrary


class TestErrors(TestCase):

    def test_dash_in_tablename(self):
        assertRaises(
            ["group by", "order by", "having", "limit", "where"],
            #              012345678901234567890123456789012345678901234567890123456789
            lambda: parse("select * from coverage-summary.source.file.covered limit 20")
        )

    def test_join_on_using_together(self):
        assertRaises(
            "Expecting one of",
            lambda: parse("SELECT * FROM t1 JOIN t2 ON t1.id=t2.id USING (id)")
        )

    def test_bad_join_name(self):
        bad_json = {'select': {'value': 't1.field1'},
                    'from': ['t1', {'left intro join': 't2', 'on': {'eq': ['t1.id', 't2.id']}}]}
        assertRaises(
            ["Fail to detect join type", 'left intro join'],
            lambda: format(bad_json)
        )

    def test_order_by_must_follow_union(self):
        assertRaises(
            ["(at char 27)"],
            lambda: parse("select a from b order by a union select 2")
        )

