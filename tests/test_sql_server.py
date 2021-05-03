# encoding: utf-8
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Author: Kyle Lahnakoski (kyle@lahnakoski.com)
#

from __future__ import absolute_import, division, unicode_literals

from unittest import TestCase

from mo_parsing.debug import Debugger
from moz_sql_parser import parse


class TestSqlServer(TestCase):
    def test_select_top_5(self):
        sql = """
        select	TOP (5)
            country_code,
            impact_code,
            impact_description,
            number_sites
        from	EUNIS.v1.BISE_Country_Threats_Pressures_Number_Sites
        order by number_sites desc
        """
        result = parse(sql)

        self.assertEqual(
            result,
            {
                "top": 5,
                "select": [
                    {"value": "country_code"},
                    {"value": "impact_code"},
                    {"value": "impact_description"},
                    {"value": "number_sites"},
                ],
                "from": "EUNIS.v1.BISE_Country_Threats_Pressures_Number_Sites",
                "orderby": {"value": "number_sites", "sort": "desc"},
            },
        )

    def test_issue13_top(self):
        # https://docs.microsoft.com/en-us/sql/t-sql/queries/top-transact-sql?view=sql-server-ver15
        sql = "SELECT TOP 3 * FROM Customers"
        with Debugger():
            result = parse(sql)
        self.assertEqual(result, {"top": 3, "select": "*", "from": "Customers",})

        sql = "SELECT TOP func(value) WITH TIES *"
        result = parse(sql)
        self.assertEqual(
            result, {"top": {"value": {"func": "value"}, "ties": True}, "select": "*"},
        )

        sql = "SELECT TOP 1 PERCENT WITH TIES *"
        result = parse(sql)
        self.assertEqual(
            result, {"top": {"percent": 1, "ties": True}, "select": "*"},
        )

        sql = "SELECT TOP a(b) PERCENT item"
        result = parse(sql)
        self.assertEqual(
            result, {"top": {"percent": {"a": "b"}}, "select": {"value": "item"}},
        )

        sql = "SELECT TOP a(b) PERCENT"
        with self.assertRaises(Exception):
            parse(sql)  # MISSING ANY COLUMN
