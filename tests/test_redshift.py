# encoding: utf-8
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Author: Kyle Lahnakoski (kyle@lahnakoski.com)
#

from __future__ import absolute_import, division, unicode_literals

from unittest import TestCase

from moz_sql_parser import parse


class TestRedshift(TestCase):
    def test_issue149a_casting(self):
        # Ref: https://docs.aws.amazon.com/redshift/latest/dg/r_CAST_function.html#r_CAST_function-examples
        sql = "select '' :: varchar as placeholder from table"
        result = parse(sql)
        self.assertEqual(
            result,
            {
                "from": "table",
                "select": {
                    "name": "placeholder",
                    "value": {"cast": [{"literal": ""}, "varchar"]},
                },
            },
        )

    def test_issue149b_epoch_to_datetime(self):
        # Ref: https://stackoverflow.com/questions/39815425/how-to-convert-epoch-to-datetime-redshift
        # https://docs.aws.amazon.com/redshift/latest/dg/r_interval_literals.html
        sql = (
            "select timestamp 'epoch' + your_timestamp_column * interval '1 second' AS"
            " your_column_alias from your_table"
        )
        result = parse(sql)
        self.assertEqual(
            result,
            {
                "from": "your_table",
                "select": {
                    "name": "your_column_alias",
                    "value": {"add": [
                        {"timestamp": "epoch"},
                        {"mul": ["your_timestamp_column", {"interval": [1, "second"]}]},
                    ]},
                },
            },
        )

    def test_issue149c_window_functions(self):
        # Ref:
        #     https://docs.aws.amazon.com/redshift/latest/dg/r_WF_LISTAGG.html#r_WF_LISTAGG-examples
        #     https://docs.aws.amazon.com/redshift/latest/dg/r_WF_first_value.html#r_WF_first_value-examples

        sql = """
            select
                listagg(sellerid) within group (order by sellerid) over()
            from winsales;
        """

        result = parse(sql)
        self.assertEqual(
            result,
            {
                "from": "winsales",
                "select": {
                    "value": {"listagg": "sellerid"},
                    "within": {"orderby": {"value": "sellerid"}},
                },
            },
        )

    def test_issue149d_switched_case(self):
        # Ref:
        #     https://docs.aws.amazon.com/redshift/latest/dg/r_CASE_function.html
        #     https://docs.microsoft.com/en-in/sql/t-sql/language-elements/case-transact-sql?view=sql-server-ver15

        sql = """
        select
            CASE quantity
                WHEN 30 THEN 'The quantity is 30'
                WHEN 31 THEN 'The quantity is 31'
                ELSE 'The quantity is not 30 or 31'
            END AS quantitytext
        from
            source
        """
        result = parse(sql)
        self.assertEqual(
            result,
            {
                "from": "source",
                "select": {
                    "name": "quantitytext",
                    "value": {"case": [
                        {
                            "when": {"eq": ["quantity", 30]},
                            "then": {"literal": "The quantity is 30"},
                        },
                        {
                            "when": {"eq": ["quantity", 31]},
                            "then": {"literal": "The quantity is 31"},
                        },
                        {"literal": "The quantity is not 30 or 31"},
                    ]},
                },
            },
        )

    def test_issue149e_2_union(self):
        sql = "select * from a union all select * from b union all select * from c"
        result = parse(sql)

        self.assertEqual(
            result,
            {"union_all": [
                {"from": "a", "select": "*"},
                {"from": "b", "select": "*"},
                {"from": "c", "select": "*"},
            ]},
        )

    def test_dates1(self):
        # https://docs.aws.amazon.com/redshift/latest/dg/r_interval_literals.html
        sql = "select interval '1w, 1h, 1m, 1s'"
        result = parse(sql)

        self.assertEqual(
            result,
            {"select": {"value": {"add": [
                {"interval": [1, "week"]},
                {"interval": [1, "hour"]},
                {"interval": [1, "minute"]},
                {"interval": [1, "second"]},
            ]}}},
        )

    def test_dates2(self):
        # https://docs.aws.amazon.com/redshift/latest/dg/r_interval_literals.html
        sql = "select interval '52 weeks'"
        result = parse(sql)

        self.assertEqual(result, {"select": {"value": {"interval": [52, "week"]}}})

    def test_dates3(self):
        # https://docs.aws.amazon.com/redshift/latest/dg/r_interval_literals.html
        sql = "select interval '0.5 days'"
        result = parse(sql)

        self.assertEqual(result, {"select": {"value": {"interval": [0.5, "day"]}}})
