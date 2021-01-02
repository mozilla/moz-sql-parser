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
                    "value": {"cast": [{"literal": ""}, {"varchar": {}}]},
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
                        {"timestamp": {"literal": "epoch"}},
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

    def test_issue5a_of_fork_date_cast_as_date(self):

        sql = 'select * from t left join ex on t.date = ex.date_at :: date""'
        result = parse(sql)
        self.assertEqual(
            result,
            {
                "from": [
                    "t",
                    {
                        "left join": "ex",
                        "on": {"eq": [
                            "t.date",
                            {"cast": ["ex.date_at", {"date": {}}]},
                        ]},
                    },
                ],
                "select": "*",
            },
        )

    def test_issue5b_of_fork_date_cast_as_date(self):

        sql = "select distinct date_at :: date as date_at from t"
        result = parse(sql)
        self.assertEqual(
            result,
            {
                "from": "t",
                "select": {"value": {"distinct": {
                    "name": "date_at",
                    "value": {"cast": ["date_at", {"date": {}}]},
                }}},
            },
        )

    def test_issue5c_of_fork_date_cast_as_date(self):

        sql = """
            select
                datediff('day', u.birth_date :: date, us.date_at :: date) as day_diff
            from
                users as u
            inner join
                user_sessions as us
        """
        result = parse(sql)
        self.assertEqual(
            result,
            {
                "from": [
                    {"name": "u", "value": "users"},
                    {"inner join": {"name": "us", "value": "user_sessions"}},
                ],
                "select": {
                    "name": "day_diff",
                    "value": {"datediff": [
                        {"literal": "day"},
                        {"cast": ["u.birth_date", {"date": {}}]},
                        {"cast": ["us.date_at", {"date": {}}]},
                    ]},
                },
            },
        )

    def test_issue5d_of_fork_column_is_keyword(self):
        sql = "select date as date_at from t"
        result = parse(sql)
        self.assertEqual(
            result, {"from": "t", "select": {"name": "date_at", "value": {"date": {}}}}
        )

    def test_issue5e_of_fork_column_is_keyword(self):
        sql = """
            select count(*) as validation_errors
            from t
            where date is null
        """
        result = parse(sql)
        self.assertEqual(
            result,
            {
                "from": "t",
                "select": {"name": "validation_errors", "value": {"count": "*"}},
                "where": {"missing": {"date": {}}},
            },
        )

    def test_issue5f_of_fork_column_is_keyword(self):
        sql = """
            select count(*) as validation_errors
            from t
            where timestamp is null
        """
        result = parse(sql)
        self.assertEqual(
            result,
            {
                "from": "t",
                "select": {"name": "validation_errors", "value": {"count": "*"}},
                "where": {"missing": {"timestamp": {}}},
            },
        )

    def test_issue5g_of_fork_window_function(self):
        # Ref: https://docs.aws.amazon.com/redshift/latest/dg/r_WF_SUM.html#r_WF_SUM-examples
        sql = """
            select
                sum(qty) over (order by dateid, salesid rows unbounded preceding) as sum
            from sales
            order by 2,1;
        """
        result = parse(sql)
        self.assertEqual(
            result,
            {
                "from": "sales",
                "orderby": [{"value": 2}, {"value": 1}],
                "select": {
                    "name": "sum",
                    "over": {
                        "orderby": [{"value": "dateid"}, {"value": "salesid"}],
                        "range": {"max": 0},
                    },
                    "value": {"sum": "qty"},
                },
            },
        )

    def test_issue5h_of_fork_extract(self):
        # Ref: https://docs.aws.amazon.com/redshift/latest/dg/r_EXTRACT_function.html#r_EXTRACT_function-examples
        sql = "select extract('epoch' from occurred_at)"
        result = parse(sql)
        self.assertEqual(
            result,
            {"select": {"value": {"extract": [{"literal": "epoch"}, "occurred_at"]}}},
        )

    def test_issue5i_of_fork_extract(self):
        # Ref: https://docs.aws.amazon.com/redshift/latest/dg/r_EXTRACT_function.html#r_EXTRACT_function-examples
        sql = "select extract(epoch from occurred_at)"
        result = parse(sql)
        self.assertEqual(
            result, {"select": {"value": {"extract": ["epoch", "occurred_at"]}}}
        )

    def test_cast_char(self):
        sql = "select cast(2008 as char(4));"
        result = parse(sql)
        self.assertEqual(result, {"select": {"value": {"cast": [2008, {"char": 4}]}}})

    def test_cast_decimal(self):
        sql = "select cast(109.652 as decimal(4,1));"
        result = parse(sql)
        self.assertEqual(
            result, {"select": {"value": {"cast": [109.652, {"decimal": [4, 1]}]}}}
        )

    def test_window_function1(self):
        sql = (
            "select sum(qty) over (order by a rows between 1 preceding and 2 following)"
        )
        result = parse(sql)
        self.assertEqual(
            result,
            {"select": {
                "over": {"orderby": {"value": "a"}, "range": {"min": -1, "max": 2}},
                "value": {"sum": "qty"},
            }},
        )

    def test_window_function2(self):
        sql = (
            "select sum(qty) over (order by a rows between 3 preceding and 1 preceding)"
        )
        result = parse(sql)
        self.assertEqual(
            result,
            {"select": {
                "over": {"orderby": {"value": "a"}, "range": {"min": -3, "max": -1}},
                "value": {"sum": "qty"},
            }},
        )

    def test_window_function3(self):
        sql = (
            "select sum(qty) over (order by a rows between 3 following and 5 following)"
        )
        result = parse(sql)
        self.assertEqual(
            result,
            {"select": {
                "over": {"orderby": {"value": "a"}, "range": {"min": 3, "max": 5}},
                "value": {"sum": "qty"},
            }},
        )

    def test_window_function4(self):
        sql = (
            "select sum(qty) over (order by a rows between 3 following and unbounded following)"
        )
        result = parse(sql)
        self.assertEqual(
            result,
            {"select": {
                "over": {"orderby": {"value": "a"}, "range": {"min": 3}},
                "value": {"sum": "qty"},
            }},
        )
