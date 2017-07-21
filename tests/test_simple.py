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

from mo_testing.fuzzytestcase import FuzzyTestCase

from moz_sql_parser import parse
from moz_sql_parser import sql_parser


class TestSimple(FuzzyTestCase):

    def test_two_tables(self):
        result = parse("SELECT * from XYZZY, ABC")
        expected = {
            "select": {"value": "*"},
            "from": ["XYZZY", "ABC"]
        }
        self.assertEqual(result, expected)

    def test_dot_table_name(self):
        result = parse("select * from SYS.XYZZY")
        expected = {
            "select": {"value": "*"},
            "from": "SYS.XYZZY"
        }
        self.assertEqual(result, expected)

    def select_one_column(self):
        result = parse("Select A from dual")
        expected = {
            "select": [{"value": "A"}],
            "from": ["dual"]
        }
        self.assertEqual(result, expected)

    def test_select_quote(self):
        result = parse("Select '''' from dual")
        expected = {
            "select": {"value": {"literal": "'"}},
            "from": "dual"
        }
        self.assertEqual(result, expected)

    def test_select_quoted_name(self):
        result = parse('Select a "@*#&", b as test."g.g".c from dual')
        expected = {
            "select": [
                {"name": "@*#&", "value": "a"},
                {"name": "test.g.g.c", "value": "b"}
            ],
            "from": "dual"
        }
        self.assertEqual(result, expected)

    def test_select_expression(self):
        #                         1         2         3         4         5         6
        #               0123456789012345678901234567890123456789012345678901234567890123456789
        result = parse("SELECT a + b/2 + 45*c + (2/d) from dual")
        expected = {
            "select": {"value": {"add": [
                "a",
                {"div": ["b", 2]},
                {"mult": [45, "c"]},
                {"div": [2, "d"]}
            ]}},
            "from": "dual"
        }
        self.assertEqual(result, expected)

    def test_select_underscore_name(self):
        #                         1         2         3         4         5         6
        #               0123456789012345678901234567890123456789012345678901234567890123456789
        result = parse("select _id from dual")
        expected = {
            "select": {"value": "_id"},
            "from": "dual"
        }
        self.assertEqual(result, expected)

    def test_select_dots_names(self):
        #                         1         2         3         4         5         6
        #               0123456789012345678901234567890123456789012345678901234567890123456789
        result = parse("select a.b.c._d from dual")
        expected = {
            "select": {"value": "a.b.c._d"},
            "from": "dual"
        }
        self.assertEqual(result, expected)

    def select_many_column(self):
        result = parse("Select a, b, c from dual")
        expected = {
            "select": [
                {"value": "a"},
                {"value": "b"},
                {"value": "c"}
            ],
            "from": ["dual"]
        }
        self.assertEqual(result, expected)

    def test_bad_select1(self):
        self.assertRaises('Expected select', lambda: parse("se1ect A, B, C from dual"))

    def test_bad_select2(self):
        self.assertRaises('Expected {{expression1 [{[as] column_name1}]}', lambda: parse("Select &&& FROM dual"))

    def test_bad_from(self):
        self.assertRaises('(at char 20)', lambda: parse("select A, B, C frum dual"))

    def test_incomplete1(self):
        self.assertRaises('Expected {{expression1 [{[as] column_name1}]}', lambda: parse("SELECT"))

    def test_incomplete2(self):
        self.assertRaises("", lambda: parse("SELECT * FROM"))

    def test_where_neq(self):
        #                         1         2         3         4         5         6
        #               0123456789012345678901234567890123456789012345678901234567890123456789
        result = parse("SELECT * FROM dual WHERE a<>'test'")
        expected = {
            "select": {"value": "*"},
            "from": "dual",
            "where": {"neq": ["a", {"literal": "test"}]}
        }
        self.assertEqual(result, expected)

    def test_where_in(self):
        result = parse("SELECT a FROM dual WHERE a in ('r', 'g', 'b')")
        expected = {
            "select": {"value": "a"},
            "from": "dual",
            "where": {"in": [
                "a",
                [
                    {"literal": "r"},
                    {"literal": "g"},
                    {"literal": "b"}
                ]
            ]}
        }
        self.assertEqual(result, expected)

    def test_where_in_and_in(self):
        #                         1         2         3         4         5         6
        #               0123456789012345678901234567890123456789012345678901234567890123456789
        result = parse("SELECT a FROM dual WHERE a in ('r', 'g', 'b') AND b in (10, 11, 12)")
        expected = {
            "select": {"value": "a"},
            "from": "dual",
            "where": {"and": [
                {"in": [
                    "a",
                    [
                        {"literal": "r"},
                        {"literal": "g"},
                        {"literal": "b"}
                    ]
                ]},
                {"in": [
                    "b",
                    [10, 11, 12]
                ]}
            ]}
        }
        self.assertEqual(result, expected)

    def test_eq(self):
        result = parse("SELECT a, b FROM t1, t2 WHERE t1.a=t2.b")
        expected = {
            "select": [
                {"value": "a"},
                {"value": "b"}
            ],
            "from": ["t1", "t2"],
            "where": {"eq": ["t1.a", "t2.b"]}
        }
        self.assertEqual(result, expected)

    def test_groupby(self):
        result = parse("select a, count(1) as b from mytable group by a")
        expected = {
            "select": [
                {"value": "a"},
                {"name": "b", "value": {"count": 1}}
            ],
            "from": "mytable",
            "groupby": {"value": "a"}
        }
        self.assertEqual(result, expected)

    def test_function(self):
        result = parse("select count(1) from mytable")
        expected = {
            "select": {"value": {"count": 1}},
            "from": "mytable"
        }
        self.assertEqual(result, expected)

    def test_order_by(self):
        result = parse("select count(1) from dual order by a")
        expected = {
            "select": {"value": {"count": 1}},
            "from": "dual",
            "orderby": {"value": "a"}
        }
        self.assertEqual(result, expected)

    def test_debug_is_off(self):
        self.assertFalse(sql_parser.DEBUG, "Turn off debugging")
