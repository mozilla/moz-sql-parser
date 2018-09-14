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

from collections import namedtuple
from unittest import skip

import pyparsing
from mo_testing.fuzzytestcase import FuzzyTestCase

from moz_sql_parser import parse
from moz_sql_parser import sql_parser

from moz_sql_parser import sql_synthesizer

class TestSimple(FuzzyTestCase):

    def get_error_message(self, expected_sql, new_sql, expected_json, new_json):
        res = """
SQL:         %s
JSON:        %s
Broken SQL:  %s
Broken JSON: %s
""" % (expected_sql, expected_json, new_sql, new_json)

        real = ":".join("{:02x}".format(ord(c)) for c in expected_sql)
        wrong = ":".join("{:02x}".format(ord(c)) for c in new_sql)
        res = res + """
Original SQL in HEX: %s
Synthetic SQL in HEX: %s
""" % (real, wrong)
        return res

    def test_two_tables(self):
        expected_sql = "SELECT * from XYZZY, ABC"
        expected_json = {'select': '*', 'from': ['XYZZY', 'ABC']}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_dot_table_name(self):
        expected_sql = "select * from SYS.XYZZY"
        expected_json = {'select': '*', 'from': 'SYS.XYZZY'}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql, new_sql, expected_json, new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    @skip("Not sure why")
    def select_one_column(self):
        expected_sql = "Select A from dual"
        expected_json = {'select': [{'value': 'A'}], 'from': ['dual']}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    @skip("Still need to figure out how to handle quotes")
    def test_select_quoted_name(self):
        expected_sql = 'Select a "@*#&", b as test."g.g".c from dual'
        expected_json = {'select': [{'name': '@*#&', 'value': 'a'}, {'name': 'test.g.g.c', 'value': 'b'}],
                         'from': 'dual'}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_select_expression(self):
        expected_sql = "SELECT a + b/2 + 45*c + (2/d) from dual"
        expected_json = {'select': {'value': {'add': ['a', {'div': ['b', 2]}, {'mult': [45, 'c']}, {'div': [2, 'd']}]}},
                         'from': 'dual'}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_select_underscore_name(self):
        expected_sql = "select _id from dual"
        expected_json = {'select': {'value': '_id'}, 'from': 'dual'}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_select_dots_names(self):
        expected_sql = "select a.b.c._d from dual"
        expected_json = {'select': {'value': 'a.b.c._d'}, 'from': 'dual'}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    @skip("Not sure why")
    def select_many_column(self):
        expected_sql = "Select a, b, c from dual"
        expected_json = {'select': [{'value': 'a'}, {'value': 'b'}, {'value': 'c'}], 'from': ['dual']}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_where_neq(self):
        expected_sql = "SELECT * FROM dual WHERE a<>'test'"
        expected_json = {'select': '*', 'from': 'dual', 'where': {'neq': ['a', {'literal': 'test'}]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_where_in(self):
        expected_sql = "SELECT a FROM dual WHERE a in ('r', 'g', 'b')"
        expected_json = {'select': {'value': 'a'}, 'from': 'dual', 'where': {'in': ['a', {'literal': ['r', 'g', 'b']}]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_where_in_and_in(self):
        expected_sql = "SELECT a FROM dual WHERE a in ('r', 'g', 'b') AND b in (10, 11, 12)"
        expected_json = {'select': {'value': 'a'}, 'from': 'dual',
                         'where': {'and': [{'in': ['a', {'literal': ['r', 'g', 'b']}]}, {'in': ['b', [10, 11, 12]]}]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_eq(self):
        expected_sql = "SELECT a, b FROM t1, t2 WHERE t1.a=t2.b"
        expected_json = {'select': [{'value': 'a'}, {'value': 'b'}], 'from': ['t1', 't2'],
                         'where': {'eq': ['t1.a', 't2.b']}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_is_null(self):
        expected_sql = "SELECT a, b FROM t1 WHERE t1.a IS NULL"
        expected_json = {'select': [{'value': 'a'}, {'value': 'b'}], 'from': 't1', 'where': {'missing': 't1.a'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_is_not_null(self):
        expected_sql = "SELECT a, b FROM t1 WHERE t1.a IS NOT NULL"
        expected_json = {'select': [{'value': 'a'}, {'value': 'b'}], 'from': 't1', 'where': {'exists': 't1.a'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_groupby(self):
        expected_sql = "select a, count(1) as b from mytable group by a"
        expected_json = {'select': [{'value': 'a'}, {'name': 'b', 'value': {'count': 1}}], 'from': 'mytable',
                         'groupby': {'value': 'a'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_function(self):
        expected_sql = "select count(1) from mytable"
        expected_json = {'select': {'value': {'count': 1}}, 'from': 'mytable'}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_order_by(self):
        expected_sql = "select count(1) from dual order by a"
        expected_json = {'select': {'value': {'count': 1}}, 'from': 'dual', 'orderby': {'value': 'a'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_order_by_asc(self):
        expected_sql = "select count(1) from dual order by a asc"
        expected_json = {'select': {'value': {'count': 1}}, 'from': 'dual', 'orderby': {'value': 'a', 'sort': 'asc'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_neg_or_precedence(self):
        expected_sql = "select B,C from table1 where A=-900 or B=100"
        expected_json = {'from': 'table1', 'where': {'or': [{'eq': ['A', -900]}, {'eq': ['B', 100]}]},
                         'select': [{'value': 'B'}, {'value': 'C'}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_negative_number(self):
        expected_sql = "select a from table1 where A=-900"
        expected_json = {'from': 'table1', 'where': {'eq': ['A', -900]}, 'select': {'value': 'a'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_like_in_where(self):
        expected_sql = "select a from table1 where A like '%20%'"
        expected_json = {'from': 'table1', 'where': {'like': ['A', {'literal': '%20%'}]}, 'select': {'value': 'a'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_like_in_select(self):
        expected_sql = "select case when A like 'bb%' then 1 else 0 end as bb from table1"
        expected_json = {'from': 'table1', 'select': {'name': 'bb', 'value': {
            'case': [{'when': {'like': ['A', {'literal': 'bb%'}]}, 'then': 1}, 0]}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_ugly_case_statement(self):
        expected_sql = """
select player_name,
weight,
case when weight > 250 then 'over 250'
when weight > 200 then '201-250'
when weight > 175 then '176-200'
else '175 or under' end as weight_group
from benn.college_football_players
"""
        expected_json = {'from': 'benn.college_football_players',
                         'select': [{'value': 'player_name'}, {'value': 'weight'}, {'name': 'weight_group', 'value': {
                             'case': [{'then': {'literal': 'over 250'}, 'when': {'gt': ['weight', 250]}},
                                      {'then': {'literal': '201-250'}, 'when': {'gt': ['weight', 200]}},
                                      {'then': {'literal': '176-200'}, 'when': {'gt': ['weight', 175]}},
                                      {'literal': '175 or under'}]}}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_like_from_pr16(self):
        expected_sql = "select * from trade where school LIKE '%shool' and name='abc' and id IN ('1','2')"
        expected_json = {'from': 'trade', 'where': {
            'and': [{'like': ['school', {'literal': '%shool'}]}, {'eq': ['name', {'literal': 'abc'}]},
                    {'in': ['id', {'literal': ['1', '2']}]}]}, 'select': '*'}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_in_expression(self):
        expected_sql = "select * from task where repo.branch.name in ('try', 'mozilla-central')"
        expected_json = {'from': 'task', 'select': '*',
                         'where': {'in': ['repo.branch.name', {'literal': ['try', 'mozilla-central']}]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_joined_table_name(self):
        expected_sql = "SELECT * FROM table1 t1 JOIN table3 t3 ON t1.id = t3.id"
        expected_json = {'from': [{'name': 't1', 'value': 'table1'},
                                  {'on': {'eq': ['t1.id', 't3.id']}, 'join': {'name': 't3', 'value': 'table3'}}],
                         'select': '*'}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))
    @skip("Not sure why")
    def test_not_equal(self):
        expected_sql = "select * from task where build.product is not null and build.product!='firefox'"
        expected_json = {'select': '*', 'from': 'task',
                         'where': {'and': [{'exists': 'build.product'}, {'neq': {'build.product': 'firefox'}}]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))

    def test_pr19(self):
        expected_sql = "select empid from emp where ename like 's%' "
        expected_json = {'from': 'emp', 'where': {'like': ['ename', {'literal': 's%'}]}, 'select': {'value': 'empid'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = sql_synthesizer.generate_sql(expected_json)
            new_json = parse(new_sql)
            self.assertEqual(new_json,
                             expected_json,
                             self.get_error_message(expected_sql,
                                                    new_sql,
                                                    expected_json,
                                                    new_json))
        except Exception as e:
            print(self.get_error_message(expected_sql, new_sql, expected_json, new_json))
            print(e)
            self.assertTrue(False, self.get_error_message(expected_sql,
                                                          new_sql,
                                                          expected_json,
                                                          new_json))
