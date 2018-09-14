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

from moz_sql_parser import format

from pprint import pformat

class TestSimple(FuzzyTestCase):

    def get_error_message(self, expected_sql, new_sql, expected_json, new_json):
        res = """
SQL:         %s
Broken SQL:  %s

JSON:
%s
Broken JSON:
%s
""" % (expected_sql, new_sql, pformat(expected_json), pformat(new_json))

        real = ":".join("{:02x}".format(ord(c)) for c in expected_sql)
        wrong = ":".join("{:02x}".format(ord(c)) for c in new_sql)
        res = res + """
Original SQL in HEX: %s
Synthetic SQL in HEX: %s
""" % (real, wrong)
        return res

    def test_001(self):
        expected_sql = "SELECT * FROM test1"
        expected_json = {'from': 'test1', 'select': '*'}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_002(self):
        expected_sql = "SELECT * FROM test1, test2"
        expected_json = {'from': ['test1', 'test2'], 'select': '*'}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_003(self):
        expected_sql = "SELECT * FROM test2, test1"
        expected_json = {'from': ['test2', 'test1'], 'select': '*'}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_004(self):
        expected_sql = "SELECT f1 FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': 'f1'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_005(self):
        expected_sql = "SELECT f2 FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': 'f2'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_006(self):
        expected_sql = "SELECT f2, f1 FROM test1"
        expected_json = {'from': 'test1', 'select': [{'value': 'f2'}, {'value': 'f1'}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_007(self):
        expected_sql = "SELECT f1, f2 FROM test1"
        expected_json = {'from': 'test1', 'select': [{'value': 'f1'}, {'value': 'f2'}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_008(self):
        expected_sql = "SELECT *, * FROM test1"
        expected_json = {'from': 'test1', 'select': ['*']}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_009(self):
        expected_sql = "SELECT *, min(f1,f2), max(f1,f2) FROM test1"
        expected_json = {'from': 'test1',
                         'select': ['*', {'value': {'min': ['f1', 'f2']}}, {'value': {'max': ['f1', 'f2']}}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_010(self):
        expected_sql = "SELECT 'one', *, 'two', * FROM test1"
        expected_json = {'from': 'test1',
                         'select': [{'value': {'literal': 'one'}}, '*', {'value': {'literal': 'two'}}, '*']}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_014(self):
        expected_sql = "SELECT *, 'hi' FROM test1, test2"
        expected_json = {'from': ['test1', 'test2'], 'select': ['*', {'value': {'literal': 'hi'}}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_015(self):
        expected_sql = "SELECT 'one', *, 'two', * FROM test1, test2"
        expected_json = {'from': ['test1', 'test2'],
                         'select': [{'value': {'literal': 'one'}}, '*', {'value': {'literal': 'two'}}, '*']}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_016(self):
        expected_sql = "SELECT test1.f1, test2.r1 FROM test1, test2"
        expected_json = {'from': ['test1', 'test2'], 'select': [{'value': 'test1.f1'}, {'value': 'test2.r1'}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_017(self):
        expected_sql = "SELECT test1.f1, test2.r1 FROM test2, test1"
        expected_json = {'from': ['test2', 'test1'], 'select': [{'value': 'test1.f1'}, {'value': 'test2.r1'}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_019(self):
        expected_sql = "SELECT * FROM test1 AS a, test1 AS b"
        expected_json = {'from': [{'value': 'test1', 'name': 'a'}, {'value': 'test1', 'name': 'b'}], 'select': '*'}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_020(self):
        expected_sql = "SELECT max(test1.f1,test2.r1), min(test1.f2,test2.r2) FROM test2, test1"
        expected_json = {'from': ['test2', 'test1'], 'select': [{'value': {'max': ['test1.f1', 'test2.r1']}},
                                                                {'value': {'min': ['test1.f2', 'test2.r2']}}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_021(self):
        expected_sql = "SELECT min(test1.f1,test2.r1), max(test1.f2,test2.r2) FROM test1, test2"
        expected_json = {'from': ['test1', 'test2'], 'select': [{'value': {'min': ['test1.f1', 'test2.r1']}},
                                                                {'value': {'max': ['test1.f2', 'test2.r2']}}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_022(self):
        expected_sql = "SELECT count(f1,f2) FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'count': ['f1', 'f2']}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_023(self):
        expected_sql = "SELECT count(f1) FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'count': 'f1'}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_024(self):
        expected_sql = "SELECT Count() FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'count': {}}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_025(self):
        expected_sql = "SELECT COUNT(*) FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'count': '*'}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_026(self):
        expected_sql = "SELECT COUNT(*)+1 FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'add': [{'count': '*'}, 1]}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_027(self):
        expected_sql = "SELECT count(*),count(a),count(b) FROM t3"
        expected_json = {'from': 't3',
                         'select': [{'value': {'count': '*'}}, {'value': {'count': 'a'}}, {'value': {'count': 'b'}}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_028(self):
        expected_sql = "SELECT count(*),count(a),count(b) FROM t4"
        expected_json = {'from': 't4',
                         'select': [{'value': {'count': '*'}}, {'value': {'count': 'a'}}, {'value': {'count': 'b'}}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_029(self):
        expected_sql = "SELECT count(*),count(a),count(b) FROM t4 WHERE b=5"
        expected_json = {'from': 't4',
                         'select': [{'value': {'count': '*'}}, {'value': {'count': 'a'}}, {'value': {'count': 'b'}}],
                         'where': {'eq': ['b', 5]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_030(self):
        expected_sql = "SELECT min(*) FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'min': '*'}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_031(self):
        expected_sql = "SELECT Min(f1) FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'min': 'f1'}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_032(self):
        expected_sql = "SELECT MIN(f1,f2) FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'min': ['f1', 'f2']}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_033(self):
        expected_sql = "SELECT coalesce(min(a),'xyzzy') FROM t3"
        expected_json = {'from': 't3', 'select': {'value': {'coalesce': [{'min': 'a'}, {'literal': 'xyzzy'}]}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_034(self):
        expected_sql = "SELECT min(coalesce(a,'xyzzy')) FROM t3"
        expected_json = {'from': 't3', 'select': {'value': {'min': {'coalesce': ['a', {'literal': 'xyzzy'}]}}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_035(self):
        expected_sql = "SELECT min(b), min(b) FROM t4"
        expected_json = {'from': 't4', 'select': [{'value': {'min': 'b'}}, {'value': {'min': 'b'}}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_036(self):
        expected_sql = "SELECT MAX(*) FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'max': '*'}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_037(self):
        expected_sql = "SELECT Max(f1) FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'max': 'f1'}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_038(self):
        expected_sql = "SELECT max(f1,f2) FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'max': ['f1', 'f2']}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_039(self):
        expected_sql = "SELECT MAX(f1,f2)+1 FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'add': [{'max': ['f1', 'f2']}, 1]}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_040(self):
        expected_sql = "SELECT MAX(f1)+1 FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'add': [{'max': 'f1'}, 1]}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_041(self):
        expected_sql = "SELECT coalesce(max(a),'xyzzy') FROM t3"
        expected_json = {'from': 't3', 'select': {'value': {'coalesce': [{'max': 'a'}, {'literal': 'xyzzy'}]}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_042(self):
        expected_sql = "SELECT max(coalesce(a,'xyzzy')) FROM t3"
        expected_json = {'from': 't3', 'select': {'value': {'max': {'coalesce': ['a', {'literal': 'xyzzy'}]}}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_043(self):
        expected_sql = "SELECT SUM(*) FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'sum': '*'}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_044(self):
        expected_sql = "SELECT Sum(f1) FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'sum': 'f1'}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_045(self):
        expected_sql = "SELECT sum(f1,f2) FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'sum': ['f1', 'f2']}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_046(self):
        expected_sql = "SELECT SUM(f1)+1 FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'add': [{'sum': 'f1'}, 1]}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_047(self):
        expected_sql = "SELECT sum(a) FROM t3"
        expected_json = {'from': 't3', 'select': {'value': {'sum': 'a'}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_048(self):
        expected_sql = "SELECT XYZZY(f1) FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'xyzzy': 'f1'}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_049(self):
        expected_sql = "SELECT SUM(min(f1,f2)) FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'sum': {'min': ['f1', 'f2']}}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_050(self):
        expected_sql = "SELECT SUM(min(f1)) FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'sum': {'min': 'f1'}}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_052(self):
        expected_sql = "SELECT f1 FROM test1 WHERE f1<11"
        expected_json = {'from': 'test1', 'select': {'value': 'f1'}, 'where': {'lt': ['f1', 11]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_053(self):
        expected_sql = "SELECT f1 FROM test1 WHERE f1<=11"
        expected_json = {'from': 'test1', 'select': {'value': 'f1'}, 'where': {'lte': ['f1', 11]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_054(self):
        expected_sql = "SELECT f1 FROM test1 WHERE f1=11"
        expected_json = {'from': 'test1', 'select': {'value': 'f1'}, 'where': {'eq': ['f1', 11]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_055(self):
        expected_sql = "SELECT f1 FROM test1 WHERE f1>=11"
        expected_json = {'from': 'test1', 'select': {'value': 'f1'}, 'where': {'gte': ['f1', 11]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_056(self):
        expected_sql = "SELECT f1 FROM test1 WHERE f1>11"
        expected_json = {'from': 'test1', 'select': {'value': 'f1'}, 'where': {'gt': ['f1', 11]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_057(self):
        expected_sql = "SELECT f1 FROM test1 WHERE f1!=11"
        expected_json = {'from': 'test1', 'select': {'value': 'f1'}, 'where': {'neq': ['f1', 11]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_058(self):
        expected_sql = "SELECT f1 FROM test1 WHERE min(f1,f2)!=11"
        expected_json = {'from': 'test1', 'select': {'value': 'f1'}, 'where': {'neq': [{'min': ['f1', 'f2']}, 11]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_059(self):
        expected_sql = "SELECT f1 FROM test1 WHERE max(f1,f2)!=11"
        expected_json = {'from': 'test1', 'select': {'value': 'f1'}, 'where': {'neq': [{'max': ['f1', 'f2']}, 11]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_060(self):
        expected_sql = "SELECT f1 FROM test1 WHERE count(f1,f2)!=11"
        expected_json = {'from': 'test1', 'select': {'value': 'f1'}, 'where': {'neq': [{'count': ['f1', 'f2']}, 11]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_061(self):
        expected_sql = "SELECT f1 FROM test1 ORDER BY f1"
        expected_json = {'from': 'test1', 'select': {'value': 'f1'}, 'orderby': {'value': 'f1'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_062(self):
        expected_sql = "SELECT f1 FROM test1 ORDER BY -f1"
        expected_json = {'from': 'test1', 'select': {'value': 'f1'}, 'orderby': {'value': {'neg': 'f1'}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_063(self):
        expected_sql = "SELECT f1 FROM test1 ORDER BY min(f1,f2)"
        expected_json = {'from': 'test1', 'select': {'value': 'f1'}, 'orderby': {'value': {'min': ['f1', 'f2']}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_064(self):
        expected_sql = "SELECT f1 FROM test1 ORDER BY min(f1)"
        expected_json = {'from': 'test1', 'select': {'value': 'f1'}, 'orderby': {'value': {'min': 'f1'}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_065(self):
        expected_sql = "SELECT f1 FROM test1 ORDER BY 8.4"
        expected_json = {'from': 'test1', 'select': {'value': 'f1'}, 'orderby': {'value': 8.4}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_066(self):
        expected_sql = "SELECT f1 FROM test1 ORDER BY '8.4'"
        expected_json = {'from': 'test1', 'select': {'value': 'f1'}, 'orderby': {'value': {'literal': '8.4'}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_067(self):
        expected_sql = "SELECT * FROM t5 ORDER BY 1"
        expected_json = {'from': 't5', 'select': '*', 'orderby': {'value': 1}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_068(self):
        expected_sql = "SELECT * FROM t5 ORDER BY 2"
        expected_json = {'from': 't5', 'select': '*', 'orderby': {'value': 2}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_069(self):
        expected_sql = "SELECT * FROM t5 ORDER BY +2"
        expected_json = {'from': 't5', 'select': '*', 'orderby': {'value': 2}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_070(self):
        expected_sql = "SELECT * FROM t5 ORDER BY 2, 1 DESC"
        expected_json = {'from': 't5', 'select': '*', 'orderby': [{'value': 2}, {'value': 1, 'sort': 'desc'}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_071(self):
        expected_sql = "SELECT * FROM t5 ORDER BY 1 DESC, b"
        expected_json = {'from': 't5', 'select': '*', 'orderby': [{'value': 1, 'sort': 'desc'}, {'value': 'b'}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_072(self):
        expected_sql = "SELECT * FROM t5 ORDER BY b DESC, 1"
        expected_json = {'from': 't5', 'select': '*', 'orderby': [{'value': 'b', 'sort': 'desc'}, {'value': 1}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_073(self):
        expected_sql = "SELECT max(f1) FROM test1 ORDER BY f2"
        expected_json = {'from': 'test1', 'select': {'value': {'max': 'f1'}}, 'orderby': {'value': 'f2'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_078(self):
        expected_sql = "SELECT A.f1, B.f1 FROM test1 as A, test1 as B ORDER BY A.f1, B.f1"
        expected_json = {'from': [{'value': 'test1', 'name': 'A'}, {'value': 'test1', 'name': 'B'}],
                         'select': [{'value': 'A.f1'}, {'value': 'B.f1'}],
                         'orderby': [{'value': 'A.f1'}, {'value': 'B.f1'}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_086(self):
        expected_sql = "SELECT a FROM t6 WHERE b IN (SELECT b FROM t6 WHERE a<='b' UNION SELECT '3' AS x ORDER BY 1 LIMIT 1)"
        expected_json = {'from': 't6', 'select': {'value': 'a'}, 'where': {'in': ['b', {'from': {
            'union': [{'from': 't6', 'select': {'value': 'b'}, 'where': {'lte': ['a', {'literal': 'b'}]}},
                      {'select': {'value': {'literal': '3'}, 'name': 'x'}}]}, 'orderby': {'value': 1}, 'limit': 1}]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_087(self):
        expected_sql = "SELECT a FROM t6 WHERE b IN (SELECT b FROM t6 WHERE a<='b' UNION SELECT '3' AS x ORDER BY 1 DESC LIMIT 1)"
        expected_json = {'from': 't6', 'select': {'value': 'a'}, 'where': {'in': ['b', {'from': {
            'union': [{'from': 't6', 'select': {'value': 'b'}, 'where': {'lte': ['a', {'literal': 'b'}]}},
                      {'select': {'value': {'literal': '3'}, 'name': 'x'}}]}, 'orderby': {'value': 1, 'sort': 'desc'},
                                                                                        'limit': 1}]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_088(self):
        expected_sql = "SELECT a FROM t6 WHERE b IN (SELECT b FROM t6 WHERE a<='b' UNION SELECT '3' AS x ORDER BY b LIMIT 2) ORDER BY a"
        expected_json = {'from': 't6', 'select': {'value': 'a'}, 'where': {'in': ['b', {'from': {
            'union': [{'from': 't6', 'select': {'value': 'b'}, 'where': {'lte': ['a', {'literal': 'b'}]}},
                      {'select': {'value': {'literal': '3'}, 'name': 'x'}}]}, 'orderby': {'value': 'b'}, 'limit': 2}]},
                         'orderby': {'value': 'a'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_089(self):
        expected_sql = "SELECT a FROM t6 WHERE b IN (SELECT b FROM t6 WHERE a<='b' UNION SELECT '3' AS x ORDER BY x DESC LIMIT 2) ORDER BY a"
        expected_json = {'from': 't6', 'select': {'value': 'a'}, 'where': {'in': ['b', {'from': {
            'union': [{'from': 't6', 'select': {'value': 'b'}, 'where': {'lte': ['a', {'literal': 'b'}]}},
                      {'select': {'value': {'literal': '3'}, 'name': 'x'}}]}, 'orderby': {'value': 'x', 'sort': 'desc'},
                                                                                        'limit': 2}]},
                         'orderby': {'value': 'a'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_093(self):
        expected_sql = "SELECT count(f1,f2) FROM test1"
        expected_json = {'from': 'test1', 'select': {'value': {'count': ['f1', 'f2']}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_094(self):
        expected_sql = "SELECT f1 FROM test1 ORDER BY f2, f1"
        expected_json = {'from': 'test1', 'select': {'value': 'f1'}, 'orderby': [{'value': 'f2'}, {'value': 'f1'}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_095(self):
        expected_sql = "SELECT f1 FROM test1 WHERE 4.3+2.4 OR 1 ORDER BY f1"
        expected_json = {'from': 'test1', 'select': {'value': 'f1'}, 'where': {'or': [{'add': [4.3, 2.4]}, 1]},
                         'orderby': {'value': 'f1'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_096(self):
        expected_sql = "SELECT f1 FROM test1 WHERE ('x' || f1) BETWEEN 'x10' AND 'x20' ORDER BY f1"
        expected_json = {'from': 'test1', 'select': {'value': 'f1'}, 'where': {
            'between': [{'concat': [{'literal': 'x'}, 'f1']}, {'literal': 'x10'}, {'literal': 'x20'}]},
                         'orderby': {'value': 'f1'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_097(self):
        expected_sql = "SELECT f1 FROM test1 WHERE 5-3==2 ORDER BY f1"
        expected_json = {'from': 'test1', 'select': {'value': 'f1'}, 'where': {'eq': [{'sub': [5, 3]}, 2]},
                         'orderby': {'value': 'f1'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    @skip("maximum recursion depth exceeded while calling a Python object")
    def test_098(self):
        expected_sql = "SELECT coalesce(f1/(f1-11),'x'), coalesce(min(f1/(f1-11),5),'y'), coalesce(max(f1/(f1-33),6),'z') FROM test1 ORDER BY f1"
        expected_json = {'from': 'test1', 'orderby': {'value': 'f1'},
                         'select': [{'value': {'coalesce': [{'div': ['f1', {'sub': ['f1', 11]}]}, {'literal': 'x'}]}}, {
                             'value': {
                                 'coalesce': [{'min': [{'div': ['f1', {'sub': ['f1', 11]}]}, 5]}, {'literal': 'y'}]}}, {
                                        'value': {'coalesce': [{'max': [{'div': ['f1', {'sub': ['f1', 33]}]}, 6]},
                                                               {'literal': 'z'}]}}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_099(self):
        expected_sql = "SELECT min(1,2,3), -max(1,2,3) FROM test1 ORDER BY f1"
        expected_json = {'from': 'test1', 'orderby': {'value': 'f1'},
                         'select': [{'value': {'min': [1, 2, 3]}}, {'value': {'neg': {'max': [1, 2, 3]}}}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_100(self):
        expected_sql = "SELECT * FROM test1 WHERE f1<0"
        expected_json = {'from': 'test1', 'select': '*', 'where': {'lt': ['f1', 0]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    @skip("Need to figure out how to wrap the select in ()")
    def test_103(self):
        expected_sql = "SELECT * FROM test1 WHERE f1<(select count(*) from test2)"
        expected_json = {'from': 'test1', 'select': '*',
                         'where': {'lt': ['f1', {'from': 'test2', 'select': {'value': {'count': '*'}}}]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_104(self):
        expected_sql = "SELECT * FROM test1 ORDER BY f1"
        expected_json = {'from': 'test1', 'select': '*', 'orderby': {'value': 'f1'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_105(self):
        expected_sql = "SELECT * FROM test1 WHERE f1<0 ORDER BY f1"
        expected_json = {'from': 'test1', 'select': '*', 'where': {'lt': ['f1', 0]}, 'orderby': {'value': 'f1'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_106(self):
        expected_sql = "SELECT f1 AS x FROM test1 ORDER BY x"
        expected_json = {'from': 'test1', 'select': {'value': 'f1', 'name': 'x'}, 'orderby': {'value': 'x'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_107(self):
        expected_sql = "SELECT f1 AS x FROM test1 ORDER BY -x"
        expected_json = {'from': 'test1', 'select': {'value': 'f1', 'name': 'x'}, 'orderby': {'value': {'neg': 'x'}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_108(self):
        expected_sql = "SELECT f1-23 AS x FROM test1 ORDER BY abs(x)"
        expected_json = {'from': 'test1', 'select': {'value': {'sub': ['f1', 23]}, 'name': 'x'},
                         'orderby': {'value': {'abs': 'x'}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_109(self):
        expected_sql = "SELECT f1-23 AS x FROM test1 ORDER BY -abs(x)"
        expected_json = {'from': 'test1', 'select': {'value': {'sub': ['f1', 23]}, 'name': 'x'},
                         'orderby': {'value': {'neg': {'abs': 'x'}}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_110(self):
        expected_sql = "SELECT f1-22 AS x, f2-22 as y FROM test1"
        expected_json = {'from': 'test1', 'select': [{'value': {'sub': ['f1', 22]}, 'name': 'x'},
                                                     {'value': {'sub': ['f2', 22]}, 'name': 'y'}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_111(self):
        expected_sql = "SELECT f1-22 AS x, f2-22 as y FROM test1 WHERE x>0 AND y<50"
        expected_json = {'from': 'test1', 'select': [{'value': {'sub': ['f1', 22]}, 'name': 'x'},
                                                     {'value': {'sub': ['f2', 22]}, 'name': 'y'}],
                         'where': {'and': [{'gt': ['x', 0]}, {'lt': ['y', 50]}]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_112(self):
        expected_sql = "SELECT f1 COLLATE nocase AS x FROM test1 ORDER BY x"
        expected_json = {'from': 'test1', 'select': {'value': {'collate nocase': 'f1'}}, 'orderby': {'value': 'x'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_113(self):
        expected_sql = "SELECT * FROM t3, t4"
        expected_json = {'from': ['t3', 't4'], 'select': '*'}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_114(self):
        expected_sql = "SELECT t3.*, t4.b FROM t3, t4"
        expected_json = {'from': ['t3', 't4'], 'select': [{'value': 't3.*'}, {'value': 't4.b'}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_115(self):
        expected_sql = "SELECT t3.*, t4.b FROM t3, t4"
        expected_json = {'from': ['t3', 't4'], 'select': [{'value': 't3.*'}, {'value': 't4.b'}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_116(self):
        expected_sql = "SELECT t3.b, t4.* FROM t3, t4"
        expected_json = {'from': ['t3', 't4'], 'select': [{'value': 't3.b'}, {'value': 't4.*'}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_118b(self):
        expected_sql = "SELECT * FROM t3 UNION SELECT 3 AS a, 4 ORDER BY a"
        expected_json = {
            'from': {'union': [{'from': 't3', 'select': '*'}, {'select': [{'value': 3, 'name': 'a'}, {'value': 4}]}]},
            'orderby': {'value': 'a'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_118c(self):
        expected_sql = "SELECT * FROM t3 UNION SELECT 3 AS a, 4 ORDER BY a"
        expected_json = {
            'from': {'union': [{'from': 't3', 'select': '*'}, {'select': [{'value': 3, 'name': 'a'}, {'value': 4}]}]},
            'orderby': {'value': 'a'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_119(self):
        expected_sql = "SELECT 3, 4 UNION SELECT * FROM t3"
        expected_json = {'union': [{'select': [{'value': 3}, {'value': 4}]}, {'from': 't3', 'select': '*'}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    @skip("Need to figure out how to wrap the select in ()")
    def test_120(self):
        expected_sql = "SELECT * FROM t3 WHERE a=(SELECT 1)"
        expected_json = {'from': 't3', 'select': '*', 'where': {'eq': ['a', {'select': {'value': 1}}]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    @skip("Need to figure out how to wrap the select in ()")
    def test_121(self):
        expected_sql = "SELECT * FROM t3 WHERE a=(SELECT 2)"
        expected_json = {'from': 't3', 'select': '*', 'where': {'eq': ['a', {'select': {'value': 2}}]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_125(self):
        expected_sql = "SELECT count( (SELECT a FROM abc WHERE a = NULL AND b >= upper.c)) FROM abc AS upper"
        expected_json = {'from': {'value': 'abc', 'name': 'upper'}, 'select': {'value': {
            'count': {'from': 'abc', 'select': {'value': 'a'},
                      'where': {'and': [{'missing': 'a'}, {'gte': ['b', 'upper.c']}]}}}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_126(self):
        expected_sql = "SELECT name FROM sqlite_master WHERE type = 'table'"
        expected_json = {'from': 'sqlite_master', 'select': {'value': 'name'},
                         'where': {'eq': ['type', {'literal': 'table'}]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_128(self):
        expected_sql = "SELECT 10 IN (SELECT rowid FROM sqlite_master)"
        expected_json = {'select': {'value': {'in': [10, {'from': 'sqlite_master', 'select': {'value': 'rowid'}}]}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_131(self):
        expected_sql = "SELECT 2 IN (SELECT a FROM t1)"
        expected_json = {'select': {'value': {'in': [2, {'from': 't1', 'select': {'value': 'a'}}]}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_139(self):
        expected_sql = "SELECT count(*) FROM tbl2"
        expected_json = {'from': 'tbl2', 'select': {'value': {'count': '*'}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_140(self):
        expected_sql = "SELECT count(*) FROM tbl2 WHERE f2>1000"
        expected_json = {'from': 'tbl2', 'select': {'value': {'count': '*'}}, 'where': {'gt': ['f2', 1000]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_141(self):
        expected_sql = "SELECT f1 FROM tbl2 WHERE 1000=f2"
        expected_json = {'from': 'tbl2', 'select': {'value': 'f1'}, 'where': {'eq': [1000, 'f2']}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_144(self):
        expected_sql = "SELECT f1 FROM tbl2 WHERE f2=1000"
        expected_json = {'from': 'tbl2', 'select': {'value': 'f1'}, 'where': {'eq': ['f2', 1000]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_145(self):
        expected_sql = "SELECT * FROM tbl2 WHERE 1000=f2"
        expected_json = {'from': 'tbl2', 'select': '*', 'where': {'eq': [1000, 'f2']}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_146(self):
        expected_sql = "SELECT * FROM tbl2 WHERE f2=1000"
        expected_json = {'from': 'tbl2', 'select': '*', 'where': {'eq': ['f2', 1000]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_148(self):
        expected_sql = "SELECT f1 FROM tbl2 WHERE f2==2000"
        expected_json = {'from': 'tbl2', 'select': {'value': 'f1'}, 'where': {'eq': ['f2', 2000]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_150(self):
        expected_sql = "SELECT * FROM aa CROSS JOIN bb WHERE b"
        expected_json = {'from': ['aa', {'cross join': 'bb'}], 'select': '*', 'where': 'b'}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_151(self):
        expected_sql = "SELECT * FROM aa CROSS JOIN bb WHERE NOT b"
        expected_json = {'from': ['aa', {'cross join': 'bb'}], 'select': '*', 'where': {'not': 'b'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_152(self):
        expected_sql = "SELECT * FROM aa, bb WHERE min(a,b)"
        expected_json = {'from': ['aa', 'bb'], 'select': '*', 'where': {'min': ['a', 'b']}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_153(self):
        expected_sql = "SELECT * FROM aa, bb WHERE NOT min(a,b)"
        expected_json = {'from': ['aa', 'bb'], 'select': '*', 'where': {'not': {'min': ['a', 'b']}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_154(self):
        expected_sql = "SELECT * FROM aa, bb WHERE CASE WHEN a=b-1 THEN 1 END"
        expected_json = {'from': ['aa', 'bb'], 'select': '*',
                         'where': {'case': {'when': {'eq': ['a', {'sub': ['b', 1]}]}, 'then': 1}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_155(self):
        expected_sql = "SELECT * FROM aa, bb WHERE CASE WHEN a=b-1 THEN 0 ELSE 1 END"
        expected_json = {'from': ['aa', 'bb'], 'select': '*',
                         'where': {'case': [{'when': {'eq': ['a', {'sub': ['b', 1]}]}, 'then': 0}, 1]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_158(self):
        expected_sql = "SELECT DISTINCT log FROM t1 ORDER BY log"
        expected_json = {'from': 't1', 'select': {'value': {'distinct': 'log'}}, 'orderby': {'value': 'log'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_160(self):
        expected_sql = "SELECT min(n),min(log),max(n),max(log),sum(n),sum(log),avg(n),avg(log) FROM t1"
        expected_json = {'from': 't1',
                         'select': [{'value': {'min': 'n'}}, {'value': {'min': 'log'}}, {'value': {'max': 'n'}},
                                    {'value': {'max': 'log'}}, {'value': {'sum': 'n'}}, {'value': {'sum': 'log'}},
                                    {'value': {'avg': 'n'}}, {'value': {'avg': 'log'}}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_161(self):
        expected_sql = "SELECT max(n)/avg(n), max(log)/avg(log) FROM t1"
        expected_json = {'from': 't1', 'select': [{'value': {'div': [{'max': 'n'}, {'avg': 'n'}]}},
                                                  {'value': {'div': [{'max': 'log'}, {'avg': 'log'}]}}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_162(self):
        expected_sql = "SELECT log, count(*) FROM t1 GROUP BY log ORDER BY log"
        expected_json = {'from': 't1', 'select': [{'value': 'log'}, {'value': {'count': '*'}}],
                         'groupby': {'value': 'log'}, 'orderby': {'value': 'log'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_163(self):
        expected_sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY log"
        expected_json = {'from': 't1', 'select': [{'value': 'log'}, {'value': {'min': 'n'}}],
                         'groupby': {'value': 'log'}, 'orderby': {'value': 'log'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_164(self):
        expected_sql = "SELECT log, avg(n) FROM t1 GROUP BY log ORDER BY log"
        expected_json = {'from': 't1', 'select': [{'value': 'log'}, {'value': {'avg': 'n'}}],
                         'groupby': {'value': 'log'}, 'orderby': {'value': 'log'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_165(self):
        expected_sql = "SELECT log, avg(n)+1 FROM t1 GROUP BY log ORDER BY log"
        expected_json = {'from': 't1', 'select': [{'value': 'log'}, {'value': {'add': [{'avg': 'n'}, 1]}}],
                         'groupby': {'value': 'log'}, 'orderby': {'value': 'log'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_166(self):
        expected_sql = "SELECT log, avg(n)-min(n) FROM t1 GROUP BY log ORDER BY log"
        expected_json = {'from': 't1', 'select': [{'value': 'log'}, {'value': {'sub': [{'avg': 'n'}, {'min': 'n'}]}}],
                         'groupby': {'value': 'log'}, 'orderby': {'value': 'log'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_167(self):
        expected_sql = "SELECT log*2+1, avg(n)-min(n) FROM t1 GROUP BY log ORDER BY log"
        expected_json = {'from': 't1', 'select': [{'value': {'add': [{'mult': ['log', 2]}, 1]}},
                                                  {'value': {'sub': [{'avg': 'n'}, {'min': 'n'}]}}],
                         'groupby': {'value': 'log'}, 'orderby': {'value': 'log'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_168(self):
        expected_sql = "SELECT log*2+1 as x, count(*) FROM t1 GROUP BY x ORDER BY x"
        expected_json = {'from': 't1', 'select': [{'value': {'add': [{'mult': ['log', 2]}, 1]}, 'name': 'x'},
                                                  {'value': {'count': '*'}}], 'groupby': {'value': 'x'},
                         'orderby': {'value': 'x'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_169(self):
        expected_sql = "SELECT log*2+1 AS x, count(*) AS y FROM t1 GROUP BY x ORDER BY y, x"
        expected_json = {'from': 't1', 'select': [{'value': {'add': [{'mult': ['log', 2]}, 1]}, 'name': 'x'},
                                                  {'value': {'count': '*'}, 'name': 'y'}], 'groupby': {'value': 'x'},
                         'orderby': [{'value': 'y'}, {'value': 'x'}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_170(self):
        expected_sql = "SELECT log*2+1 AS x, count(*) AS y FROM t1 GROUP BY x ORDER BY 10-(x+y)"
        expected_json = {'from': 't1', 'select': [{'value': {'add': [{'mult': ['log', 2]}, 1]}, 'name': 'x'},
                                                  {'value': {'count': '*'}, 'name': 'y'}], 'groupby': {'value': 'x'},
                         'orderby': {'value': {'sub': [10, {'add': ['x', 'y']}]}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_171(self):
        expected_sql = "SELECT log, count(*) FROM t1 GROUP BY something HAVING log>=4"
        expected_json = {'from': 't1', 'select': [{'value': 'log'}, {'value': {'count': '*'}}],
                         'groupby': {'value': 'something'}, 'having': {'gte': ['log', 4]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_172(self):
        expected_sql = "SELECT log, count(*) FROM t1 GROUP BY log HAVING log>=4 ORDER BY log"
        expected_json = {'from': 't1', 'select': [{'value': 'log'}, {'value': {'count': '*'}}],
                         'groupby': {'value': 'log'}, 'having': {'gte': ['log', 4]}, 'orderby': {'value': 'log'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_173(self):
        expected_sql = "SELECT log, count(*) FROM t1 GROUP BY log HAVING count(*)>=4 ORDER BY log"
        expected_json = {'from': 't1', 'select': [{'value': 'log'}, {'value': {'count': '*'}}],
                         'groupby': {'value': 'log'}, 'having': {'gte': [{'count': '*'}, 4]},
                         'orderby': {'value': 'log'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_174(self):
        expected_sql = "SELECT log, count(*) FROM t1 GROUP BY log HAVING count(*)>=4 ORDER BY max(n)+0"
        expected_json = {'from': 't1', 'select': [{'value': 'log'}, {'value': {'count': '*'}}],
                         'groupby': {'value': 'log'}, 'having': {'gte': [{'count': '*'}, 4]},
                         'orderby': {'value': {'add': [{'max': 'n'}, 0]}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_175(self):
        expected_sql = "SELECT log AS x, count(*) AS y FROM t1 GROUP BY x HAVING y>=4 ORDER BY max(n)+0"
        expected_json = {'from': 't1',
                         'select': [{'value': 'log', 'name': 'x'}, {'value': {'count': '*'}, 'name': 'y'}],
                         'groupby': {'value': 'x'}, 'having': {'gte': ['y', 4]},
                         'orderby': {'value': {'add': [{'max': 'n'}, 0]}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_176(self):
        expected_sql = "SELECT log AS x FROM t1 GROUP BY x HAVING count(*)>=4 ORDER BY max(n)+0"
        expected_json = {'from': 't1', 'select': {'value': 'log', 'name': 'x'}, 'groupby': {'value': 'x'},
                         'having': {'gte': [{'count': '*'}, 4]}, 'orderby': {'value': {'add': [{'max': 'n'}, 0]}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_177(self):
        expected_sql = "SELECT log, count(*), avg(n), max(n+log*2) FROM t1 GROUP BY log ORDER BY max(n+log*2)+0, avg(n)+0"
        expected_json = {'from': 't1', 'select': [{'value': 'log'}, {'value': {'count': '*'}}, {'value': {'avg': 'n'}},
                                                  {'value': {'max': {'add': ['n', {'mult': ['log', 2]}]}}}],
                         'groupby': {'value': 'log'},
                         'orderby': [{'value': {'add': [{'max': {'add': ['n', {'mult': ['log', 2]}]}}, 0]}},
                                     {'value': {'add': [{'avg': 'n'}, 0]}}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_178(self):
        expected_sql = "SELECT log, count(*), avg(n), max(n+log*2) FROM t1 GROUP BY log ORDER BY max(n+log*2)+0, min(log,avg(n))+0"
        expected_json = {'from': 't1', 'select': [{'value': 'log'}, {'value': {'count': '*'}}, {'value': {'avg': 'n'}},
                                                  {'value': {'max': {'add': ['n', {'mult': ['log', 2]}]}}}],
                         'groupby': {'value': 'log'},
                         'orderby': [{'value': {'add': [{'max': {'add': ['n', {'mult': ['log', 2]}]}}, 0]}},
                                     {'value': {'add': [{'min': ['log', {'avg': 'n'}]}, 0]}}]}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_179(self):
        expected_sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY log"
        expected_json = {'from': 't1', 'select': [{'value': 'log'}, {'value': {'min': 'n'}}],
                         'groupby': {'value': 'log'}, 'orderby': {'value': 'log'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_180(self):
        expected_sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY log DESC"
        expected_json = {'from': 't1', 'select': [{'value': 'log'}, {'value': {'min': 'n'}}],
                         'groupby': {'value': 'log'}, 'orderby': {'value': 'log', 'sort': 'desc'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_181(self):
        expected_sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY 1"
        expected_json = {'from': 't1', 'select': [{'value': 'log'}, {'value': {'min': 'n'}}],
                         'groupby': {'value': 'log'}, 'orderby': {'value': 1}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_183(self):
        expected_sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY log"
        expected_json = {'from': 't1', 'select': [{'value': 'log'}, {'value': {'min': 'n'}}],
                         'groupby': {'value': 'log'}, 'orderby': {'value': 'log'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_184(self):
        expected_sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY log DESC"
        expected_json = {'from': 't1', 'select': [{'value': 'log'}, {'value': {'min': 'n'}}],
                         'groupby': {'value': 'log'}, 'orderby': {'value': 'log', 'sort': 'desc'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_185(self):
        expected_sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY 1"
        expected_json = {'from': 't1', 'select': [{'value': 'log'}, {'value': {'min': 'n'}}],
                         'groupby': {'value': 'log'}, 'orderby': {'value': 1}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_186(self):
        expected_sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY 1 DESC"
        expected_json = {'from': 't1', 'select': [{'value': 'log'}, {'value': {'min': 'n'}}],
                         'groupby': {'value': 'log'}, 'orderby': {'value': 1, 'sort': 'desc'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_187(self):
        expected_sql = "SELECT a, sum(b) FROM t2 WHERE b=5 GROUP BY a"
        expected_json = {'from': 't2', 'select': [{'value': 'a'}, {'value': {'sum': 'b'}}], 'groupby': {'value': 'a'},
                         'where': {'eq': ['b', 5]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_188(self):
        expected_sql = "SELECT a, sum(b) FROM t2 WHERE b=5"
        expected_json = {'from': 't2', 'select': [{'value': 'a'}, {'value': {'sum': 'b'}}], 'where': {'eq': ['b', 5]}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_189(self):
        expected_sql = "SELECT typeof(sum(a3)) FROM a"
        expected_json = {'from': 'a', 'select': {'value': {'typeof': {'sum': 'a3'}}}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

    def test_190(self):
        expected_sql = "SELECT typeof(sum(a3)) FROM a GROUP BY a1"
        expected_json = {'from': 'a', 'select': {'value': {'typeof': {'sum': 'a3'}}}, 'groupby': {'value': 'a1'}}
        new_sql = ""
        new_json = ""
        try:
            new_sql = format(expected_json)
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

