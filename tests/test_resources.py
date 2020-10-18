# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Author: Kyle Lahnakoski (kyle@lahnakoski.com)
#
from __future__ import absolute_import, division, unicode_literals

import os
from unittest import TestCase, skipIf

from moz_sql_parser import parse

IS_MASTER = os.environ.get("TRAVIS_BRANCH") == "master"


class TestResources(TestCase):
    def test_001(self):
        sql = "SELECT * FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": "*"}
        self.assertEqual(result, expected)

    def test_002(self):
        sql = "SELECT * FROM test1, test2"
        result = parse(sql)
        expected = {"from": ["test1", "test2"], "select": "*"}
        self.assertEqual(result, expected)

    def test_003(self):
        sql = "SELECT * FROM test2, test1"
        result = parse(sql)
        expected = {"from": ["test2", "test1"], "select": "*"}
        self.assertEqual(result, expected)

    def test_004(self):
        sql = "SELECT f1 FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": "f1"}}
        self.assertEqual(result, expected)

    def test_005(self):
        sql = "SELECT f2 FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": "f2"}}
        self.assertEqual(result, expected)

    def test_006(self):
        sql = "SELECT f2, f1 FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": [{"value": "f2"}, {"value": "f1"}]}
        self.assertEqual(result, expected)

    def test_007(self):
        sql = "SELECT f1, f2 FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": [{"value": "f1"}, {"value": "f2"}]}
        self.assertEqual(result, expected)

    def test_008(self):
        sql = "SELECT *, * FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": ["*", "*"]}
        self.assertEqual(result, expected)

    def test_009(self):
        sql = "SELECT *, min(f1,f2), max(f1,f2) FROM test1"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": [
                "*",
                {"value": {"min": ["f1", "f2"]}},
                {"value": {"max": ["f1", "f2"]}},
            ],
        }
        self.assertEqual(result, expected)

    def test_010(self):
        sql = "SELECT 'one', *, 'two', * FROM test1"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": [
                {"value": {"literal": "one"}},
                "*",
                {"value": {"literal": "two"}},
                "*",
            ],
        }
        self.assertEqual(result, expected)

    def test_014(self):
        sql = "SELECT *, 'hi' FROM test1, test2"
        result = parse(sql)
        expected = {
            "from": ["test1", "test2"],
            "select": ["*", {"value": {"literal": "hi"}}],
        }
        self.assertEqual(result, expected)

    def test_015(self):
        sql = "SELECT 'one', *, 'two', * FROM test1, test2"
        result = parse(sql)
        expected = {
            "from": ["test1", "test2"],
            "select": [
                {"value": {"literal": "one"}},
                "*",
                {"value": {"literal": "two"}},
                "*",
            ],
        }
        self.assertEqual(result, expected)

    def test_016(self):
        sql = "SELECT test1.f1, test2.r1 FROM test1, test2"
        result = parse(sql)
        expected = {
            "from": ["test1", "test2"],
            "select": [{"value": "test1.f1"}, {"value": "test2.r1"}],
        }
        self.assertEqual(result, expected)

    def test_017(self):
        sql = "SELECT test1.f1, test2.r1 FROM test2, test1"
        result = parse(sql)
        expected = {
            "from": ["test2", "test1"],
            "select": [{"value": "test1.f1"}, {"value": "test2.r1"}],
        }
        self.assertEqual(result, expected)

    def test_019(self):
        sql = "SELECT * FROM test1 AS a, test1 AS b"
        result = parse(sql)
        expected = {
            "from": [{"value": "test1", "name": "a"}, {"value": "test1", "name": "b"}],
            "select": "*",
        }
        self.assertEqual(result, expected)

    def test_020(self):
        sql = "SELECT max(test1.f1,test2.r1), min(test1.f2,test2.r2)\nFROM test2, test1"
        result = parse(sql)
        expected = {
            "from": ["test2", "test1"],
            "select": [
                {"value": {"max": ["test1.f1", "test2.r1"]}},
                {"value": {"min": ["test1.f2", "test2.r2"]}},
            ],
        }
        self.assertEqual(result, expected)

    def test_021(self):
        sql = "SELECT min(test1.f1,test2.r1), max(test1.f2,test2.r2)\nFROM test1, test2"
        result = parse(sql)
        expected = {
            "from": ["test1", "test2"],
            "select": [
                {"value": {"min": ["test1.f1", "test2.r1"]}},
                {"value": {"max": ["test1.f2", "test2.r2"]}},
            ],
        }
        self.assertEqual(result, expected)

    def test_022(self):
        sql = "SELECT count(f1,f2) FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": {"count": ["f1", "f2"]}}}
        self.assertEqual(result, expected)

    def test_023(self):
        sql = "SELECT count(f1) FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": {"count": "f1"}}}
        self.assertEqual(result, expected)

    def test_024(self):
        sql = "SELECT Count() FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": {"count": {}}}}
        self.assertEqual(result, expected)

    def test_025(self):
        sql = "SELECT COUNT(*) FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": {"count": "*"}}}
        self.assertEqual(result, expected)

    def test_026(self):
        sql = "SELECT COUNT(*)+1 FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": {"add": [{"count": "*"}, 1]}}}
        self.assertEqual(result, expected)

    def test_027(self):
        sql = "SELECT count(*),count(a),count(b) FROM t3"
        result = parse(sql)
        expected = {
            "from": "t3",
            "select": [
                {"value": {"count": "*"}},
                {"value": {"count": "a"}},
                {"value": {"count": "b"}},
            ],
        }
        self.assertEqual(result, expected)

    def test_028(self):
        sql = "SELECT count(*),count(a),count(b) FROM t4"
        result = parse(sql)
        expected = {
            "from": "t4",
            "select": [
                {"value": {"count": "*"}},
                {"value": {"count": "a"}},
                {"value": {"count": "b"}},
            ],
        }
        self.assertEqual(result, expected)

    def test_029(self):
        sql = "SELECT count(*),count(a),count(b) FROM t4 WHERE b=5"
        result = parse(sql)
        expected = {
            "from": "t4",
            "select": [
                {"value": {"count": "*"}},
                {"value": {"count": "a"}},
                {"value": {"count": "b"}},
            ],
            "where": {"eq": ["b", 5]},
        }
        self.assertEqual(result, expected)

    def test_030(self):
        sql = "SELECT min(*) FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": {"min": "*"}}}
        self.assertEqual(result, expected)

    def test_031(self):
        sql = "SELECT Min(f1) FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": {"min": "f1"}}}
        self.assertEqual(result, expected)

    def test_032(self):
        sql = "SELECT MIN(f1,f2) FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": {"min": ["f1", "f2"]}}}
        self.assertEqual(result, expected)

    def test_033(self):
        sql = "SELECT coalesce(min(a),'xyzzy') FROM t3"
        result = parse(sql)
        expected = {
            "from": "t3",
            "select": {"value": {"coalesce": [{"min": "a"}, {"literal": "xyzzy"}]}},
        }
        self.assertEqual(result, expected)

    def test_034(self):
        sql = "SELECT min(coalesce(a,'xyzzy')) FROM t3"
        result = parse(sql)
        expected = {
            "from": "t3",
            "select": {"value": {"min": {"coalesce": ["a", {"literal": "xyzzy"}]}}},
        }
        self.assertEqual(result, expected)

    def test_035(self):
        sql = "SELECT min(b), min(b) FROM t4"
        result = parse(sql)
        expected = {
            "from": "t4",
            "select": [{"value": {"min": "b"}}, {"value": {"min": "b"}}],
        }
        self.assertEqual(result, expected)

    def test_036(self):
        sql = "SELECT MAX(*) FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": {"max": "*"}}}
        self.assertEqual(result, expected)

    def test_037(self):
        sql = "SELECT Max(f1) FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": {"max": "f1"}}}
        self.assertEqual(result, expected)

    def test_038(self):
        sql = "SELECT max(f1,f2) FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": {"max": ["f1", "f2"]}}}
        self.assertEqual(result, expected)

    def test_039(self):
        sql = "SELECT MAX(f1,f2)+1 FROM test1"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": {"add": [{"max": ["f1", "f2"]}, 1]}},
        }
        self.assertEqual(result, expected)

    def test_040(self):
        sql = "SELECT MAX(f1)+1 FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": {"add": [{"max": "f1"}, 1]}}}
        self.assertEqual(result, expected)

    def test_041(self):
        #      0123456789012345678901234567890123456789
        sql = "SELECT coalesce(max(a),'xyzzy') FROM t3"
        result = parse(sql)
        expected = {
            "from": "t3",
            "select": {"value": {"coalesce": [{"max": "a"}, {"literal": "xyzzy"}]}},
        }
        self.assertEqual(result, expected)

    def test_042(self):
        sql = "SELECT max(coalesce(a,'xyzzy')) FROM t3"
        result = parse(sql)
        expected = {
            "from": "t3",
            "select": {"value": {"max": {"coalesce": ["a", {"literal": "xyzzy"}]}}},
        }
        self.assertEqual(result, expected)

    def test_043(self):
        sql = "SELECT SUM(*) FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": {"sum": "*"}}}
        self.assertEqual(result, expected)

    def test_044(self):
        sql = "SELECT Sum(f1) FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": {"sum": "f1"}}}
        self.assertEqual(result, expected)

    def test_045(self):
        sql = "SELECT sum(f1,f2) FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": {"sum": ["f1", "f2"]}}}
        self.assertEqual(result, expected)

    def test_046(self):
        sql = "SELECT SUM(f1)+1 FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": {"add": [{"sum": "f1"}, 1]}}}
        self.assertEqual(result, expected)

    def test_047(self):
        sql = "SELECT sum(a) FROM t3"
        result = parse(sql)
        expected = {"from": "t3", "select": {"value": {"sum": "a"}}}
        self.assertEqual(result, expected)

    def test_048(self):
        sql = "SELECT XYZZY(f1) FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": {"xyzzy": "f1"}}}
        self.assertEqual(result, expected)

    def test_049(self):
        sql = "SELECT SUM(min(f1,f2)) FROM test1"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": {"sum": {"min": ["f1", "f2"]}}},
        }
        self.assertEqual(result, expected)

    def test_050(self):
        sql = "SELECT SUM(min(f1)) FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": {"sum": {"min": "f1"}}}}
        self.assertEqual(result, expected)

    def test_052(self):
        sql = "SELECT f1 FROM test1 WHERE f1<11"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1"},
            "where": {"lt": ["f1", 11]},
        }
        self.assertEqual(result, expected)

    def test_053(self):
        sql = "SELECT f1 FROM test1 WHERE f1<=11"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1"},
            "where": {"lte": ["f1", 11]},
        }
        self.assertEqual(result, expected)

    def test_054(self):
        sql = "SELECT f1 FROM test1 WHERE f1=11"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1"},
            "where": {"eq": ["f1", 11]},
        }
        self.assertEqual(result, expected)

    def test_055(self):
        sql = "SELECT f1 FROM test1 WHERE f1>=11"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1"},
            "where": {"gte": ["f1", 11]},
        }
        self.assertEqual(result, expected)

    def test_056(self):
        sql = "SELECT f1 FROM test1 WHERE f1>11"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1"},
            "where": {"gt": ["f1", 11]},
        }
        self.assertEqual(result, expected)

    def test_057(self):
        sql = "SELECT f1 FROM test1 WHERE f1!=11"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1"},
            "where": {"neq": ["f1", 11]},
        }
        self.assertEqual(result, expected)

    def test_058(self):
        sql = "SELECT f1 FROM test1 WHERE min(f1,f2)!=11"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1"},
            "where": {"neq": [{"min": ["f1", "f2"]}, 11]},
        }
        self.assertEqual(result, expected)

    def test_059(self):
        sql = "SELECT f1 FROM test1 WHERE max(f1,f2)!=11"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1"},
            "where": {"neq": [{"max": ["f1", "f2"]}, 11]},
        }
        self.assertEqual(result, expected)

    def test_060(self):
        sql = "SELECT f1 FROM test1 WHERE count(f1,f2)!=11"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1"},
            "where": {"neq": [{"count": ["f1", "f2"]}, 11]},
        }
        self.assertEqual(result, expected)

    def test_061(self):
        sql = "SELECT f1 FROM test1 ORDER BY f1"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1"},
            "orderby": {"value": "f1"},
        }
        self.assertEqual(result, expected)

    def test_062(self):
        sql = "SELECT f1 FROM test1 ORDER BY -f1"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1"},
            "orderby": {"value": {"neg": "f1"}},
        }
        self.assertEqual(result, expected)

    def test_063(self):
        sql = "SELECT f1 FROM test1 ORDER BY min(f1,f2)"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1"},
            "orderby": {"value": {"min": ["f1", "f2"]}},
        }
        self.assertEqual(result, expected)

    def test_064(self):
        sql = "SELECT f1 FROM test1 ORDER BY min(f1)"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1"},
            "orderby": {"value": {"min": "f1"}},
        }
        self.assertEqual(result, expected)

    def test_065(self):
        sql = "SELECT f1 FROM test1 ORDER BY 8.4"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1"},
            "orderby": {"value": 8.4},
        }
        self.assertEqual(result, expected)

    def test_066(self):
        sql = "SELECT f1 FROM test1 ORDER BY '8.4'"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1"},
            "orderby": {"value": {"literal": "8.4"}},
        }
        self.assertEqual(result, expected)

    def test_067(self):
        sql = "SELECT * FROM t5 ORDER BY 1"
        result = parse(sql)
        expected = {"from": "t5", "select": "*", "orderby": {"value": 1}}
        self.assertEqual(result, expected)

    def test_068(self):
        sql = "SELECT * FROM t5 ORDER BY 2"
        result = parse(sql)
        expected = {"from": "t5", "select": "*", "orderby": {"value": 2}}
        self.assertEqual(result, expected)

    def test_069(self):
        sql = "SELECT * FROM t5 ORDER BY +2"
        result = parse(sql)
        expected = {"from": "t5", "select": "*", "orderby": {"value": 2}}
        self.assertEqual(result, expected)

    def test_070(self):
        sql = "SELECT * FROM t5 ORDER BY 2, 1 DESC"
        result = parse(sql)
        expected = {
            "from": "t5",
            "select": "*",
            "orderby": [{"value": 2}, {"value": 1, "sort": "desc"}],
        }
        self.assertEqual(result, expected)

    def test_071(self):
        sql = "SELECT * FROM t5 ORDER BY 1 DESC, b"
        result = parse(sql)
        expected = {
            "from": "t5",
            "select": "*",
            "orderby": [{"value": 1, "sort": "desc"}, {"value": "b"}],
        }
        self.assertEqual(result, expected)

    def test_072(self):
        sql = "SELECT * FROM t5 ORDER BY b DESC, 1"
        result = parse(sql)
        expected = {
            "from": "t5",
            "select": "*",
            "orderby": [{"value": "b", "sort": "desc"}, {"value": 1}],
        }
        self.assertEqual(result, expected)

    def test_073(self):
        sql = "SELECT max(f1) FROM test1 ORDER BY f2"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": {"max": "f1"}},
            "orderby": {"value": "f2"},
        }
        self.assertEqual(result, expected)

    def test_078(self):
        sql = "SELECT A.f1, B.f1 FROM test1 as A, test1 as B\nORDER BY A.f1, B.f1"
        result = parse(sql)
        expected = {
            "from": [{"value": "test1", "name": "A"}, {"value": "test1", "name": "B"}],
            "select": [{"value": "A.f1"}, {"value": "B.f1"}],
            "orderby": [{"value": "A.f1"}, {"value": "B.f1"}],
        }
        self.assertEqual(result, expected)

    def test_086(self):
        #                1111111111222222222233333333334444444444555555555566666666667777777777
        #      01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
        sql = (
            "SELECT a FROM t6 WHERE b IN\n(SELECT b FROM t6 WHERE a<='b' UNION SELECT"
            " '3' AS x\nORDER BY 1 LIMIT 1)"
        )
        result = parse(sql)
        expected = {
            "from": "t6",
            "select": {"value": "a"},
            "where": {"in": [
                "b",
                {
                    "from": {"union": [
                        {
                            "from": "t6",
                            "select": {"value": "b"},
                            "where": {"lte": ["a", {"literal": "b"}]},
                        },
                        {"select": {"value": {"literal": "3"}, "name": "x"}},
                    ]},
                    "orderby": {"value": 1},
                    "limit": 1,
                },
            ]},
        }
        self.assertEqual(result, expected)

    def test_087(self):
        sql = (
            "SELECT a FROM t6 WHERE b IN\n(SELECT b FROM t6 WHERE a<='b' UNION SELECT"
            " '3' AS x\nORDER BY 1 DESC LIMIT 1)"
        )
        result = parse(sql)
        expected = {
            "from": "t6",
            "select": {"value": "a"},
            "where": {"in": [
                "b",
                {
                    "from": {"union": [
                        {
                            "from": "t6",
                            "select": {"value": "b"},
                            "where": {"lte": ["a", {"literal": "b"}]},
                        },
                        {"select": {"value": {"literal": "3"}, "name": "x"}},
                    ]},
                    "orderby": {"value": 1, "sort": "desc"},
                    "limit": 1,
                },
            ]},
        }
        self.assertEqual(result, expected)

    def test_088(self):
        sql = (
            "SELECT a FROM t6 WHERE b IN\n(SELECT b FROM t6 WHERE a<='b' UNION SELECT"
            " '3' AS x\nORDER BY b LIMIT 2)\nORDER BY a"
        )
        result = parse(sql)
        expected = {
            "from": "t6",
            "select": {"value": "a"},
            "where": {"in": [
                "b",
                {
                    "from": {"union": [
                        {
                            "from": "t6",
                            "select": {"value": "b"},
                            "where": {"lte": ["a", {"literal": "b"}]},
                        },
                        {"select": {"value": {"literal": "3"}, "name": "x"}},
                    ]},
                    "orderby": {"value": "b"},
                    "limit": 2,
                },
            ]},
            "orderby": {"value": "a"},
        }
        self.assertEqual(result, expected)

    def test_089(self):
        sql = (
            "SELECT a FROM t6 WHERE b IN\n(SELECT b FROM t6 WHERE a<='b' UNION SELECT"
            " '3' AS x\nORDER BY x DESC LIMIT 2)\nORDER BY a"
        )
        result = parse(sql)
        expected = {
            "from": "t6",
            "select": {"value": "a"},
            "where": {"in": [
                "b",
                {
                    "from": {"union": [
                        {
                            "from": "t6",
                            "select": {"value": "b"},
                            "where": {"lte": ["a", {"literal": "b"}]},
                        },
                        {"select": {"value": {"literal": "3"}, "name": "x"}},
                    ]},
                    "orderby": {"value": "x", "sort": "desc"},
                    "limit": 2,
                },
            ]},
            "orderby": {"value": "a"},
        }
        self.assertEqual(result, expected)

    def test_090(self):
        sql = "SELECT f1 FROM test1 UNION SELECT WHERE"
        self.assertRaises(Exception, parse, sql)

    def test_091(self):
        sql = "SELECT f1 FROM test1 as 'hi', test2 as"
        self.assertRaises(Exception, parse, sql)

    def test_093(self):
        sql = "SELECT count(f1,f2) FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": {"count": ["f1", "f2"]}}}
        self.assertEqual(result, expected)

    def test_094(self):
        sql = "SELECT f1 FROM test1 ORDER BY f2, f1"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1"},
            "orderby": [{"value": "f2"}, {"value": "f1"}],
        }
        self.assertEqual(result, expected)

    def test_095(self):
        sql = "SELECT f1 FROM test1 WHERE 4.3+2.4 OR 1 ORDER BY f1"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1"},
            "where": {"or": [{"add": [4.3, 2.4]}, 1]},
            "orderby": {"value": "f1"},
        }
        self.assertEqual(result, expected)

    @skipIf(IS_MASTER, "does not work on master, not enough stack space")
    def test_096(self):
        #      01234567890123456789012345678901234567890123456789012345678901 234567890123456789
        sql = (
            "SELECT f1 FROM test1 WHERE ('x' || f1) BETWEEN 'x10' AND 'x20'\nORDER"
            " BY f1"
        )
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1"},
            "where": {"between": [
                {"concat": [{"literal": "x"}, "f1"]},
                {"literal": "x10"},
                {"literal": "x20"},
            ]},
            "orderby": {"value": "f1"},
        }
        self.assertEqual(result, expected)

    def test_097(self):
        sql = "SELECT f1 FROM test1 WHERE 5-3==2\nORDER BY f1"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1"},
            "where": {"eq": [{"sub": [5, 3]}, 2]},
            "orderby": {"value": "f1"},
        }
        self.assertEqual(result, expected)

    @skipIf(IS_MASTER, "does not work on master, not enough stack space")
    def test_098(self):
        sql = (
            "SELECT"
            " coalesce(f1/(f1-11),'x'),\ncoalesce(min(f1/(f1-11),5),'y'),\ncoalesce(max(f1/(f1-33),6),'z')\nFROM"
            " test1 ORDER BY f1"
        )
        result = parse(sql)
        expected = {
            "from": "test1",
            "orderby": {"value": "f1"},
            "select": [
                {"value": {"coalesce": [
                    {"div": ["f1", {"sub": ["f1", 11]}]},
                    {"literal": "x"},
                ]}},
                {"value": {"coalesce": [
                    {"min": [{"div": ["f1", {"sub": ["f1", 11]}]}, 5]},
                    {"literal": "y"},
                ]}},
                {"value": {"coalesce": [
                    {"max": [{"div": ["f1", {"sub": ["f1", 33]}]}, 6]},
                    {"literal": "z"},
                ]}},
            ],
        }
        self.assertEqual(result, expected)

    def test_099(self):
        sql = "SELECT min(1,2,3), -max(1,2,3)\nFROM test1 ORDER BY f1"
        result = parse(sql)
        expected = {
            "from": "test1",
            "orderby": {"value": "f1"},
            "select": [
                {"value": {"min": [1, 2, 3]}},
                {"value": {"neg": {"max": [1, 2, 3]}}},
            ],
        }
        self.assertEqual(result, expected)

    def test_100(self):
        sql = "SELECT * FROM test1 WHERE f1<0"
        result = parse(sql)
        expected = {"from": "test1", "select": "*", "where": {"lt": ["f1", 0]}}
        self.assertEqual(result, expected)

    def test_103(self):
        sql = "SELECT * FROM test1 WHERE f1<(select count(*) from test2)"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": "*",
            "where": {"lt": [
                "f1",
                {"from": "test2", "select": {"value": {"count": "*"}}},
            ]},
        }
        self.assertEqual(result, expected)

    def test_104(self):
        sql = "SELECT * FROM test1 ORDER BY f1"
        result = parse(sql)
        expected = {"from": "test1", "select": "*", "orderby": {"value": "f1"}}
        self.assertEqual(result, expected)

    def test_105(self):
        sql = "SELECT * FROM test1 WHERE f1<0 ORDER BY f1"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": "*",
            "where": {"lt": ["f1", 0]},
            "orderby": {"value": "f1"},
        }
        self.assertEqual(result, expected)

    def test_106(self):
        sql = "SELECT f1 AS x FROM test1 ORDER BY x"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1", "name": "x"},
            "orderby": {"value": "x"},
        }
        self.assertEqual(result, expected)

    def test_107(self):
        #      0123456789012345678901234567890123456789
        sql = "SELECT f1 AS x FROM test1 ORDER BY -x"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": "f1", "name": "x"},
            "orderby": {"value": {"neg": "x"}},
        }
        self.assertEqual(result, expected)

    def test_108(self):
        sql = "SELECT f1-23 AS x FROM test1 ORDER BY abs(x)"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": {"sub": ["f1", 23]}, "name": "x"},
            "orderby": {"value": {"abs": "x"}},
        }
        self.assertEqual(result, expected)

    def test_109(self):
        sql = "SELECT f1-23 AS x FROM test1 ORDER BY -abs(x)"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"value": {"sub": ["f1", 23]}, "name": "x"},
            "orderby": {"value": {"neg": {"abs": "x"}}},
        }
        self.assertEqual(result, expected)

    def test_110(self):
        sql = "SELECT f1-22 AS x, f2-22 as y FROM test1"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": [
                {"value": {"sub": ["f1", 22]}, "name": "x"},
                {"value": {"sub": ["f2", 22]}, "name": "y"},
            ],
        }
        self.assertEqual(result, expected)

    def test_111(self):
        sql = "SELECT f1-22 AS x, f2-22 as y FROM test1 WHERE x>0 AND y<50"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": [
                {"value": {"sub": ["f1", 22]}, "name": "x"},
                {"value": {"sub": ["f2", 22]}, "name": "y"},
            ],
            "where": {"and": [{"gt": ["x", 0]}, {"lt": ["y", 50]}]},
        }
        self.assertEqual(result, expected)

    def test_112(self):
        sql = "SELECT f1 COLLATE nocase AS x FROM test1 ORDER BY x"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": {"name": "x", "value": {"collate": ["f1", "nocase"]}},
            "orderby": {"value": "x"},
        }
        self.assertEqual(result, expected)

    def test_113(self):
        sql = "SELECT * FROM t3, t4"
        result = parse(sql)
        expected = {"from": ["t3", "t4"], "select": "*"}
        self.assertEqual(result, expected)

    def test_114(self):
        sql = "SELECT t3.*, t4.b FROM t3, t4"
        result = parse(sql)
        expected = {
            "from": ["t3", "t4"],
            "select": [{"value": "t3.*"}, {"value": "t4.b"}],
        }
        self.assertEqual(result, expected)

    def test_115(self):
        sql = 'SELECT "t3".*, t4.b FROM t3, t4'
        result = parse(sql)
        expected = {
            "from": ["t3", "t4"],
            "select": [{"value": "t3.*"}, {"value": "t4.b"}],
        }
        self.assertEqual(result, expected)

    def test_116(self):
        sql = "SELECT t3.b, t4.* FROM t3, t4"
        result = parse(sql)
        expected = {
            "from": ["t3", "t4"],
            "select": [{"value": "t3.b"}, {"value": "t4.*"}],
        }
        self.assertEqual(result, expected)

    def test_118a(self):
        sql = "SELECT * FROM t3 UNION SELECT 3 AS 'a', 4 ORDER BY a"
        self.assertRaises(Exception, parse, sql)

    def test_118b(self):
        sql = 'SELECT * FROM t3 UNION SELECT 3 AS "a", 4 ORDER BY a'
        result = parse(sql)
        expected = {
            "from": {"union": [
                {"from": "t3", "select": "*"},
                {"select": [{"value": 3, "name": "a"}, {"value": 4}]},
            ]},
            "orderby": {"value": "a"},
        }
        self.assertEqual(result, expected)

    def test_118c(self):
        sql = "SELECT * FROM t3 UNION SELECT 3 AS a, 4 ORDER BY a"
        result = parse(sql)
        expected = {
            "from": {"union": [
                {"from": "t3", "select": "*"},
                {"select": [{"value": 3, "name": "a"}, {"value": 4}]},
            ]},
            "orderby": {"value": "a"},
        }
        self.assertEqual(result, expected)

    def test_119(self):
        sql = "SELECT 3, 4 UNION SELECT * FROM t3"
        result = parse(sql)
        expected = {"union": [
            {"select": [{"value": 3}, {"value": 4}]},
            {"from": "t3", "select": "*"},
        ]}
        self.assertEqual(result, expected)

    def test_120(self):
        sql = "SELECT * FROM t3 WHERE a=(SELECT 1)"
        result = parse(sql)
        expected = {
            "from": "t3",
            "select": "*",
            "where": {"eq": ["a", {"select": {"value": 1}}]},
        }
        self.assertEqual(result, expected)

    def test_121(self):
        sql = "SELECT * FROM t3 WHERE a=(SELECT 2)"
        result = parse(sql)
        expected = {
            "from": "t3",
            "select": "*",
            "where": {"eq": ["a", {"select": {"value": 2}}]},
        }
        self.assertEqual(result, expected)

    def test_125(self):
        #                11111111112222222222333333333344444444445555555555666666666677777777778888888888
        #      012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
        sql = (
            "SELECT count(\n(SELECT a FROM abc WHERE a = NULL AND b >= upper.c)\n) FROM"
            " abc AS upper"
        )
        result = parse(sql)
        expected = {
            "from": {"value": "abc", "name": "upper"},
            "select": {"value": {"count": {
                "from": "abc",
                "select": {"value": "a"},
                "where": {"and": [{"missing": "a"}, {"gte": ["b", "upper.c"]}]},
            }}},
        }
        self.assertEqual(result, expected)

    def test_126(self):
        sql = "SELECT name FROM sqlite_master WHERE type = 'table'"
        result = parse(sql)
        expected = {
            "from": "sqlite_master",
            "select": {"value": "name"},
            "where": {"eq": ["type", {"literal": "table"}]},
        }
        self.assertEqual(result, expected)

    def test_128(self):
        sql = "SELECT 10 IN (SELECT rowid FROM sqlite_master)"
        result = parse(sql)
        expected = {
            "select": {"value": {"in": [
                10,
                {"from": "sqlite_master", "select": {"value": "rowid"}},
            ]}},
        }
        self.assertEqual(result, expected)

    def test_131(self):
        sql = "SELECT 2 IN (SELECT a FROM t1)"
        result = parse(sql)
        expected = {
            "select": {"value": {"in": [2, {"from": "t1", "select": {"value": "a"}}]}},
        }
        self.assertEqual(result, expected)

    def test_139(self):
        sql = "SELECT count(*) FROM tbl2"
        result = parse(sql)
        expected = {"from": "tbl2", "select": {"value": {"count": "*"}}}
        self.assertEqual(result, expected)

    def test_140(self):
        sql = "SELECT count(*) FROM tbl2 WHERE f2>1000"
        result = parse(sql)
        expected = {
            "from": "tbl2",
            "select": {"value": {"count": "*"}},
            "where": {"gt": ["f2", 1000]},
        }
        self.assertEqual(result, expected)

    def test_141(self):
        sql = "SELECT f1 FROM tbl2 WHERE 1000=f2"
        result = parse(sql)
        expected = {
            "from": "tbl2",
            "select": {"value": "f1"},
            "where": {"eq": [1000, "f2"]},
        }
        self.assertEqual(result, expected)

    def test_144(self):
        sql = "SELECT f1 FROM tbl2 WHERE f2=1000"
        result = parse(sql)
        expected = {
            "from": "tbl2",
            "select": {"value": "f1"},
            "where": {"eq": ["f2", 1000]},
        }
        self.assertEqual(result, expected)

    def test_145(self):
        sql = "SELECT * FROM tbl2 WHERE 1000=f2"
        result = parse(sql)
        expected = {"from": "tbl2", "select": "*", "where": {"eq": [1000, "f2"]}}
        self.assertEqual(result, expected)

    def test_146(self):
        sql = "SELECT * FROM tbl2 WHERE f2=1000"
        result = parse(sql)
        expected = {"from": "tbl2", "select": "*", "where": {"eq": ["f2", 1000]}}
        self.assertEqual(result, expected)

    def test_148(self):
        sql = "SELECT f1 FROM tbl2 WHERE f2==2000"
        result = parse(sql)
        expected = {
            "from": "tbl2",
            "select": {"value": "f1"},
            "where": {"eq": ["f2", 2000]},
        }
        self.assertEqual(result, expected)

    def test_150(self):
        sql = "SELECT * FROM aa CROSS JOIN bb WHERE b"
        result = parse(sql)
        expected = {"from": ["aa", {"cross join": "bb"}], "select": "*", "where": "b"}
        self.assertEqual(result, expected)

    def test_151(self):
        sql = "SELECT * FROM aa CROSS JOIN bb WHERE NOT b"
        result = parse(sql)
        expected = {
            "from": ["aa", {"cross join": "bb"}],
            "select": "*",
            "where": {"not": "b"},
        }
        self.assertEqual(result, expected)

    def test_152(self):
        sql = "SELECT * FROM aa, bb WHERE min(a,b)"
        result = parse(sql)
        expected = {"from": ["aa", "bb"], "select": "*", "where": {"min": ["a", "b"]}}
        self.assertEqual(result, expected)

    def test_153(self):
        sql = "SELECT * FROM aa, bb WHERE NOT min(a,b)"
        result = parse(sql)
        expected = {
            "from": ["aa", "bb"],
            "select": "*",
            "where": {"not": {"min": ["a", "b"]}},
        }
        self.assertEqual(result, expected)

    def test_154(self):
        sql = "SELECT * FROM aa, bb WHERE CASE WHEN a=b-1 THEN 1 END"
        result = parse(sql)
        expected = {
            "from": ["aa", "bb"],
            "select": "*",
            "where": {"case": {"when": {"eq": ["a", {"sub": ["b", 1]}]}, "then": 1}},
        }
        self.assertEqual(result, expected)

    def test_155(self):
        sql = "SELECT * FROM aa, bb WHERE CASE WHEN a=b-1 THEN 0 ELSE 1 END"
        result = parse(sql)
        expected = {
            "from": ["aa", "bb"],
            "select": "*",
            "where": {"case": [
                {"when": {"eq": ["a", {"sub": ["b", 1]}]}, "then": 0},
                1,
            ]},
        }
        self.assertEqual(result, expected)

    def test_158(self):
        sql = "SELECT DISTINCT log FROM t1 ORDER BY log"
        result = parse(sql)
        expected = {
            "from": "t1",
            "select_distinct": {"value": "log"},
            "orderby": {"value": "log"},
        }
        self.assertEqual(result, expected)

    def test_160(self):
        sql = (
            "SELECT"
            " min(n),min(log),max(n),max(log),sum(n),sum(log),avg(n),avg(log)\nFROM t1"
        )
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [
                {"value": {"min": "n"}},
                {"value": {"min": "log"}},
                {"value": {"max": "n"}},
                {"value": {"max": "log"}},
                {"value": {"sum": "n"}},
                {"value": {"sum": "log"}},
                {"value": {"avg": "n"}},
                {"value": {"avg": "log"}},
            ],
        }
        self.assertEqual(result, expected)

    def test_161(self):
        sql = "SELECT max(n)/avg(n), max(log)/avg(log) FROM t1"
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [
                {"value": {"div": [{"max": "n"}, {"avg": "n"}]}},
                {"value": {"div": [{"max": "log"}, {"avg": "log"}]}},
            ],
        }
        self.assertEqual(result, expected)

    def test_162(self):
        #      012345678901234567890123456789012345678901234567890123456789
        sql = "SELECT log, count(*) FROM t1 GROUP BY log ORDER BY log"
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [{"value": "log"}, {"value": {"count": "*"}}],
            "groupby": {"value": "log"},
            "orderby": {"value": "log"},
        }
        self.assertEqual(result, expected)

    def test_163(self):
        sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY log"
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [{"value": "log"}, {"value": {"min": "n"}}],
            "groupby": {"value": "log"},
            "orderby": {"value": "log"},
        }
        self.assertEqual(result, expected)

    def test_164(self):
        sql = "SELECT log, avg(n) FROM t1 GROUP BY log ORDER BY log"
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [{"value": "log"}, {"value": {"avg": "n"}}],
            "groupby": {"value": "log"},
            "orderby": {"value": "log"},
        }
        self.assertEqual(result, expected)

    def test_165(self):
        sql = "SELECT log, avg(n)+1 FROM t1 GROUP BY log ORDER BY log"
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [{"value": "log"}, {"value": {"add": [{"avg": "n"}, 1]}}],
            "groupby": {"value": "log"},
            "orderby": {"value": "log"},
        }
        self.assertEqual(result, expected)

    def test_166(self):
        sql = "SELECT log, avg(n)-min(n) FROM t1 GROUP BY log ORDER BY log"
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [
                {"value": "log"},
                {"value": {"sub": [{"avg": "n"}, {"min": "n"}]}},
            ],
            "groupby": {"value": "log"},
            "orderby": {"value": "log"},
        }
        self.assertEqual(result, expected)

    def test_167(self):
        sql = "SELECT log*2+1, avg(n)-min(n) FROM t1 GROUP BY log ORDER BY log"
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [
                {"value": {"add": [{"mul": ["log", 2]}, 1]}},
                {"value": {"sub": [{"avg": "n"}, {"min": "n"}]}},
            ],
            "groupby": {"value": "log"},
            "orderby": {"value": "log"},
        }
        self.assertEqual(result, expected)

    def test_168(self):
        sql = "SELECT log*2+1 as x, count(*) FROM t1 GROUP BY x ORDER BY x"
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [
                {"value": {"add": [{"mul": ["log", 2]}, 1]}, "name": "x"},
                {"value": {"count": "*"}},
            ],
            "groupby": {"value": "x"},
            "orderby": {"value": "x"},
        }
        self.assertEqual(result, expected)

    def test_169(self):
        sql = "SELECT log*2+1 AS x, count(*) AS y FROM t1 GROUP BY x ORDER BY y, x"
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [
                {"value": {"add": [{"mul": ["log", 2]}, 1]}, "name": "x"},
                {"value": {"count": "*"}, "name": "y"},
            ],
            "groupby": {"value": "x"},
            "orderby": [{"value": "y"}, {"value": "x"}],
        }
        self.assertEqual(result, expected)

    def test_170(self):
        sql = "SELECT log*2+1 AS x, count(*) AS y FROM t1 GROUP BY x ORDER BY 10-(x+y)"
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [
                {"value": {"add": [{"mul": ["log", 2]}, 1]}, "name": "x"},
                {"value": {"count": "*"}, "name": "y"},
            ],
            "groupby": {"value": "x"},
            "orderby": {"value": {"sub": [10, {"add": ["x", "y"]}]}},
        }
        self.assertEqual(result, expected)

    def test_171(self):
        sql = "SELECT log, count(*) FROM t1 GROUP BY something HAVING log>=4"
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [{"value": "log"}, {"value": {"count": "*"}}],
            "groupby": {"value": "something"},
            "having": {"gte": ["log", 4]},
        }
        self.assertEqual(result, expected)

    def test_172(self):
        sql = "SELECT log, count(*) FROM t1 GROUP BY log HAVING log>=4 ORDER BY log"
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [{"value": "log"}, {"value": {"count": "*"}}],
            "groupby": {"value": "log"},
            "having": {"gte": ["log", 4]},
            "orderby": {"value": "log"},
        }
        self.assertEqual(result, expected)

    def test_173(self):
        sql = (
            "SELECT log, count(*) FROM t1\nGROUP BY log\nHAVING count(*)>=4\nORDER"
            " BY log"
        )
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [{"value": "log"}, {"value": {"count": "*"}}],
            "groupby": {"value": "log"},
            "having": {"gte": [{"count": "*"}, 4]},
            "orderby": {"value": "log"},
        }
        self.assertEqual(result, expected)

    def test_174(self):
        sql = (
            "SELECT log, count(*) FROM t1\nGROUP BY log\nHAVING count(*)>=4\nORDER BY"
            " max(n)+0"
        )
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [{"value": "log"}, {"value": {"count": "*"}}],
            "groupby": {"value": "log"},
            "having": {"gte": [{"count": "*"}, 4]},
            "orderby": {"value": {"add": [{"max": "n"}, 0]}},
        }
        self.assertEqual(result, expected)

    def test_175(self):
        sql = (
            "SELECT log AS x, count(*) AS y FROM t1\nGROUP BY x\nHAVING y>=4\nORDER BY"
            " max(n)+0"
        )
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [
                {"value": "log", "name": "x"},
                {"value": {"count": "*"}, "name": "y"},
            ],
            "groupby": {"value": "x"},
            "having": {"gte": ["y", 4]},
            "orderby": {"value": {"add": [{"max": "n"}, 0]}},
        }
        self.assertEqual(result, expected)

    def test_176(self):
        sql = (
            "SELECT log AS x FROM t1\nGROUP BY x\nHAVING count(*)>=4\nORDER BY max(n)+0"
        )
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": {"value": "log", "name": "x"},
            "groupby": {"value": "x"},
            "having": {"gte": [{"count": "*"}, 4]},
            "orderby": {"value": {"add": [{"max": "n"}, 0]}},
        }
        self.assertEqual(result, expected)

    def test_177(self):
        sql = (
            "SELECT log, count(*), avg(n), max(n+log*2) FROM t1\nGROUP BY log\nORDER BY"
            " max(n+log*2)+0, avg(n)+0"
        )
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [
                {"value": "log"},
                {"value": {"count": "*"}},
                {"value": {"avg": "n"}},
                {"value": {"max": {"add": ["n", {"mul": ["log", 2]}]}}},
            ],
            "groupby": {"value": "log"},
            "orderby": [
                {"value": {"add": [{"max": {"add": ["n", {"mul": ["log", 2]}]}}, 0]}},
                {"value": {"add": [{"avg": "n"}, 0]}},
            ],
        }
        self.assertEqual(result, expected)

    def test_178(self):
        sql = (
            "SELECT log, count(*), avg(n), max(n+log*2) FROM t1\nGROUP BY log\nORDER BY"
            " max(n+log*2)+0, min(log,avg(n))+0"
        )
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [
                {"value": "log"},
                {"value": {"count": "*"}},
                {"value": {"avg": "n"}},
                {"value": {"max": {"add": ["n", {"mul": ["log", 2]}]}}},
            ],
            "groupby": {"value": "log"},
            "orderby": [
                {"value": {"add": [{"max": {"add": ["n", {"mul": ["log", 2]}]}}, 0]}},
                {"value": {"add": [{"min": ["log", {"avg": "n"}]}, 0]}},
            ],
        }
        self.assertEqual(result, expected)

    def test_179(self):
        sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY log"
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [{"value": "log"}, {"value": {"min": "n"}}],
            "groupby": {"value": "log"},
            "orderby": {"value": "log"},
        }
        self.assertEqual(result, expected)

    def test_180(self):
        sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY log DESC"
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [{"value": "log"}, {"value": {"min": "n"}}],
            "groupby": {"value": "log"},
            "orderby": {"value": "log", "sort": "desc"},
        }
        self.assertEqual(result, expected)

    def test_181(self):
        sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY 1"
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [{"value": "log"}, {"value": {"min": "n"}}],
            "groupby": {"value": "log"},
            "orderby": {"value": 1},
        }
        self.assertEqual(result, expected)

    def test_183(self):
        sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY log"
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [{"value": "log"}, {"value": {"min": "n"}}],
            "groupby": {"value": "log"},
            "orderby": {"value": "log"},
        }
        self.assertEqual(result, expected)

    def test_184(self):
        #      012345678901234567890123456789012345678901234567890123456789
        sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY log DESC"
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [{"value": "log"}, {"value": {"min": "n"}}],
            "groupby": {"value": "log"},
            "orderby": {"value": "log", "sort": "desc"},
        }
        self.assertEqual(result, expected)

    def test_185(self):
        sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY 1"
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [{"value": "log"}, {"value": {"min": "n"}}],
            "groupby": {"value": "log"},
            "orderby": {"value": 1},
        }
        self.assertEqual(result, expected)

    def test_186(self):
        sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY 1 DESC"
        result = parse(sql)
        expected = {
            "from": "t1",
            "select": [{"value": "log"}, {"value": {"min": "n"}}],
            "groupby": {"value": "log"},
            "orderby": {"value": 1, "sort": "desc"},
        }
        self.assertEqual(result, expected)

    def test_187(self):
        #      01234567890123456789012345678901234567890123456789
        sql = "SELECT a, sum(b) FROM t2 WHERE b=5 GROUP BY a"
        result = parse(sql)
        expected = {
            "from": "t2",
            "select": [{"value": "a"}, {"value": {"sum": "b"}}],
            "groupby": {"value": "a"},
            "where": {"eq": ["b", 5]},
        }
        self.assertEqual(result, expected)

    def test_188(self):
        sql = "SELECT a, sum(b) FROM t2 WHERE b=5"
        result = parse(sql)
        expected = {
            "from": "t2",
            "select": [{"value": "a"}, {"value": {"sum": "b"}}],
            "where": {"eq": ["b", 5]},
        }
        self.assertEqual(result, expected)

    def test_189(self):
        sql = "SELECT typeof(sum(a3)) FROM a"
        result = parse(sql)
        expected = {"from": "a", "select": {"value": {"typeof": {"sum": "a3"}}}}
        self.assertEqual(result, expected)

    def test_190(self):
        sql = "SELECT typeof(sum(a3)) FROM a GROUP BY a1"
        result = parse(sql)
        expected = {
            "from": "a",
            "select": {"value": {"typeof": {"sum": "a3"}}},
            "groupby": {"value": "a1"},
        }
        self.assertEqual(result, expected)
