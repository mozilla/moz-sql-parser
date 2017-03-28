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


class TestResources(FuzzyTestCase):
    def test_1(self):
        sql = "SELECT * FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": "*"}}
        self.assertEqual(result, expected)

    def test_2(self):
        sql = "SELECT * FROM test1, test2"
        result = parse(sql)
        expected = {"from": ["test1", "test2"], "select": {"value": "*"}}
        self.assertEqual(result, expected)

    def test_3(self):
        sql = "SELECT * FROM test2, test1"
        result = parse(sql)
        expected = {"from": ["test2", "test1"], "select": {"value": "*"}}
        self.assertEqual(result, expected)

    def test_4(self):
        sql = "SELECT f1 FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": "f1"}}
        self.assertEqual(result, expected)

    def test_5(self):
        sql = "SELECT f2 FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": {"value": "f2"}}
        self.assertEqual(result, expected)

    def test_6(self):
        sql = "SELECT f2, f1 FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": [{"value": "f2"}, {"value": "f1"}]}
        self.assertEqual(result, expected)

    def test_7(self):
        sql = "SELECT f1, f2 FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": [{"value": "f1"}, {"value": "f2"}]}
        self.assertEqual(result, expected)

    def test_8(self):
        sql = "SELECT *, * FROM test1"
        result = parse(sql)
        expected = {"from": "test1", "select": [{"value": "*"}]}
        self.assertEqual(result, expected)

    def test_9(self):
        sql = "SELECT *, min(f1,f2), max(f1,f2) FROM test1"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": [
                {"value": "*"},
                {"value": {"min": ["f1", "f2"]}},
                {"value": {"max": ["f1", "f2"]}}
            ]
        }
        self.assertEqual(result, expected)

    def test_10(self):
        sql = "SELECT 'one', *, 'two', * FROM test1"
        result = parse(sql)
        expected = {
            "from": "test1",
            "select": [
                {"value": {"literal": "one"}},
                {"value": "*"},
                {"value": {"literal": "two"}},
                {"value": "*"}
            ]
        }
        self.assertEqual(result, expected)

    def test_14(self):
        sql = "SELECT *, 'hi' FROM test1, test2"
        result = parse(sql)
        expected = {
            "from": ["test1", "test2"],
            "select": [
                {"value": "*"},
                {"value": {"literal": "hi"}}
            ]
        }
        self.assertEqual(result, expected)

    def test_15(self):
        sql = "SELECT 'one', *, 'two', * FROM test1, test2"
        result = parse(sql)
        expected = {
            "from": ["test1", "test2"],
            "select": [
                {"value": {"literal": "one"}},
                {"value": "*"},
                {"value": {"literal": "two"}},
                {"value": "*"}
            ]
        }
        self.assertEqual(result, expected)

    def test_16(self):
        sql = "SELECT test1.f1, test2.r1 FROM test1, test2"
        result = parse(sql)
        expected = {
            "from": ["test1", "test2"],
            "select": [
                {"value": "test1.f1"},
                {"value": "test2.r1"}
            ]
        }
        self.assertEqual(result, expected)

    def test_17(self):
        sql = "SELECT test1.f1, test2.r1 FROM test2, test1"
        result = parse(sql)
        expected = {
            "from": ["test2", "test1"],
            "select": [
                {"value": "test1.f1"},
                {"value": "test2.r1"}
            ]
        }
        self.assertEqual(result, expected)

    def test_19(self):
        sql = "SELECT * FROM test1 AS a, test1 AS b"
        result = parse(sql)
        expected = {
            "from": ["test2", "test1"],
            "select": [
                {"value": "*"}
            ]
        }
        self.assertEqual(result, expected)

    def test_20(self):
        sql = "SELECT max(test1.f1,test2.r1), min(test1.f2,test2.r2)\nFROM test2, test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_21(self):
        sql = "SELECT min(test1.f1,test2.r1), max(test1.f2,test2.r2)\nFROM test1, test2"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_22(self):
        sql = "SELECT count(f1,f2) FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_23(self):
        sql = "SELECT count(f1) FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_24(self):
        sql = "SELECT Count() FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_25(self):
        sql = "SELECT COUNT(*) FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_26(self):
        sql = "SELECT COUNT(*)+1 FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_27(self):
        sql = "SELECT count(*),count(a),count(b) FROM t3"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_28(self):
        sql = "SELECT count(*),count(a),count(b) FROM t4"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_29(self):
        sql = "SELECT count(*),count(a),count(b) FROM t4 WHERE b=5"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_30(self):
        sql = "SELECT min(*) FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_31(self):
        sql = "SELECT Min(f1) FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_32(self):
        sql = "SELECT MIN(f1,f2) FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_33(self):
        sql = "SELECT coalesce(min(a),'xyzzy') FROM t3"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_34(self):
        sql = "SELECT min(coalesce(a,'xyzzy')) FROM t3"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_35(self):
        sql = "SELECT min(b), min(b) FROM t4"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_36(self):
        sql = "SELECT MAX(*) FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_37(self):
        sql = "SELECT Max(f1) FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_38(self):
        sql = "SELECT max(f1,f2) FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_39(self):
        sql = "SELECT MAX(f1,f2)+1 FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_40(self):
        sql = "SELECT MAX(f1)+1 FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_41(self):
        sql = "SELECT coalesce(max(a),'xyzzy') FROM t3"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_42(self):
        sql = "SELECT max(coalesce(a,'xyzzy')) FROM t3"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_43(self):
        sql = "SELECT SUM(*) FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_44(self):
        sql = "SELECT Sum(f1) FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_45(self):
        sql = "SELECT sum(f1,f2) FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_46(self):
        sql = "SELECT SUM(f1)+1 FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_47(self):
        sql = "SELECT sum(a) FROM t3"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_48(self):
        sql = "SELECT XYZZY(f1) FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_49(self):
        sql = "SELECT SUM(min(f1,f2)) FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_50(self):
        sql = "SELECT SUM(min(f1)) FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_51(self):
        sql = "CREATE TABLE tkt2526(a,b,c PRIMARY KEY);\nINSERT INTO tkt2526 VALUES('x','y',NULL);\nINSERT INTO tkt2526 VALUES('x','z',NULL);"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_52(self):
        sql = "SELECT f1 FROM test1 WHERE f1<11"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_53(self):
        sql = "SELECT f1 FROM test1 WHERE f1<=11"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_54(self):
        sql = "SELECT f1 FROM test1 WHERE f1=11"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_55(self):
        sql = "SELECT f1 FROM test1 WHERE f1>=11"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_56(self):
        sql = "SELECT f1 FROM test1 WHERE f1>11"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_57(self):
        sql = "SELECT f1 FROM test1 WHERE f1!=11"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_58(self):
        sql = "SELECT f1 FROM test1 WHERE min(f1,f2)!=11"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_59(self):
        sql = "SELECT f1 FROM test1 WHERE max(f1,f2)!=11"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_60(self):
        sql = "SELECT f1 FROM test1 WHERE count(f1,f2)!=11"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_61(self):
        sql = "SELECT f1 FROM test1 ORDER BY f1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_62(self):
        sql = "SELECT f1 FROM test1 ORDER BY -f1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_63(self):
        sql = "SELECT f1 FROM test1 ORDER BY min(f1,f2)"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_64(self):
        sql = "SELECT f1 FROM test1 ORDER BY min(f1)"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_65(self):
        sql = "SELECT f1 FROM test1 ORDER BY 8.4"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_66(self):
        sql = "SELECT f1 FROM test1 ORDER BY '8.4'"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_67(self):
        sql = "CREATE TABLE t5(a,b);\nINSERT INTO t5 VALUES(1,10);\nINSERT INTO t5 VALUES(2,9);\nSELECT * FROM t5 ORDER BY 1;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_68(self):
        sql = "SELECT * FROM t5 ORDER BY 2;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_69(self):
        sql = "SELECT * FROM t5 ORDER BY +2;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_70(self):
        sql = "INSERT INTO t5 VALUES(3,10);\nSELECT * FROM t5 ORDER BY 2, 1 DESC;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_71(self):
        sql = "SELECT * FROM t5 ORDER BY 1 DESC, b;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_72(self):
        sql = "SELECT * FROM t5 ORDER BY b DESC, 1;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_73(self):
        sql = "SELECT max(f1) FROM test1 ORDER BY f2"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_74(self):
        sql = "CREATE TABLE test2(t1 text, t2 text)"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_75(self):
        sql = "INSERT INTO test2 VALUES('abc','xyz')"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_76(self):
        sql = "PRAGMA full_column_names=on"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_77(self):
        sql = "PRAGMA full_column_names=off"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_78(self):
        sql = "SELECT A.f1, B.f1 FROM test1 as A, test1 as B\nORDER BY A.f1, B.f1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_79(self):
        sql = "PRAGMA short_column_names=OFF;\nPRAGMA full_column_names=OFF;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_80(self):
        sql = "PRAGMA short_column_names=OFF;\nPRAGMA full_column_names=ON;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_81(self):
        sql = "PRAGMA short_column_names=OFF;\nPRAGMA full_column_names=ON;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_82(self):
        sql = "PRAGMA short_column_names=ON;\nPRAGMA full_column_names=ON;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_83(self):
        sql = "PRAGMA short_column_names=ON;\nPRAGMA full_column_names=OFF;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_84(self):
        sql = "PRAGMA short_column_names=OFF;\nPRAGMA full_column_names=ON;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_85(self):
        sql = "PRAGMA short_column_names=ON;\nPRAGMA full_column_names=OFF;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_86(self):
        sql = "CREATE TABLE t6(a TEXT, b TEXT);\nINSERT INTO t6 VALUES('a','0');\nINSERT INTO t6 VALUES('b','1');\nINSERT INTO t6 VALUES('c','2');\nINSERT INTO t6 VALUES('d','3');\nSELECT a FROM t6 WHERE b IN\n(SELECT b FROM t6 WHERE a<='b' UNION SELECT '3' AS x\nORDER BY 1 LIMIT 1)"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_87(self):
        sql = "SELECT a FROM t6 WHERE b IN\n(SELECT b FROM t6 WHERE a<='b' UNION SELECT '3' AS x\nORDER BY 1 DESC LIMIT 1)"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_88(self):
        sql = "SELECT a FROM t6 WHERE b IN\n(SELECT b FROM t6 WHERE a<='b' UNION SELECT '3' AS x\nORDER BY b LIMIT 2)\nORDER BY a;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_89(self):
        sql = "SELECT a FROM t6 WHERE b IN\n(SELECT b FROM t6 WHERE a<='b' UNION SELECT '3' AS x\nORDER BY x DESC LIMIT 2)\nORDER BY a;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_90(self):
        sql = "SELECT f1 FROM test1 UNION SELECT WHERE;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_91(self):
        sql = "SELECT f1 FROM test1 as 'hi', test2 as"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_92(self):
        sql = "SELECT count(f1,f2) FROM test1;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_93(self):
        sql = "SELECT count(f1,f2) FROM test1;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_94(self):
        sql = "SELECT f1 FROM test1 ORDER BY f2, f1;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_95(self):
        sql = "SELECT f1 FROM test1 WHERE 4.3+2.4 OR 1 ORDER BY f1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_96(self):
        sql = "SELECT f1 FROM test1 WHERE ('x' || f1) BETWEEN 'x10' AND 'x20'\nORDER BY f1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_97(self):
        sql = "SELECT f1 FROM test1 WHERE 5-3==2\nORDER BY f1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_98(self):
        sql = "SELECT coalesce(f1/(f1-11),'x'),\ncoalesce(min(f1/(f1-11),5),'y'),\ncoalesce(max(f1/(f1-33),6),'z')\nFROM test1 ORDER BY f1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_99(self):
        sql = "SELECT min(1,2,3), -max(1,2,3)\nFROM test1 ORDER BY f1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_100(self):
        sql = "SELECT * FROM test1 WHERE f1<0"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_101(self):
        sql = "PRAGMA empty_result_callbacks=on"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_102(self):
        sql = "SELECT * FROM test1 WHERE f1<0"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_103(self):
        sql = "SELECT * FROM test1 WHERE f1<(select count(*) from test2)"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_104(self):
        sql = "SELECT * FROM test1 ORDER BY f1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_105(self):
        sql = "SELECT * FROM test1 WHERE f1<0 ORDER BY f1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_106(self):
        sql = "SELECT f1 AS x FROM test1 ORDER BY x"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_107(self):
        sql = "SELECT f1 AS x FROM test1 ORDER BY -x"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_108(self):
        sql = "SELECT f1-23 AS x FROM test1 ORDER BY abs(x)"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_109(self):
        sql = "SELECT f1-23 AS x FROM test1 ORDER BY -abs(x)"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_110(self):
        sql = "SELECT f1-22 AS x, f2-22 as y FROM test1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_111(self):
        sql = "SELECT f1-22 AS x, f2-22 as y FROM test1 WHERE x>0 AND y<50"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_112(self):
        sql = "SELECT f1 COLLATE nocase AS x FROM test1 ORDER BY x"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_113(self):
        sql = "SELECT * FROM t3, t4;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_114(self):
        sql = "SELECT t3.*, t4.b FROM t3, t4;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_115(self):
        sql = "SELECT \"t3\".*, t4.b FROM t3, t4;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_116(self):
        sql = "SELECT t3.b, t4.* FROM t3, t4;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_117(self):
        sql = "DELETE FROM t3;\nINSERT INTO t3 VALUES(1,2);"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_118(self):
        sql = "SELECT * FROM t3 UNION SELECT 3 AS 'a', 4 ORDER BY a;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_119(self):
        sql = "SELECT 3, 4 UNION SELECT * FROM t3;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_120(self):
        sql = "SELECT * FROM t3 WHERE a=(SELECT 1);"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_121(self):
        sql = "SELECT * FROM t3 WHERE a=(SELECT 2);"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_122(self):
        sql = "BEGIN;\ncreate TABLE abc(a, b, c, PRIMARY KEY(a, b));\nINSERT INTO abc VALUES(1, 1, 1);"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_123(self):
        sql = "INSERT INTO abc SELECT a+(select max(a) FROM abc),\nb+(select max(a) FROM abc), c+(select max(a) FROM abc) FROM abc;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_124(self):
        sql = "COMMIT"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_125(self):
        sql = "SELECT count(\n(SELECT a FROM abc WHERE a = NULL AND b >= upper.c)\n) FROM abc AS upper;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_126(self):
        sql = "SELECT name FROM sqlite_master WHERE type = 'table'"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_127(self):
        sql = "SELECT * FROM sqlite_master WHERE rowid>10;\nSELECT * FROM sqlite_master WHERE rowid=10;\nSELECT * FROM sqlite_master WHERE rowid<10;\nSELECT * FROM sqlite_master WHERE rowid<=10;\nSELECT * FROM sqlite_master WHERE rowid>=10;\nSELECT * FROM sqlite_master;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_128(self):
        sql = "SELECT 10 IN (SELECT rowid FROM sqlite_master);"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_129(self):
        sql = "CREATE TABLE t1(a);\nCREATE INDEX i1 ON t1(a);\nINSERT INTO t1 VALUES(1);\nINSERT INTO t1 VALUES(2);\nINSERT INTO t1 VALUES(3);"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_130(self):
        sql = "DROP INDEX i1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_131(self):
        sql = "SELECT 2 IN (SELECT a FROM t1)"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_132(self):
        sql = "CREATE TABLE tbl1(f1 int, f2 int)"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_133(self):
        sql = "BEGIN"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_134(self):
        sql = "COMMIT"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_135(self):
        sql = "CREATE TABLE tbl2(f1 int, f2 int, f3 int); BEGIN;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_136(self):
        sql = "INSERT INTO tbl2 VALUES(i,i2,i3)"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_137(self):
        sql = "COMMIT"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_138(self):
        sql = "DROP TABLE tbl2"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_139(self):
        sql = "SELECT count(*) FROM tbl2"
        result = parse(sql)
        expected = {"from": "tbl2", "select": {"value": "*", "aggregate": "count"}}
        self.assertEqual(result, expected)

    def test_140(self):
        sql = "SELECT count(*) FROM tbl2 WHERE f2>1000"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_141(self):
        sql = "SELECT f1 FROM tbl2 WHERE 1000=f2"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_142(self):
        sql = "CREATE INDEX idx1 ON tbl2(f2)"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_143(self):
        sql = "SELECT f1 FROM tbl2 WHERE 1000=f2"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_144(self):
        sql = "SELECT f1 FROM tbl2 WHERE f2=1000"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_145(self):
        sql = "SELECT * FROM tbl2 WHERE 1000=f2"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_146(self):
        sql = "SELECT * FROM tbl2 WHERE f2=1000"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_147(self):
        sql = "DROP INDEX idx1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_148(self):
        sql = "SELECT f1 FROM tbl2 WHERE f2==2000"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_149(self):
        sql = "CREATE TABLE aa(a);\nCREATE TABLE bb(b);\nINSERT INTO aa VALUES(1);\nINSERT INTO aa VALUES(3);\nINSERT INTO bb VALUES(2);\nINSERT INTO bb VALUES(4);\nSELECT * FROM aa, bb WHERE max(a,b)>2;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_150(self):
        sql = "INSERT INTO bb VALUES(0);\nSELECT * FROM aa CROSS JOIN bb WHERE b;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_151(self):
        sql = "SELECT * FROM aa CROSS JOIN bb WHERE NOT b;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_152(self):
        sql = "SELECT * FROM aa, bb WHERE min(a,b);"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_153(self):
        sql = "SELECT * FROM aa, bb WHERE NOT min(a,b);"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_154(self):
        sql = "SELECT * FROM aa, bb WHERE CASE WHEN a=b-1 THEN 1 END;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_155(self):
        sql = "SELECT * FROM aa, bb WHERE CASE WHEN a=b-1 THEN 0 ELSE 1 END;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_156(self):
        sql = "CREATE TABLE t1(n int, log int);\nBEGIN;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_157(self):
        sql = "COMMIT"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_158(self):
        sql = "SELECT DISTINCT log FROM t1 ORDER BY log"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_159(self):
        sql = "SELECT count(*) FROM t1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_160(self):
        sql = "SELECT min(n),min(log),max(n),max(log),sum(n),sum(log),avg(n),avg(log)\nFROM t1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_161(self):
        sql = "SELECT max(n)/avg(n), max(log)/avg(log) FROM t1"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_162(self):
        sql = "SELECT log, count(*) FROM t1 GROUP BY log ORDER BY log"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_163(self):
        sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY log"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_164(self):
        sql = "SELECT log, avg(n) FROM t1 GROUP BY log ORDER BY log"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_165(self):
        sql = "SELECT log, avg(n)+1 FROM t1 GROUP BY log ORDER BY log"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_166(self):
        sql = "SELECT log, avg(n)-min(n) FROM t1 GROUP BY log ORDER BY log"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_167(self):
        sql = "SELECT log*2+1, avg(n)-min(n) FROM t1 GROUP BY log ORDER BY log"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_168(self):
        sql = "SELECT log*2+1 as x, count(*) FROM t1 GROUP BY x ORDER BY x"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_169(self):
        sql = "SELECT log*2+1 AS x, count(*) AS y FROM t1 GROUP BY x ORDER BY y, x"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_170(self):
        sql = "SELECT log*2+1 AS x, count(*) AS y FROM t1 GROUP BY x ORDER BY 10-(x+y)"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_171(self):
        sql = "SELECT log, count(*) FROM t1 GROUP BY something HAVING log>=4"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_172(self):
        sql = "SELECT log, count(*) FROM t1 GROUP BY log HAVING log>=4 ORDER BY log"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_173(self):
        sql = "SELECT log, count(*) FROM t1\nGROUP BY log\nHAVING count(*)>=4\nORDER BY log"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_174(self):
        sql = "SELECT log, count(*) FROM t1\nGROUP BY log\nHAVING count(*)>=4\nORDER BY max(n)+0"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_175(self):
        sql = "SELECT log AS x, count(*) AS y FROM t1\nGROUP BY x\nHAVING y>=4\nORDER BY max(n)+0"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_176(self):
        sql = "SELECT log AS x FROM t1\nGROUP BY x\nHAVING count(*)>=4\nORDER BY max(n)+0"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_177(self):
        sql = "SELECT log, count(*), avg(n), max(n+log*2) FROM t1\nGROUP BY log\nORDER BY max(n+log*2)+0, avg(n)+0"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_178(self):
        sql = "SELECT log, count(*), avg(n), max(n+log*2) FROM t1\nGROUP BY log\nORDER BY max(n+log*2)+0, min(log,avg(n))+0"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_179(self):
        sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY log;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_180(self):
        sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY log DESC;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_181(self):
        sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY 1;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_182(self):
        sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY 1 DESC;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_183(self):
        sql = "CREATE INDEX i1 ON t1(log);\nSELECT log, min(n) FROM t1 GROUP BY log ORDER BY log;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_184(self):
        sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY log DESC;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_185(self):
        sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY 1;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_186(self):
        sql = "SELECT log, min(n) FROM t1 GROUP BY log ORDER BY 1 DESC;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_187(self):
        sql = "CREATE TABLE t2(a,b);\nINSERT INTO t2 VALUES(1,2);\nSELECT a, sum(b) FROM t2 WHERE b=5 GROUP BY a;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_188(self):
        sql = "SELECT a, sum(b) FROM t2 WHERE b=5;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_189(self):
        sql = "CREATE TABLE A (\nA1 DOUBLE,\nA2 VARCHAR COLLATE NOCASE,\nA3 DOUBLE\n);\nINSERT INTO A VALUES(39136,'ABC',1201900000);\nINSERT INTO A VALUES(39136,'ABC',1207000000);\nSELECT typeof(sum(a3)) FROM a;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_190(self):
        sql = "SELECT typeof(sum(a3)) FROM a GROUP BY a1;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_191(self):
        sql = "CREATE TABLE t1(n int, log int);\nBEGIN;"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)

    def test_192(self):
        sql = "DROP TABLE t2"
        result = parse(sql)
        expected = "error"
        self.assertEqual(result, expected)
