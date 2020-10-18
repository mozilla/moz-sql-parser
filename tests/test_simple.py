# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Author: Kyle Lahnakoski (kyle@lahnakoski.com)
#

from __future__ import absolute_import, division, unicode_literals

import json
from unittest import TestCase, skipIf

from mo_dots import Null

from moz_sql_parser import parse
from test_resources import IS_MASTER

try:
    from tests.util import assertRaises
except ImportError:
    from .util import assertRaises  # RELATIVE IMPORT SO WE CAN RUN IN pyLibrary


class TestSimple(TestCase):
    def test_two_tables(self):
        result = parse("SELECT * from XYZZY, ABC")
        expected = {"select": "*", "from": ["XYZZY", "ABC"]}
        self.assertEqual(result, expected)

    def test_dot_table_name(self):
        result = parse("select * from SYS.XYZZY")
        expected = {"select": "*", "from": "SYS.XYZZY"}
        self.assertEqual(result, expected)

    def test_select_one_column(self):
        result = parse("Select A from dual")
        expected = {"select": {"value": "A"}, "from": "dual"}
        self.assertEqual(result, expected)

    def test_select_quote(self):
        result = parse("Select '''' from dual")
        expected = {"select": {"value": {"literal": "'"}}, "from": "dual"}
        self.assertEqual(result, expected)

    def test_select_quoted_name(self):
        result = parse('Select a "@*#&", b as test."g.g".c from dual')
        expected = {
            "select": [
                {"name": "@*#&", "value": "a"},
                {"name": "test.g.g.c", "value": "b"},
            ],
            "from": "dual",
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
                {"mul": [45, "c"]},
                {"div": [2, "d"]},
            ]}},
            "from": "dual",
        }
        self.assertEqual(result, expected)

    def test_select_underscore_name(self):
        #                         1         2         3         4         5         6
        #               0123456789012345678901234567890123456789012345678901234567890123456789
        result = parse("select _id from dual")
        expected = {"select": {"value": "_id"}, "from": "dual"}
        self.assertEqual(result, expected)

    def test_select_dots_names(self):
        #                         1         2         3         4         5         6
        #               0123456789012345678901234567890123456789012345678901234567890123456789
        result = parse("select a.b.c._d from dual")
        expected = {"select": {"value": "a.b.c._d"}, "from": "dual"}
        self.assertEqual(result, expected)

    def test_select_many_column(self):
        result = parse("Select a, b, c from dual")
        expected = {
            "select": [{"value": "a"}, {"value": "b"}, {"value": "c"}],
            "from": "dual",
        }
        self.assertEqual(result, expected)

    def test_bad_select1(self):
        with self.assertRaises(Exception):
            # was 'Expecting select'
            parse("se1ect A, B, C from dual")

    def test_bad_select2(self):
        with self.assertRaises(Exception):
            # was 'Expecting {{expression1 + [{[as] + column_name1}]}'
            parse("Select &&& FROM dual")

    def test_bad_from(self):
        assertRaises("(at char 20)", lambda: parse("select A, B, C frum dual"))

    def test_incomplete1(self):
        with self.assertRaises(Exception):
            # was 'Expecting {{expression1 + [{[as] + column_name1}]}}'
            parse("SELECT")

    def test_incomplete2(self):
        assertRaises("", lambda: parse("SELECT * FROM"))

    def test_where_neq(self):
        #                         1         2         3         4         5         6
        #               0123456789012345678901234567890123456789012345678901234567890123456789
        result = parse("SELECT * FROM dual WHERE a<>'test'")
        expected = {
            "select": "*",
            "from": "dual",
            "where": {"neq": ["a", {"literal": "test"}]},
        }
        self.assertEqual(result, expected)

    def test_where_in(self):
        result = parse("SELECT a FROM dual WHERE a in ('r', 'g', 'b')")
        expected = {
            "select": {"value": "a"},
            "from": "dual",
            "where": {"in": ["a", {"literal": ["r", "g", "b"]}]},
        }
        self.assertEqual(result, expected)

    def test_where_in_and_in(self):
        #                         1         2         3         4         5         6
        #               0123456789012345678901234567890123456789012345678901234567890123456789
        result = parse(
            "SELECT a FROM dual WHERE a in ('r', 'g', 'b') AND b in (10, 11, 12)"
        )
        expected = {
            "select": {"value": "a"},
            "from": "dual",
            "where": {"and": [
                {"in": ["a", {"literal": ["r", "g", "b"]}]},
                {"in": ["b", [10, 11, 12]]},
            ]},
        }
        self.assertEqual(result, expected)

    def test_eq(self):
        result = parse("SELECT a, b FROM t1, t2 WHERE t1.a=t2.b")
        expected = {
            "select": [{"value": "a"}, {"value": "b"}],
            "from": ["t1", "t2"],
            "where": {"eq": ["t1.a", "t2.b"]},
        }
        self.assertEqual(result, expected)

    def test_is_null(self):
        result = parse("SELECT a, b FROM t1 WHERE t1.a IS NULL")
        expected = {
            "select": [{"value": "a"}, {"value": "b"}],
            "from": "t1",
            "where": {"missing": "t1.a"},
        }
        self.assertEqual(result, expected)

    def test_is_not_null(self):
        result = parse("SELECT a, b FROM t1 WHERE t1.a IS NOT NULL")
        expected = {
            "select": [{"value": "a"}, {"value": "b"}],
            "from": "t1",
            "where": {"exists": "t1.a"},
        }
        self.assertEqual(result, expected)

    def test_groupby(self):
        result = parse("select a, count(1) as b from mytable group by a")
        expected = {
            "select": [{"value": "a"}, {"name": "b", "value": {"count": 1}}],
            "from": "mytable",
            "groupby": {"value": "a"},
        }
        self.assertEqual(result, expected)

    def test_function(self):
        #               0         1         2
        #               0123456789012345678901234567890
        result = parse("select count(1) from mytable")
        expected = {"select": {"value": {"count": 1}}, "from": "mytable"}
        self.assertEqual(result, expected)

    def test_function_underscore(self):
        #               0         1         2
        #               0123456789012345678901234567890
        result = parse("select DATE_TRUNC('2019-04-12', WEEK) from mytable")
        expected = {
            "select": {"value": {"date_trunc": [{"literal": "2019-04-12"}, "WEEK"]}},
            "from": "mytable",
        }
        self.assertEqual(result, expected)

    def test_order_by(self):
        result = parse("select count(1) from dual order by a")
        expected = {
            "select": {"value": {"count": 1}},
            "from": "dual",
            "orderby": {"value": "a"},
        }
        self.assertEqual(result, expected)

    def test_order_by_asc(self):
        result = parse("select count(1) from dual order by a asc")
        expected = {
            "select": {"value": {"count": 1}},
            "from": "dual",
            "orderby": {"value": "a", "sort": "asc"},
        }
        self.assertEqual(result, expected)

    def test_neg_or_precedence(self):
        result = parse("select B,C from table1 where A=-900 or B=100")
        expected = {
            "from": "table1",
            "where": {"or": [{"eq": ["A", -900]}, {"eq": ["B", 100]}]},
            "select": [{"value": "B"}, {"value": "C"}],
        }
        self.assertEqual(result, expected)

    def test_negative_number(self):
        result = parse("select a from table1 where A=-900")
        expected = {
            "from": "table1",
            "where": {"eq": ["A", -900]},
            "select": {"value": "a"},
        }
        self.assertEqual(result, expected)

    def test_like_in_where(self):
        result = parse("select a from table1 where A like '%20%'")
        expected = {
            "from": "table1",
            "where": {"like": ["A", {"literal": "%20%"}]},
            "select": {"value": "a"},
        }
        self.assertEqual(result, expected)

    def test_not_like_in_where(self):
        result = parse("select a from table1 where A not like '%20%'")
        expected = {
            "from": "table1",
            "where": {"not_like": ["A", {"literal": "%20%"}]},
            "select": {"value": "a"},
        }
        self.assertEqual(result, expected)

    def test_like_in_select(self):
        #               0         1         2         3         4         5         6
        #               0123456789012345678901234567890123456789012345678901234567890123456789
        result = parse(
            "select case when A like 'bb%' then 1 else 0 end as bb from table1"
        )
        expected = {
            "from": "table1",
            "select": {
                "name": "bb",
                "value": {"case": [
                    {"when": {"like": ["A", {"literal": "bb%"}]}, "then": 1},
                    0,
                ]},
            },
        }
        self.assertEqual(result, expected)

    def test_not_like_in_select(self):
        result = parse(
            "select case when A not like 'bb%' then 1 else 0 end as bb from table1"
        )
        expected = {
            "from": "table1",
            "select": {
                "name": "bb",
                "value": {"case": [
                    {"when": {"not_like": ["A", {"literal": "bb%"}]}, "then": 1},
                    0,
                ]},
            },
        }
        self.assertEqual(result, expected)

    def test_like_from_pr16(self):
        result = parse(
            "select * from trade where school LIKE '%shool' and name='abc' and id IN"
            " ('1','2')"
        )
        expected = {
            "from": "trade",
            "where": {"and": [
                {"like": ["school", {"literal": "%shool"}]},
                {"eq": ["name", {"literal": "abc"}]},
                {"in": ["id", {"literal": ["1", "2"]}]},
            ]},
            "select": "*",
        }
        self.assertEqual(result, expected)

    def test_in_expression(self):
        result = parse(
            "select * from task where repo.branch.name in ('try', 'mozilla-central')"
        )
        expected = {
            "from": "task",
            "select": "*",
            "where": {"in": [
                "repo.branch.name",
                {"literal": ["try", "mozilla-central"]},
            ]},
        }
        self.assertEqual(result, expected)

    def test_not_in_expression(self):
        result = parse(
            "select * from task where repo.branch.name not in ('try',"
            " 'mozilla-central')"
        )
        expected = {
            "from": "task",
            "select": "*",
            "where": {"nin": [
                "repo.branch.name",
                {"literal": ["try", "mozilla-central"]},
            ]},
        }
        self.assertEqual(result, expected)

    def test_joined_table_name(self):
        result = parse("SELECT * FROM table1 t1 JOIN table3 t3 ON t1.id = t3.id")

        expected = {
            "from": [
                {"name": "t1", "value": "table1"},
                {
                    "on": {"eq": ["t1.id", "t3.id"]},
                    "join": {"name": "t3", "value": "table3"},
                },
            ],
            "select": "*",
        }
        self.assertEqual(result, expected)

    def test_not_equal(self):
        #               0         1         2         3         4         5         6        7          8
        #               012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
        result = parse(
            "select * from task where build.product is not null and"
            " build.product!='firefox'"
        )

        expected = {
            "select": "*",
            "from": "task",
            "where": {"and": [
                {"exists": "build.product"},
                {"neq": ["build.product", {"literal": "firefox"}]},
            ]},
        }
        self.assertEqual(result, expected)

    def test_pr19(self):
        result = parse("select empid from emp where ename like 's%' ")
        expected = {
            "from": "emp",
            "where": {"like": ["ename", {"literal": "s%"}]},
            "select": {"value": "empid"},
        }
        self.assertEqual(result, expected)

    def test_backtick(self):
        result = parse("SELECT `user ID` FROM a")
        expected = {"select": {"value": "user ID"}, "from": "a"}
        self.assertEqual(result, expected)

    def test_backtick_escape(self):
        result = parse("SELECT `user`` ID` FROM a")
        expected = {"select": {"value": "user` ID"}, "from": "a"}
        self.assertEqual(result, expected)

    def test_left_join(self):
        result = parse("SELECT t1.field1 FROM t1 LEFT JOIN t2 ON t1.id = t2.id")
        expected = {
            "select": {"value": "t1.field1"},
            "from": ["t1", {"left join": "t2", "on": {"eq": ["t1.id", "t2.id"]}}],
        }
        self.assertEqual(result, expected)

    def test_multiple_left_join(self):
        result = parse(
            "SELECT t1.field1 "
            "FROM t1 "
            "LEFT JOIN t2 ON t1.id = t2.id "
            "LEFT JOIN t3 ON t1.id = t3.id"
        )
        expected = {
            "select": {"value": "t1.field1"},
            "from": [
                "t1",
                {"left join": "t2", "on": {"eq": ["t1.id", "t2.id"]}},
                {"left join": "t3", "on": {"eq": ["t1.id", "t3.id"]}},
            ],
        }
        self.assertEqual(result, expected)

    def test_union(self):
        result = parse("SELECT b FROM t6 UNION SELECT '3' AS x ORDER BY x")
        expected = {
            "from": {"union": [
                {"from": "t6", "select": {"value": "b"}},
                {"select": {"value": {"literal": "3"}, "name": "x"}},
            ]},
            "orderby": {"value": "x"},
        }
        self.assertEqual(result, expected)

    def test_left_outer_join(self):
        result = parse("SELECT t1.field1 FROM t1 LEFT OUTER JOIN t2 ON t1.id = t2.id")
        expected = {
            "select": {"value": "t1.field1"},
            "from": ["t1", {"left outer join": "t2", "on": {"eq": ["t1.id", "t2.id"]}}],
        }
        self.assertEqual(result, expected)

    def test_right_join(self):
        result = parse("SELECT t1.field1 FROM t1 RIGHT JOIN t2 ON t1.id = t2.id")
        expected = {
            "select": {"value": "t1.field1"},
            "from": ["t1", {"right join": "t2", "on": {"eq": ["t1.id", "t2.id"]}}],
        }
        self.assertEqual(result, expected)

    def test_right_outer_join(self):
        result = parse("SELECT t1.field1 FROM t1 RIGHT OUTER JOIN t2 ON t1.id = t2.id")
        expected = {
            "select": {"value": "t1.field1"},
            "from": [
                "t1",
                {"right outer join": "t2", "on": {"eq": ["t1.id", "t2.id"]}},
            ],
        }
        self.assertEqual(result, expected)

    def test_full_join(self):
        result = parse("SELECT t1.field1 FROM t1 FULL JOIN t2 ON t1.id = t2.id")
        expected = {
            "select": {"value": "t1.field1"},
            "from": ["t1", {"full join": "t2", "on": {"eq": ["t1.id", "t2.id"]}}],
        }
        self.assertEqual(result, expected)

    def test_full_outer_join(self):
        result = parse("SELECT t1.field1 FROM t1 FULL OUTER JOIN t2 ON t1.id = t2.id")
        expected = {
            "select": {"value": "t1.field1"},
            "from": ["t1", {"full outer join": "t2", "on": {"eq": ["t1.id", "t2.id"]}}],
        }
        self.assertEqual(result, expected)

    def test_join_via_using(self):
        result = parse("SELECT t1.field1 FROM t1 JOIN t2 USING (id)")
        expected = {
            "select": {"value": "t1.field1"},
            "from": ["t1", {"join": "t2", "using": "id"}],
        }
        self.assertEqual(result, expected)

    def test_where_between(self):
        result = parse("SELECT a FROM dual WHERE a BETWEEN 1 and 2")
        expected = {
            "select": {"value": "a"},
            "from": "dual",
            "where": {"between": ["a", 1, 2]},
        }
        self.assertEqual(result, expected)

    def test_where_not_between(self):
        result = parse("SELECT a FROM dual WHERE a NOT BETWEEN 1 and 2")
        expected = {
            "select": {"value": "a"},
            "from": "dual",
            "where": {"not_between": ["a", 1, 2]},
        }
        self.assertEqual(result, expected)

    def test_select_from_select(self):
        #               0         1         2         3
        #               0123456789012345678901234567890123456789
        result = parse("SELECT b.a FROM ( SELECT 2 AS a ) b")
        expected = {
            "select": {"value": "b.a"},
            "from": {"name": "b", "value": {"select": {"value": 2, "name": "a"}}},
        }
        self.assertEqual(result, expected)

    def test_unicode_strings(self):
        result = parse("select '0:普通,1:旗舰' from mobile")
        expected = {"select": {"value": {"literal": "0:普通,1:旗舰"}}, "from": "mobile"}
        self.assertEqual(result, expected)

    def test_issue68(self):
        result = parse("select deflate(sum(int(mobile_price.price))) from mobile")
        expected = {
            "select": {"value": {"deflate": {"sum": {"int": "mobile_price.price"}}}},
            "from": "mobile",
        }
        self.assertEqual(result, expected)

    def test_issue_90(self):
        result = parse(
            """SELECT MIN(cn.name) AS from_company
        FROM company_name AS cn, company_type AS ct, keyword AS k, movie_link AS ml, title AS t
        WHERE cn.country_code !='[pl]' AND ct.kind IS NOT NULL AND t.production_year > 1950 AND ml.movie_id = t.id
        """
        )

        expected = {
            "select": {"value": {"min": "cn.name"}, "name": "from_company"},
            "from": [
                {"value": "company_name", "name": "cn"},
                {"value": "company_type", "name": "ct"},
                {"value": "keyword", "name": "k"},
                {"value": "movie_link", "name": "ml"},
                {"value": "title", "name": "t"},
            ],
            "where": {"and": [
                {"neq": ["cn.country_code", {"literal": "[pl]"}]},
                {"exists": "ct.kind"},
                {"gt": ["t.production_year", 1950]},
                {"eq": ["ml.movie_id", "t.id"]},
            ]},
        }
        self.assertEqual(result, expected)

    def test_issue_68a(self):
        sql = """
        SELECT * 
        FROM aka_name AS an, cast_info AS ci, info_type AS it, link_type AS lt, movie_link AS ml, name AS n, person_info AS pi, title AS t 
        WHERE
            an.name  is not NULL
            and (an.name LIKE '%a%' or an.name LIKE 'A%')
            AND it.info ='mini biography'
            AND lt.link  in ('references', 'referenced in', 'features', 'featured in')
            AND n.name_pcode_cf BETWEEN 'A' AND 'F'
            AND (n.gender = 'm' OR (n.gender = 'f' AND n.name LIKE 'A%'))
            AND pi.note  is not NULL
            AND t.production_year BETWEEN 1980 AND 2010
            AND n.id = an.person_id 
            AND n.id = pi.person_id 
            AND ci.person_id = n.id 
            AND t.id = ci.movie_id 
            AND ml.linked_movie_id = t.id 
            AND lt.id = ml.link_type_id 
            AND it.id = pi.info_type_id 
            AND pi.person_id = an.person_id 
            AND pi.person_id = ci.person_id 
            AND an.person_id = ci.person_id 
            AND ci.movie_id = ml.linked_movie_id
        """
        result = parse(sql)
        expected = {
            "from": [
                {"name": "an", "value": "aka_name"},
                {"name": "ci", "value": "cast_info"},
                {"name": "it", "value": "info_type"},
                {"name": "lt", "value": "link_type"},
                {"name": "ml", "value": "movie_link"},
                {"name": "n", "value": "name"},
                {"name": "pi", "value": "person_info"},
                {"name": "t", "value": "title"},
            ],
            "select": "*",
            "where": {"and": [
                {"exists": "an.name"},
                {"or": [
                    {"like": ["an.name", {"literal": "%a%"}]},
                    {"like": ["an.name", {"literal": "A%"}]},
                ]},
                {"eq": ["it.info", {"literal": "mini biography"}]},
                {"in": [
                    "lt.link",
                    {"literal": [
                        "references",
                        "referenced in",
                        "features",
                        "featured in",
                    ]},
                ]},
                {"between": ["n.name_pcode_cf", {"literal": "A"}, {"literal": "F"}]},
                {"or": [
                    {"eq": ["n.gender", {"literal": "m"}]},
                    {"and": [
                        {"eq": ["n.gender", {"literal": "f"}]},
                        {"like": ["n.name", {"literal": "A%"}]},
                    ]},
                ]},
                {"exists": "pi.note"},
                {"between": ["t.production_year", 1980, 2010]},
                {"eq": ["n.id", "an.person_id"]},
                {"eq": ["n.id", "pi.person_id"]},
                {"eq": ["ci.person_id", "n.id"]},
                {"eq": ["t.id", "ci.movie_id"]},
                {"eq": ["ml.linked_movie_id", "t.id"]},
                {"eq": ["lt.id", "ml.link_type_id"]},
                {"eq": ["it.id", "pi.info_type_id"]},
                {"eq": ["pi.person_id", "an.person_id"]},
                {"eq": ["pi.person_id", "ci.person_id"]},
                {"eq": ["an.person_id", "ci.person_id"]},
                {"eq": ["ci.movie_id", "ml.linked_movie_id"]},
            ]},
        }
        self.assertEqual(result, expected)

    def test_issue_68b(self):
        #      0         1         2         3         4         5         6         7         8         9
        #      012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
        sql = (
            "SELECT COUNT(*) AS CNT FROM test.tb WHERE (id IN (unhex('1'),unhex('2')))"
            " AND  status=1;"
        )
        result = parse(sql)
        expected = {
            "select": {"value": {"count": "*"}, "name": "CNT"},
            "from": "test.tb",
            "where": {"and": [
                {"in": [
                    "id",
                    [{"unhex": {"literal": "1"}}, {"unhex": {"literal": "2"}}],
                ]},
                {"eq": ["status", 1]},
            ]},
        }
        self.assertEqual(result, expected)

    def test_binary_and(self):
        sql = "SELECT * FROM t WHERE  c & 4;"
        result = parse(sql)
        expected = {"select": "*", "from": "t", "where": {"binary_and": ["c", 4]}}
        self.assertEqual(result, expected)

    def test_binary_or(self):
        sql = "SELECT * FROM t WHERE c | 4;"
        result = parse(sql)
        expected = {"select": "*", "from": "t", "where": {"binary_or": ["c", 4]}}
        self.assertEqual(result, expected)

    def test_binary_not(self):
        #      0         1         2
        #      012345678901234567890123456789
        sql = "SELECT * FROM t WHERE ~c;"
        result = parse(sql)
        expected = {"select": "*", "from": "t", "where": {"binary_not": "c"}}
        self.assertEqual(result, expected)

    def test_or_and(self):
        sql = "SELECT * FROM dual WHERE a OR b AND c"
        result = parse(sql)
        expected = {
            "select": "*",
            "from": "dual",
            "where": {"or": ["a", {"and": ["b", "c"]}]},
        }
        self.assertEqual(result, expected)

    def test_and_or(self):
        sql = "SELECT * FROM dual WHERE a AND b or c"
        result = parse(sql)
        expected = {
            "select": "*",
            "from": "dual",
            "where": {"or": [{"and": ["a", "b"]}, "c"]},
        }
        self.assertEqual(result, expected)

    def test_underscore_function1(self):
        sql = "SELECT _()"
        result = parse(sql)
        expected = {
            "select": {"value": {"_": {}}},
        }
        self.assertEqual(result, expected)

    def test_underscore_function2(self):
        sql = "SELECT _a(a$b)"
        result = parse(sql)
        expected = {
            "select": {"value": {"_a": "a$b"}},
        }
        self.assertEqual(result, expected)

    def test_underscore_function3(self):
        sql = "SELECT _$$_(a, b$)"
        result = parse(sql)
        expected = {
            "select": {"value": {"_$$_": ["a", "b$"]}},
        }
        self.assertEqual(result, expected)

    def test_union_all1(self):
        #               0         1         2         3         4         5         6         7         8         9
        #               012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
        result = parse("SELECT b FROM t6 UNION ALL SELECT '3' AS x ORDER BY x")
        expected = {
            "from": {"union_all": [
                {"from": "t6", "select": {"value": "b"}},
                {"select": {"value": {"literal": "3"}, "name": "x"}},
            ]},
            "orderby": {"value": "x"},
        }
        self.assertEqual(result, expected)

    def test_union_all2(self):
        result = parse("SELECT b UNION ALL SELECT c")
        expected = {"union_all": [
            {"select": {"value": "b"}},
            {"select": {"value": "c"}},
        ]}
        self.assertEqual(result, expected)

    def test_issue106(self):
        result = parse(
            """
            SELECT *
            FROM MyTable
            GROUP BY Col
            HAVING AVG(X) >= 2
            AND AVG(X) <= 4
            OR AVG(X) = 5;
        """
        )
        expected = {
            "select": "*",
            "from": "MyTable",
            "groupby": {"value": "Col"},
            "having": {"or": [
                {"and": [{"gte": [{"avg": "X"}, 2]}, {"lte": [{"avg": "X"}, 4]}]},
                {"eq": [{"avg": "X"}, 5]},
            ]},
        }
        self.assertEqual(result, expected)

    def test_issue97_function_names(self):
        sql = "SELECT ST_AsText(ST_MakePoint(174, -36));"
        result = parse(sql)
        expected = {"select": {"value": {"st_astext": {"st_makepoint": [174, -36]}}}}
        self.assertEqual(result, expected)

    def test_issue91_order_of_operations1(self):
        sql = "select 5-4+2"
        result = parse(sql)
        expected = {"select": {"value": {"add": [{"sub": [5, 4]}, 2]}}}
        self.assertEqual(result, expected)

    def test_issue91_order_of_operations2(self):
        sql = "select 5/4*2"
        result = parse(sql)
        expected = {"select": {"value": {"mul": [{"div": [5, 4]}, 2]}}}
        self.assertEqual(result, expected)

    def test_issue_92(self):
        sql = "SELECT * FROM `movies`"
        result = parse(sql)
        expected = {"select": "*", "from": "movies"}
        self.assertEqual(result, expected)

    def test_with_clause(self):
        sql = (
            " WITH dept_count AS ("
            "     SELECT deptno, COUNT(*) AS dept_count"
            "     FROM emp"
            "     GROUP BY deptno"
            ")"
            " SELECT "
            "     e.ename AS employee_name,"
            "     dc1.dept_count AS emp_dept_count,"
            "     m.ename AS manager_name,"
            "     dc2.dept_count AS mgr_dept_count"
            " FROM "
            "     emp e,"
            "     dept_count dc1,"
            "     emp m,"
            "     dept_count dc2"
            " WHERE "
            "     e.deptno = dc1.deptno"
            "     AND e.mgr = m.empno"
            "     AND m.deptno = dc2.deptno;"
        )
        result = parse(sql)
        expected = {
            "with": {
                "name": "dept_count",
                "value": {
                    "from": "emp",
                    "groupby": {"value": "deptno"},
                    "select": [
                        {"value": "deptno"},
                        {"name": "dept_count", "value": {"count": "*"}},
                    ],
                },
            },
            "from": [
                {"name": "e", "value": "emp"},
                {"name": "dc1", "value": "dept_count"},
                {"name": "m", "value": "emp"},
                {"name": "dc2", "value": "dept_count"},
            ],
            "select": [
                {"name": "employee_name", "value": "e.ename"},
                {"name": "emp_dept_count", "value": "dc1.dept_count"},
                {"name": "manager_name", "value": "m.ename"},
                {"name": "mgr_dept_count", "value": "dc2.dept_count"},
            ],
            "where": {"and": [
                {"eq": ["e.deptno", "dc1.deptno"]},
                {"eq": ["e.mgr", "m.empno"]},
                {"eq": ["m.deptno", "dc2.deptno"]},
            ]},
        }

        self.assertEqual(result, expected)

    def test_2with_clause(self):
        #    0         1         2         3         4         5         6         7         8         9
        #    012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
        sql = (
            " WITH a AS (SELECT 1), b AS (SELECT 2)"
            " SELECT * FROM a UNION ALL SELECT * FROM b"
        )
        result = parse(sql)
        expected = {
            "with": [
                {"name": "a", "value": {"select": {"value": 1}}},
                {"name": "b", "value": {"select": {"value": 2}}},
            ],
            "union_all": [{"select": "*", "from": "a"}, {"select": "*", "from": "b"},],
        }
        self.assertEqual(result, expected)

    def test_issue_103b(self):
        #        0         1         2         3         4         5         6         7         8         9
        #        012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
        sql = """SELECT G.ITEM_ID AS "ITEM_ID", fn_get_dimension_by_grcom (H.BU_COD, G.ITEM_ID) AS "DESCRIPTION", trim(G.EAN11) "EANCODE", trim(DECODE (G.MATNR_ORIG_B2F, NULL, DECODE (G.MATNR_ORIG, NULL, G.MATNR, G.MATNR_ORIG), G.MATNR_ORIG_B2F)) AS "CODICE_PRODOTTO", DECODE (H.BRAND, 'BF GOODRICH', 'BFGOODRICH', H.BRAND) AS "BRAND_ID", H.BRAND AS "XBRAND", H.MARKET5 AS "MKT_ID", m.MC_COUNTRY_CODE AS "COUNTRY_CODE", H.BU_COD AS "GRCOM", H.DATE_FROM AS "PRICELIST_DATE", H.CURRENCY AS "CURRENCY_ID", K.CR_DESCRIPTION AS "CURRENCY_DESC", K.CR_DESCRIPTION AS "CURRENCY_SHORT_DESC", G.PATTERN AS "BTS_ID", P.PATTERN AS "PATTERN_SHORT_DESC", trim(G.SERIES) AS "SERIE", trim(G.WIDTH) AS "CORDA", trim(G.RIM) AS "CALETTAMENTO", G.STRUTTURA AS "STRUTTURA", DECODE (IS_NUMBER (G.WIDTH), 0, 0, TO_NUMBER (G.WIDTH)) AS "CORDA_NOM", DECODE (IS_NUMBER (G.SERIES), 0, 0, TO_NUMBER (G.SERIES)) AS "SERIE_NOM", 0 AS "STRUTTURA_NOM", DECODE (IS_NUMBER (G.RIM), 0, 0, TO_NUMBER (G.RIM)) AS "CALETTAMENTO_NOM", trim(G.LOADIN1) AS "LOAD_INDEX", trim(DECODE (TRIM (G.LOADIN2), '', NULL, TRIM (G.LOADIN2))) AS "LOAD_INDEX_1", trim(G.EXTRA_LOAD_FLAG) AS "EXTRA_LOAD_INDEX", G.RUNFLAT_FLAG AS "RUNFLAT_ID", DECODE (TRIM (G.OEMARK), '', NULL, TRIM (G.OEMARK)) AS "OE_MARK", trim(G.SPEEDIN1) AS "SPEED_INDEX", trim(DECODE (TRIM (G.SPEEDIN2), '', NULL, TRIM (G.SPEEDIN2))) AS "SPEED_INDEX_1", trim(G.CODE_MKS) AS "CODE_MKS", G.DESCR_MKS AS "MKS", D.PRICE AS "GROSS_PRICE", trim(fn_get_dimension_loadindex (g.item_id)) AS "DESCR_LOADINDEX", trim(fn_get_dimension_speedindex (g.item_id)) AS "DESCR_SPEEDINDEX", DECODE (TRIM (G.LOADIN1DB), '', NULL, TRIM (G.LOADIN1DB)) AS "LOADINDEX1DOUBLEMOUNT", DECODE (TRIM (G.LOADIN2DB), '', NULL, TRIM (G.LOADIN2DB)) AS "LOADINDEX2DOUBLEMOUNT", DECODE (TRIM (G.NOISECLASS), '', NULL, TRIM (G.NOISECLASS)) AS "NOISECLASS", DECODE (G.ARTICLEGROUPCODE, '01', 'Tyre', '02', 'Rim', NULL) AS "ARTICLEGROUP", G.ARTICLEGROUPCODE AS "ARTICLEGROUPCODE", DECODE (IS_NUMBER (G.DEPTH), 1, G.DEPTH, NULL) AS "ORIGINALTREADDEPTH", DECODE (IS_NUMBER (G.WEIGHT), 1, TO_NUMBER (G.WEIGHT) * 1000, NULL) AS "WEIGHT", DECODE (g.pncs, 'Yes', 1, 'No', 0, NULL) AS "PNCS", DECODE (g.sealind, 'Yes', 1, 'No', 0, NULL) AS "SELFSEALING", DECODE (g.sealind, 'Yes', g.RUNFLAT_FLAG_SEALIND, NULL) AS "SELFSEALINGINDICATOR", DECODE (g.extra_load, 'Yes', 1, 'No', 0, NULL) AS "EXTRA_LOAD", g.application_code AS "APPLICATION_CODE", NULL AS "PRODUCTSEGMENT", DECODE (g.application_code, 'F1', 'FittedUnitCar', 'F2', 'FittedUnitVan', 'F9', 'FittedUnitSuv', '01', 'Car', '02', 'Van', '03', 'Truck', '04', 'EM', '05', 'AS', '06', 'Industry', '08', 'Moto', '09', 'SUV', NULL) AS "APPLICATION", DECODE (g.SNOWFLAG, 'Yes', 1, 'No', 0, NULL) AS "SNOWFLAG", DECODE (g.RUNFLAT, 'Yes', 1, 'No', 0, NULL) AS "RUNFLAT", DECODE (TRIM (g.NOISE_PERFORMANCE), '', NULL, TRIM (G.NOISE_PERFORMANCE)) AS "NOISE_PERFORMANCE", DECODE (TRIM (g.rollres), '', NULL, TRIM (G.rollres)) AS "ROLLRES", DECODE (TRIM (g.wetgrip), '', NULL, TRIM (G.wetgrip)) AS "WETGRIP", g.MANUFACTURER AS "MANUFACTURER", DECODE (DECODE (IS_NUMBER (g.season), 1, TO_NUMBER (g.season), 0), 1, 'summer', 2, 'winter', 10, 'allseasons', NULL) AS "SEASONALITY" FROM DIM_CURRENCY k, P2_PATTERN_ALL p, P2_MATERIAL_ALL g, DW.DIM_MARKET_CHANNEL m, PRLST_DETAIL d, (SELECT H1.PRICELIST_ID, H1.BRAND, H1.BU_COD, H1.MARKET5, H1.DATE_FROM, H1.CURRENCY FROM PRCLST_HEADER h1, LOOKUP_BRAND b1 WHERE H1.ENABLE_VIEWING_B2F = 1 AND (H1.BRAND, H1.BU_COD, H1.MARKET5, H1.DATE_FROM) IN ( SELECT H2.BRAND, H2.BU_COD, H2.MARKET5, MAX (H2.DATE_FROM) FROM PRCLST_HEADER h2 WHERE H2.BU_COD = 'CAR' AND H2.ENABLE_VIEWING_B2F = 1 GROUP BY H2.BRAND, H2.BU_COD, H2.MARKET5) AND H1.BRAND = B1.BRAND) h WHERE h.currency = K.CR_COD_CURRENCY_SAP AND h.pricelist_id = D.PRICELIST_ID AND H.BRAND = G.BRCONA AND D.IPCODE = G.MATNR AND P.BRAND = G.BRCONA AND upper(P.PATTERN) = upper(G.PATTERN) AND h.market5 = m.MARKET_CHANNEL_CODE AND G.IS_USER = 1 AND (G.BRCONA, G.MATNR) NOT IN (SELECT C.BRCONA, C.MATNR FROM P2_MAT_USER_CONFLICTS c WHERE C.LAST_ACTION IN (21)) ORDER BY G.ITEM_ID"""
        result = parse(sql)
        expected = json.loads(
            """{"select": [{"value": "G.ITEM_ID", "name": "ITEM_ID"}, {"value": {"fn_get_dimension_by_grcom": ["H.BU_COD", "G.ITEM_ID"]}, "name": "DESCRIPTION"}, {"value": {"trim": "G.EAN11"}, "name": "EANCODE"}, {"value": {"trim": {"decode": ["G.MATNR_ORIG_B2F", null, {"decode": ["G.MATNR_ORIG", null, "G.MATNR", "G.MATNR_ORIG"]}, "G.MATNR_ORIG_B2F"]}}, "name": "CODICE_PRODOTTO"}, {"value": {"decode": ["H.BRAND", {"literal": "BF GOODRICH"}, {"literal": "BFGOODRICH"}, "H.BRAND"]}, "name": "BRAND_ID"}, {"value": "H.BRAND", "name": "XBRAND"}, {"value": "H.MARKET5", "name": "MKT_ID"}, {"value": "m.MC_COUNTRY_CODE", "name": "COUNTRY_CODE"}, {"value": "H.BU_COD", "name": "GRCOM"}, {"value": "H.DATE_FROM", "name": "PRICELIST_DATE"}, {"value": "H.CURRENCY", "name": "CURRENCY_ID"}, {"value": "K.CR_DESCRIPTION", "name": "CURRENCY_DESC"}, {"value": "K.CR_DESCRIPTION", "name": "CURRENCY_SHORT_DESC"}, {"value": "G.PATTERN", "name": "BTS_ID"}, {"value": "P.PATTERN", "name": "PATTERN_SHORT_DESC"}, {"value": {"trim": "G.SERIES"}, "name": "SERIE"}, {"value": {"trim": "G.WIDTH"}, "name": "CORDA"}, {"value": {"trim": "G.RIM"}, "name": "CALETTAMENTO"}, {"value": "G.STRUTTURA", "name": "STRUTTURA"}, {"value": {"decode": [{"is_number": "G.WIDTH"}, 0, 0, {"to_number": "G.WIDTH"}]}, "name": "CORDA_NOM"}, {"value": {"decode": [{"is_number": "G.SERIES"}, 0, 0, {"to_number": "G.SERIES"}]}, "name": "SERIE_NOM"}, {"value": 0, "name": "STRUTTURA_NOM"}, {"value": {"decode": [{"is_number": "G.RIM"}, 0, 0, {"to_number": "G.RIM"}]}, "name": "CALETTAMENTO_NOM"}, {"value": {"trim": "G.LOADIN1"}, "name": "LOAD_INDEX"}, {"value": {"trim": {"decode": [{"trim": "G.LOADIN2"}, {"literal": ""}, null, {"trim": "G.LOADIN2"}]}}, "name": "LOAD_INDEX_1"}, {"value": {"trim": "G.EXTRA_LOAD_FLAG"}, "name": "EXTRA_LOAD_INDEX"}, {"value": "G.RUNFLAT_FLAG", "name": "RUNFLAT_ID"}, {"value": {"decode": [{"trim": "G.OEMARK"}, {"literal": ""}, null, {"trim": "G.OEMARK"}]}, "name": "OE_MARK"}, {"value": {"trim": "G.SPEEDIN1"}, "name": "SPEED_INDEX"}, {"value": {"trim": {"decode": [{"trim": "G.SPEEDIN2"}, {"literal": ""}, null, {"trim": "G.SPEEDIN2"}]}}, "name": "SPEED_INDEX_1"}, {"value": {"trim": "G.CODE_MKS"}, "name": "CODE_MKS"}, {"value": "G.DESCR_MKS", "name": "MKS"}, {"value": "D.PRICE", "name": "GROSS_PRICE"}, {"value": {"trim": {"fn_get_dimension_loadindex": "g.item_id"}}, "name": "DESCR_LOADINDEX"}, {"value": {"trim": {"fn_get_dimension_speedindex": "g.item_id"}}, "name": "DESCR_SPEEDINDEX"}, {"value": {"decode": [{"trim": "G.LOADIN1DB"}, {"literal": ""}, null, {"trim": "G.LOADIN1DB"}]}, "name": "LOADINDEX1DOUBLEMOUNT"}, {"value": {"decode": [{"trim": "G.LOADIN2DB"}, {"literal": ""}, null, {"trim": "G.LOADIN2DB"}]}, "name": "LOADINDEX2DOUBLEMOUNT"}, {"value": {"decode": [{"trim": "G.NOISECLASS"}, {"literal": ""}, null, {"trim": "G.NOISECLASS"}]}, "name": "NOISECLASS"}, {"value": {"decode": ["G.ARTICLEGROUPCODE", {"literal": "01"}, {"literal": "Tyre"}, {"literal": "02"}, {"literal": "Rim"}, null]}, "name": "ARTICLEGROUP"}, {"value": "G.ARTICLEGROUPCODE", "name": "ARTICLEGROUPCODE"}, {"value": {"decode": [{"is_number": "G.DEPTH"}, 1, "G.DEPTH", null]}, "name": "ORIGINALTREADDEPTH"}, {"value": {"decode": [{"is_number": "G.WEIGHT"}, 1, {"mul": [{"to_number": "G.WEIGHT"}, 1000]}, null]}, "name": "WEIGHT"}, {"value": {"decode": ["g.pncs", {"literal": "Yes"}, 1, {"literal": "No"}, 0, null]}, "name": "PNCS"}, {"value": {"decode": ["g.sealind", {"literal": "Yes"}, 1, {"literal": "No"}, 0, null]}, "name": "SELFSEALING"}, {"value": {"decode": ["g.sealind", {"literal": "Yes"}, "g.RUNFLAT_FLAG_SEALIND", null]}, "name": "SELFSEALINGINDICATOR"}, {"value": {"decode": ["g.extra_load", {"literal": "Yes"}, 1, {"literal": "No"}, 0, null]}, "name": "EXTRA_LOAD"}, {"value": "g.application_code", "name": "APPLICATION_CODE"}, {"value": null, "name": "PRODUCTSEGMENT"}, {"value": {"decode": ["g.application_code", {"literal": "F1"}, {"literal": "FittedUnitCar"}, {"literal": "F2"}, {"literal": "FittedUnitVan"}, {"literal": "F9"}, {"literal": "FittedUnitSuv"}, {"literal": "01"}, {"literal": "Car"}, {"literal": "02"}, {"literal": "Van"}, {"literal": "03"}, {"literal": "Truck"}, {"literal": "04"}, {"literal": "EM"}, {"literal": "05"}, {"literal": "AS"}, {"literal": "06"}, {"literal": "Industry"}, {"literal": "08"}, {"literal": "Moto"}, {"literal": "09"}, {"literal": "SUV"}, null]}, "name": "APPLICATION"}, {"value": {"decode": ["g.SNOWFLAG", {"literal": "Yes"}, 1, {"literal": "No"}, 0, null]}, "name": "SNOWFLAG"}, {"value": {"decode": ["g.RUNFLAT", {"literal": "Yes"}, 1, {"literal": "No"}, 0, null]}, "name": "RUNFLAT"}, {"value": {"decode": [{"trim": "g.NOISE_PERFORMANCE"}, {"literal": ""}, null, {"trim": "G.NOISE_PERFORMANCE"}]}, "name": "NOISE_PERFORMANCE"}, {"value": {"decode": [{"trim": "g.rollres"}, {"literal": ""}, null, {"trim": "G.rollres"}]}, "name": "ROLLRES"}, {"value": {"decode": [{"trim": "g.wetgrip"}, {"literal": ""}, null, {"trim": "G.wetgrip"}]}, "name": "WETGRIP"}, {"value": "g.MANUFACTURER", "name": "MANUFACTURER"}, {"value": {"decode": [{"decode": [{"is_number": "g.season"}, 1, {"to_number": "g.season"}, 0]}, 1, {"literal": "summer"}, 2, {"literal": "winter"}, 10, {"literal": "allseasons"}, null]}, "name": "SEASONALITY"}], "from": [{"value": "DIM_CURRENCY", "name": "k"}, {"value": "P2_PATTERN_ALL", "name": "p"}, {"value": "P2_MATERIAL_ALL", "name": "g"}, {"value": "DW.DIM_MARKET_CHANNEL", "name": "m"}, {"value": "PRLST_DETAIL", "name": "d"}, {"value": {"select": [{"value": "H1.PRICELIST_ID"}, {"value": "H1.BRAND"}, {"value": "H1.BU_COD"}, {"value": "H1.MARKET5"}, {"value": "H1.DATE_FROM"}, {"value": "H1.CURRENCY"}], "from": [{"value": "PRCLST_HEADER", "name": "h1"}, {"value": "LOOKUP_BRAND", "name": "b1"}], "where": {"and": [{"eq": ["H1.ENABLE_VIEWING_B2F", 1]}, {"in": [["H1.BRAND", "H1.BU_COD", "H1.MARKET5", "H1.DATE_FROM"], {"select": [{"value": "H2.BRAND"}, {"value": "H2.BU_COD"}, {"value": "H2.MARKET5"}, {"value": {"max": "H2.DATE_FROM"}}], "from": {"value": "PRCLST_HEADER", "name": "h2"}, "where": {"and": [{"eq": ["H2.BU_COD", {"literal": "CAR"}]}, {"eq": ["H2.ENABLE_VIEWING_B2F", 1]}]}, "groupby": [{"value": "H2.BRAND"}, {"value": "H2.BU_COD"}, {"value": "H2.MARKET5"}]}]}, {"eq": ["H1.BRAND", "B1.BRAND"]}]}}, "name": "h"}], "where": {"and": [{"eq": ["h.currency", "K.CR_COD_CURRENCY_SAP"]}, {"eq": ["h.pricelist_id", "D.PRICELIST_ID"]}, {"eq": ["H.BRAND", "G.BRCONA"]}, {"eq": ["D.IPCODE", "G.MATNR"]}, {"eq": ["P.BRAND", "G.BRCONA"]}, {"eq": [{"upper": "P.PATTERN"}, {"upper": "G.PATTERN"}]}, {"eq": ["h.market5", "m.MARKET_CHANNEL_CODE"]}, {"eq": ["G.IS_USER", 1]}, {"nin": [["G.BRCONA", "G.MATNR"], {"select": [{"value": "C.BRCONA"}, {"value": "C.MATNR"}], "from": {"value": "P2_MAT_USER_CONFLICTS", "name": "c"}, "where": {"in": ["C.LAST_ACTION", 21]}}]}]}, "orderby": {"value": "G.ITEM_ID"}}"""
        )
        self.assertEqual(result, expected)

    def test_issue_38a(self):
        sql = "SELECT a IN ('abc',3,'def')"
        result = parse(sql)
        expected = {"select": {"value": {"in": ["a", {"literal": ["abc", 3, "def"]}]}}}
        self.assertEqual(result, expected)

    def test_issue_38b(self):
        sql = "SELECT a IN (abc,3,'def')"
        result = parse(sql)
        expected = {"select": {"value": {"in": ["a", ["abc", 3, {"literal": "def"}]]}}}
        self.assertEqual(result, expected)

    def test_issue_107_recursion(self):
        sql = (
            " SELECT city_name"
            " FROM city"
            " WHERE population = ("
            "     SELECT MAX(population)"
            "     FROM city"
            "     WHERE state_name IN ("
            "         SELECT state_name"
            "         FROM state"
            "         WHERE area = (SELECT MIN(area) FROM state)"
            "     )"
            " )"
        )
        result = parse(sql)
        expected = {
            "from": "city",
            "select": {"value": "city_name"},
            "where": {"eq": [
                "population",
                {
                    "from": "city",
                    "select": {"value": {"max": "population"}},
                    "where": {"in": [
                        "state_name",
                        {
                            "from": "state",
                            "select": {"value": "state_name"},
                            "where": {"eq": [
                                "area",
                                {"from": "state", "select": {"value": {"min": "area"}}},
                            ]},
                        },
                    ]},
                },
            ]},
        }
        self.assertEqual(result, expected)

    def test_issue_95(self):
        #      0         1         2         3         4         5         6         7         8         9
        #      012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
        sql = "select * from some_table.some_function('parameter', 1, some_col)"
        result = parse(sql)
        expected = {
            "select": "*",
            "from": {"value": {"some_table.some_function": [
                {"literal": "parameter"},
                1,
                "some_col",
            ]}},
        }
        self.assertEqual(result, expected)

    def test_at_ident(self):
        sql = "select @@version_comment"
        result = parse(sql)
        expected = {"select": {"value": "@@version_comment"}}
        self.assertEqual(result, expected)

    def test_date(self):
        sql = "select DATE '2020 01 25'"
        result = parse(sql)
        expected = {"select": {"value": {"date": {"literal": "2020 01 25"}}}}
        self.assertEqual(result, expected)

    def test_interval(self):
        sql = "select INTErval 30.5 monthS"
        result = parse(sql)
        expected = {"select": {"value": {"interval": [30.5, "month"]}}}
        self.assertEqual(result, expected)

    def test_date_less_interval(self):
        sql = "select DATE '2020 01 25' - interval 4 seconds"
        result = parse(sql)
        expected = {"select": {"value": {"sub": [
            {"date": {"literal": "2020 01 25"}},
            {"interval": [4, "second"]},
        ]}}}
        self.assertEqual(result, expected)

    def test_issue_141(self):
        sql = "select name from table order by age limit 1 offset 3"
        result = parse(sql)
        expected = {
            "select": {"value": "name"},
            "from": "table",
            "orderby": {"value": "age"},
            "limit": 1,
            "offset": 3,
        }
        self.assertEqual(result, expected)

    def test_issue_144(self):
        sql = (
            "SELECT count(url) FROM crawl_urls WHERE ((http_status_code = 200 AND"
            " meta_redirect = FALSE AND primary_page = TRUE AND indexable = TRUE AND"
            " canonicalized_page = FALSE AND (paginated_page = FALSE OR (paginated_page"
            " = TRUE AND page_1 = TRUE))) AND ((css <> TRUE AND js <> TRUE AND is_image"
            " <> TRUE AND internal = TRUE) AND (header_content_type = 'text/html' OR"
            " header_content_type = ''))) ORDER BY count(url) DESC"
        )
        result = parse(sql)
        expected = {
            "select": {"value": {"count": "url"}},
            "from": "crawl_urls",
            "where": {"and": [
                {"eq": ["http_status_code", 200]},
                {"eq": ["meta_redirect", False]},
                {"eq": ["primary_page", True]},
                {"eq": ["indexable", True]},
                {"eq": ["canonicalized_page", False]},
                {"or": [
                    {"eq": ["paginated_page", False]},
                    {"and": [
                        {"eq": ["paginated_page", True]},
                        {"eq": ["page_1", True]},
                    ]},
                ]},
                {"neq": ["css", True]},
                {"neq": ["js", True]},
                {"neq": ["is_image", True]},
                {"eq": ["internal", True]},
                {"or": [
                    {"eq": ["header_content_type", {"literal": "text/html"}]},
                    {"eq": ["header_content_type", {"literal": ""}]},
                ]},
            ]},
            "orderby": {"value": {"count": "url"}, "sort": "desc"},
        }
        self.assertEqual(result, expected)

    def test_and_w_tuple(self):
        sql = "SELECT * FROM a WHERE ((a = 1 AND (b=2 AND c=3, False)))"
        result = parse(sql)
        expected = {
            "select": "*",
            "from": "a",
            "where": {"and": [
                {"eq": ["a", 1]},
                [{"and": [{"eq": ["b", 2]}, {"eq": ["c", 3]}]}, False],
            ]},
        }
        self.assertEqual(result, expected)

    def test_null_parameter(self):
        sql = "select DECODE(A, NULL, 'b')"
        result = parse(sql)
        expected = {"select": {"value": {"decode": ["A", Null, {"literal": "b"}]}}}
        self.assertEqual(result, expected)

    def test_issue143a(self):
        sql = "Select [A] from dual"
        result = parse(sql)
        expected = {"select": {"value": "A"}, "from": "dual"}
        self.assertEqual(result, expected)

    def test_issue143b(self):
        sql = "Select [A] from [dual]"
        result = parse(sql)
        expected = {"select": {"value": "A"}, "from": "dual"}
        self.assertEqual(result, expected)

    def test_issue143c(self):
        sql = "Select [A] from dual [T1]"
        result = parse(sql)
        expected = {"select": {"value": "A"}, "from": {"value": "dual", "name": "T1"}}
        self.assertEqual(result, expected)

    def test_issue143d_quote(self):
        sql = 'Select ["]'
        result = parse(sql)
        expected = {"select": {"value": '"'}}
        self.assertEqual(result, expected)

    def test_issue143e_close(self):
        sql = "Select []]]"
        result = parse(sql)
        expected = {"select": {"value": "]"}}
        self.assertEqual(result, expected)

    def test_issue140(self):
        sql = "select rank(*) over (partition by a order by b, c) from tab"
        result = parse(sql)
        expected = {
            "select": {
                "value": {"rank": "*"},
                "over": {"partitionby": "a", "orderby": ["b", "c"]},
            },
            "from": "tab",
        }
        self.assertEqual(result, expected)

    def test_issue119(self):
        sql = "SELECT 1 + CAST(1 AS INT) result"
        result = parse(sql)
        expected = {"select": {
            "value": {"add": [1, {"cast": [1, "int"]}]},
            "name": "result",
        }}
        self.assertEqual(result, expected)

    def test_issue120(self):
        sql = "SELECT DISTINCT Country, City FROM Customers"
        result = parse(sql)
        expected = {
            "select_distinct": [{"value": "Country"}, {"value": "City"}],
            "from": "Customers",
        }
        self.assertEqual(result, expected)


