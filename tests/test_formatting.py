# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Author: Beto Dealmeida (beto@dealmeida.net)
#

from __future__ import absolute_import, division, unicode_literals

from unittest import TestCase

from moz_sql_parser import format


class TestSimple(TestCase):
    def test_two_tables(self):
        result = format({"select": "*", "from": ["XYZZY", "ABC"]})
        expected = "SELECT * FROM XYZZY, ABC"
        self.assertEqual(result, expected)

    def test_dot_table_name(self):
        result = format({"select": "*", "from": "SYS.XYZZY",})
        expected = "SELECT * FROM SYS.XYZZY"
        self.assertEqual(result, expected)

    def select_one_column(self):
        result = format({"select": [{"value": "A"}], "from": ["dual"],})
        expected = "SELECT A FROM dual"
        self.assertEqual(result, expected)

    def test_select_quote(self):
        result = format({"select": {"value": {"literal": "'"}}, "from": "dual",})
        expected = "SELECT '''' FROM dual"
        self.assertEqual(result, expected)

    def test_select_quoted_name(self):
        result = format({
            "select": [
                {"name": "@*#&", "value": "a"},
                {"name": "test.g.g.c", "value": "b"},
            ],
            "from": "dual",
        })
        expected = 'SELECT a AS "@*#&", b AS test.g.g.c FROM dual'
        self.assertEqual(result, expected)

    def test_select_expression(self):
        result = format({
            "select": {"value": {"add": [
                "a",
                {"div": ["b", 2]},
                {"mul": [45, "c"]},
                {"div": [2, "d"]},
            ]}},
            "from": "dual",
        })
        expected = "SELECT a + b / 2 + 45 * c + 2 / d FROM dual"
        self.assertEqual(result, expected)

    def test_select_underscore_name(self):
        result = format({"select": {"value": "_id"}, "from": "dual",})
        expected = "SELECT _id FROM dual"
        self.assertEqual(result, expected)

    def test_select_dots_names(self):
        result = format({"select": {"value": "a.b.c._d"}, "from": "dual",})
        expected = "SELECT a.b.c._d FROM dual"
        self.assertEqual(result, expected)

    def select_many_column(self):
        result = format({
            "select": [{"value": "a"}, {"value": "b"}, {"value": "c"},],
            "from": ["dual"],
        })
        expected = "SELECT a, b, c FROM dual"
        self.assertEqual(result, expected)

    def test_where_neq(self):
        result = format({
            "select": "*",
            "from": "dual",
            "where": {"neq": ["a", {"literal": "test"}]},
        })
        expected = "SELECT * FROM dual WHERE a <> 'test'"
        self.assertEqual(result, expected)

    def test_where_in(self):
        result = format({
            "select": {"value": "a"},
            "from": "dual",
            "where": {"in": ["a", {"literal": ["r", "g", "b"]},]},
        })
        expected = "SELECT a FROM dual WHERE a IN ('r', 'g', 'b')"
        self.assertEqual(result, expected)

    def test_where_in_and_in(self):
        result = format({
            "select": {"value": "a"},
            "from": "dual",
            "where": {"and": [
                {"in": ["a", {"literal": ["r", "g", "b"]},]},
                {"in": ["b", [10, 11, 12],]},
            ]},
        })
        expected = "SELECT a FROM dual WHERE a IN ('r', 'g', 'b') AND b IN (10, 11, 12)"
        self.assertEqual(result, expected)

    def test_eq(self):
        result = format({
            "select": [{"value": "a"}, {"value": "b"},],
            "from": ["t1", "t2"],
            "where": {"eq": ["t1.a", "t2.b"]},
        })
        expected = "SELECT a, b FROM t1, t2 WHERE t1.a = t2.b"
        self.assertEqual(result, expected)

    def test_is_null(self):
        result = format({
            "select": [{"value": "a"}, {"value": "b"},],
            "from": "t1",
            "where": {"missing": "t1.a"},
        })
        expected = "SELECT a, b FROM t1 WHERE t1.a IS NULL"
        self.assertEqual(result, expected)

    def test_is_not_null(self):
        result = format({
            "select": [{"value": "a"}, {"value": "b"},],
            "from": "t1",
            "where": {"exists": "t1.a"},
        })
        expected = "SELECT a, b FROM t1 WHERE t1.a IS NOT NULL"
        self.assertEqual(result, expected)

    def test_groupby(self):
        result = format({
            "select": [{"value": "a"}, {"name": "b", "value": {"count": 1}},],
            "from": "mytable",
            "groupby": {"value": "a"},
        })
        expected = "SELECT a, COUNT(1) AS b FROM mytable GROUP BY a"
        self.assertEqual(result, expected)

    def test_function(self):
        result = format({"select": {"value": {"count": 1}}, "from": "mytable",})
        expected = "SELECT COUNT(1) FROM mytable"
        self.assertEqual(result, expected)

    def test_order_by(self):
        result = format({
            "select": {"value": {"count": 1}},
            "from": "dual",
            "orderby": {"value": "a"},
        })
        expected = "SELECT COUNT(1) FROM dual ORDER BY a"
        self.assertEqual(result, expected)

    def test_order_by_asc(self):
        result = format({
            "select": {"value": {"count": 1}},
            "from": "dual",
            "orderby": {"value": "a", "sort": "asc"},
        })
        expected = "SELECT COUNT(1) FROM dual ORDER BY a ASC"
        self.assertEqual(result, expected)

    def test_order_by_desc(self):
        result = format({
            "select": {"value": {"count": 1}},
            "from": "dual",
            "orderby": {"value": "a", "sort": "desc"},
        })
        expected = "SELECT COUNT(1) FROM dual ORDER BY a DESC"
        self.assertEqual(result, expected)

    def test_neg_or_precedence(self):
        result = format({
            "from": "table1",
            "where": {"or": [{"eq": ["A", -900]}, {"eq": ["B", 100]}]},
            "select": [{"value": "B"}, {"value": "C"}],
        })
        expected = "SELECT B, C FROM table1 WHERE A = -900 OR B = 100"
        self.assertEqual(result, expected)

    def test_negative_number(self):
        result = format({
            "from": "table1",
            "where": {"eq": ["A", -900]},
            "select": {"value": "a"},
        })
        expected = "SELECT a FROM table1 WHERE A = -900"
        self.assertEqual(result, expected)

    def test_like_in_where(self):
        result = format({
            "from": "table1",
            "where": {"like": ["A", {"literal": "%20%"}]},
            "select": {"value": "a"},
        })
        expected = "SELECT a FROM table1 WHERE A LIKE '%20%'"
        self.assertEqual(result, expected)

    def test_like_in_select(self):
        result = format({
            "from": "table1",
            "select": {
                "name": "bb",
                "value": {"case": [
                    {"when": {"like": ["A", {"literal": "bb%"}]}, "then": 1},
                    0,
                ]},
            },
        })
        expected = "SELECT CASE WHEN A LIKE 'bb%' THEN 1 ELSE 0 END AS bb FROM table1"
        self.assertEqual(result, expected)

    def test_like_from_pr16(self):
        result = format({
            "from": "trade",
            "where": {"and": [
                {"like": ["school", {"literal": "%shool"}]},
                {"eq": ["name", {"literal": "abc"}]},
                {"in": ["id", {"literal": ["1", "2"]}]},
            ]},
            "select": "*",
        })
        expected = (
            "SELECT * FROM trade WHERE school LIKE '%shool' AND name = 'abc' AND id IN"
            " ('1', '2')"
        )
        self.assertEqual(result, expected)

    def test_rlike_in_where(self):
        result = format({
            "from": "table1",
            "where": {"rlike": ["A", {"literal": ".*20.*"}]},
            "select": {"value": "a"},
        })
        expected = "SELECT a FROM table1 WHERE A RLIKE '.*20.*'"
        self.assertEqual(result, expected)

    def test_rlike_in_select(self):
        result = format({
            "from": "table1",
            "select": {
                "name": "bb",
                "value": {"case": [
                    {"when": {"rlike": ["A", {"literal": "bb.*"}]}, "then": 1},
                    0,
                ]},
            },
        })
        expected = "SELECT CASE WHEN A RLIKE 'bb.*' THEN 1 ELSE 0 END AS bb FROM table1"
        self.assertEqual(result, expected)

    def test_in_expression(self):
        result = format({
            "from": "task",
            "select": "*",
            "where": {"in": [
                "repo.branch.name",
                {"literal": ["try", "mozilla-central"]},
            ]},
        })
        expected = (
            "SELECT * FROM task WHERE repo.branch.name IN ('try', 'mozilla-central')"
        )
        self.assertEqual(result, expected)

    def test_joined_table_name(self):
        result = format({
            "from": [
                {"name": "t1", "value": "table1"},
                {
                    "on": {"eq": ["t1.id", "t3.id"]},
                    "join": {"name": "t3", "value": "table3"},
                },
            ],
            "select": "*",
        })
        expected = "SELECT * FROM table1 AS t1 JOIN table3 AS t3 ON t1.id = t3.id"
        self.assertEqual(result, expected)

    def test_not_equal(self):
        result = format({
            "select": "*",
            "from": "task",
            "where": {"and": [
                {"exists": "build.product"},
                {"neq": ["build.product", {"literal": "firefox"}]},
            ]},
        })
        expected = (
            "SELECT * FROM task WHERE build.product IS NOT NULL AND build.product <>"
            " 'firefox'"
        )
        self.assertEqual(result, expected)

    def test_union(self):
        result = format({
            "union": [{"select": "*", "from": "a"}, {"select": "*", "from": "b"},],
        })
        expected = "SELECT * FROM a UNION SELECT * FROM b"
        self.assertEqual(result, expected)

    def test_limit(self):
        result = format({"select": "*", "from": "a", "limit": 10})
        expected = "SELECT * FROM a LIMIT 10"
        self.assertEqual(result, expected)

    def test_offset(self):
        result = format({"select": "*", "from": "a", "limit": 10, "offset": 10})
        expected = "SELECT * FROM a LIMIT 10 OFFSET 10"
        self.assertEqual(result, expected)

    def test_count_literal(self):
        result = format({
            "select": {"value": {"count": {"literal": "literal"}}},
            "from": "a",
        })
        expected = "SELECT COUNT('literal') FROM a"
        self.assertEqual(result, expected)

    def test_no_arguments(self):
        result = format({"select": {"value": {"now": {}}}})
        expected = "SELECT NOW()"
        self.assertEqual(result, expected)

    def test_between(self):
        result = format({
            "select": [{"value": "a"}],
            "from": ["t1"],
            "where": {"between": ["t1.a", 10, {"literal": "ABC"}]},
        })
        expected = "SELECT a FROM t1 WHERE t1.a BETWEEN 10 AND 'ABC'"
        self.assertEqual(result, expected)

    def test_binary_and(self):
        expected = "SELECT * FROM t WHERE c & 4"
        result = format({"select": "*", "from": "t", "where": {"binary_and": ["c", 4]}})
        self.assertEqual(result, expected)

    def test_binary_or(self):
        expected = "SELECT * FROM t WHERE c | 4"
        result = format({"select": "*", "from": "t", "where": {"binary_or": ["c", 4]}})
        self.assertEqual(result, expected)

    def test_binary_not(self):
        expected = "SELECT * FROM t WHERE ~c"
        result = format({"select": "*", "from": "t", "where": {"binary_not": "c"}})
        self.assertEqual(result, expected)

    def test_issue_104(self):
        expected = (
            'SELECT NomPropriete AS Categorie, ROUND(AVG(NotePonderee), 2) AS "Moyenne'
            ' des notes", ROUND(AVG(Complexite), 2) AS "Complexite moyenne" FROM'
            " Propriete, Categorie, Jeu WHERE IdPropriete = IdCategorie AND"
            " Categorie.IdJeu = Jeu.IdJeu AND NotePonderee > 0 GROUP BY IdPropriete,"
            ' NomPropriete ORDER BY "Moyenne des notes" DESC,"Complexite moyenne" DESC'
        )
        result = format({
            "select": [
                {"value": "NomPropriete", "name": "Categorie"},
                {
                    "value": {"round": [{"avg": "NotePonderee"}, 2]},
                    "name": "Moyenne des notes",
                },
                {
                    "value": {"round": [{"avg": "Complexite"}, 2]},
                    "name": "Complexite moyenne",
                },
            ],
            "from": ["Propriete", "Categorie", "Jeu"],
            "where": {"and": [
                {"eq": ["IdPropriete", "IdCategorie"]},
                {"eq": ["Categorie.IdJeu", "Jeu.IdJeu"]},
                {"gt": ["NotePonderee", 0]},
            ]},
            "groupby": [{"value": "IdPropriete"}, {"value": "NomPropriete"}],
            "orderby": [
                {"value": "Moyenne des notes", "sort": "desc"},
                {"value": "Complexite moyenne", "sort": "desc"},
            ],
        })
        self.assertEqual(result, expected)

    def test_with_cte(self):
        expected = "WITH t AS (SELECT a FROM table) SELECT * FROM t"
        result = format({
            "select": "*",
            "from": "t",
            "with": {"name": "t", "value": {"select": {"value": "a"}, "from": "table"}},
        })
        self.assertEqual(result, expected)

    def test_with_cte_various(self):
        expected = (
            "WITH t1 AS (SELECT a FROM table), t2 AS (SELECT 1) SELECT * FROM t1, t2"
        )
        result = format({
            "select": "*",
            "from": ["t1", "t2"],
            "with": [
                {"name": "t1", "value": {"select": {"value": "a"}, "from": "table"}},
                {"name": "t2", "value": {"select": {"value": 1}}},
            ],
        })
        self.assertEqual(result, expected)
