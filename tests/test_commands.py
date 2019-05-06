# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Author: Kyle Lahnakoski (kyle@lahnakoski.com)
#

from __future__ import absolute_import, division, unicode_literals

from unittest import TestCase, skip

from moz_sql_parser import parse, sql_parser
from tests.util import assertRaises


class TestSimple(TestCase):

    def test_create_table_one_column(self):
        result = parse("create table student (name varchar2)")
        expected = {'create table': [{'name': 'student'}, {'columns': {'name': 'name', 'type': 'varchar2'}}]}
        self.assertEqual(result, expected)

    def test_create_table_two_columns(self):
        result = parse("create table student (name varchar2, rollno int)")
        expected = {'create table': [{'name': 'student'}, {'columns': [{'name': 'name', 'type': 'varchar2'}, {'name': 'rollno', 'type': 'int'}]}]}
        self.assertEqual(result, expected)

    def test_create_table_three_columns(self):
        result = parse("create table customers (id name, name varchar, salary decimal )")
        expected = {'create table': [{'name': 'customers'}, {'columns': [{'name': 'id', 'type': 'name'}, {'name': 'name', 'type': 'varchar'}, {'name': 'salary', 'type': 'decimal'}]}]}
        self.assertEqual(result, expected)

    def test_create_table_four_columns(self):
        result = parse("create table customers( id int, name varchar, address char, salary decimal)")
        expected = {'create table': [{'name': 'customers'}, {'columns': [{'name': 'id', 'type': 'int'}, {'name': 'name', 'type': 'varchar'}, {'name': 'address', 'type': 'char'}, {'name': 'salary', 'type': 'decimal'}]}]}
        self.assertEqual(result, expected)

    def test_create_table_five_columns(self):
        result = parse("create table persons ( PersonID int, LastName varchar, FirstName varchar, Address varchar, City varchar)")
        expected = {'create table': [{'name': 'persons'}, {'columns': [{'name': 'personid', 'type': 'int'}, {'name': 'lastname', 'type': 'varchar'}, {'name': 'firstname', 'type': 'varchar'}, {'name': 'address', 'type': 'varchar'}, {'name': 'city', 'type': 'varchar'}]}]}
        self.assertEqual(result, expected)

    def test_create_table_two_columns_with_size(self):
        result = parse("create table student (name varchar2(25), rollno int(2))")
        expected = {'create table': [{'name': 'student'}, {'columns': [{'name': 'name', 'type': ['varchar2', 25]}, {'name': 'rollno', 'type': ['int', 2]}]}]}
        self.assertEqual(result, expected)

    def test_create_table_five_columns_with_size(self):
        result = parse("create table persons ( PersonID int(2), LastName varchar(10), FirstName varchar(10), Address varchar(50), City varchar(10))")
        expected = {'create table': [{'name': 'persons'}, {'columns': [{'name': 'personid', 'type': ['int', 2]}, {'name': 'lastname', 'type': ['varchar', 10]}, {'name': 'firstname', 'type': ['varchar', 10]}, {'name': 'address', 'type': ['varchar', 50]}, {'name': 'city', 'type': ['varchar', 10]}]}]}
        self.assertEqual(result, expected)

    def test_create_table_one_columns_with_size(self):
        result = parse("create table student (name varchar not null)")
        expected = {'create table': [{'name': 'student'}, {'columns': {'name': 'name', 'type': 'varchar', 'option': 'not null'}}]}
        self.assertEqual(result, expected)

    def test_create_table_two_columns_with_option(self):
        result = parse("create table student (name varchar not null, sunny int primary key)")
        expected = {"create table": [{"name": "student"}, {"columns": [{"name": "name", "type": "varchar", "option": "not null"}, {"name": "sunny", "type": "int", "option": "primary key"}]}]}
        self.assertEqual(result, expected)
