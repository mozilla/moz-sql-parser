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
        result = parse("create table student ( name varchar2)")
        expected = {'create table': [{'name': 'student'}, {'columns': {'name': 'name', 'type': 'varchar2'}}]}
        self.assertEqual(result, expected)

    def test_create_table_two_column(self):
        result = parse("create table student ( name varchar)")
        expected = {'create table': [{'name': 'student'}, {'columns': {'name': 'name', 'type': 'varchar2'} }]}
        self.assertEqual(result, expected)

