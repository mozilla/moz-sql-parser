# encoding: utf-8
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Author: Kyle Lahnakoski (kyle@lahnakoski.com)
#

from __future__ import absolute_import, division, unicode_literals

from unittest import TestCase, skip

from mo_parsing.debug import Debugger
from moz_sql_parser import parse


class TestSqlServer(TestCase):
    def test_issue13_top(self):
        # https://docs.microsoft.com/en-us/sql/t-sql/queries/top-transact-sql?view=sql-server-ver15
        sql = "SELECT TOP 3 * FROM Customers"
        sql = "SELECT TOP func(value) WITH TIES *"
        sql = "SELECT TOP 1 PERCENT WITH TIES *"
