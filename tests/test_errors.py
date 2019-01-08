# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Author: Kyle Lahnakoski (kyle@lahnakoski.com)
#

from __future__ import absolute_import, division, unicode_literals

from unittest import TestCase

from moz_sql_parser import parse
from tests.util import assertRaises


class TestErrors(TestCase):

    def test_dash_in_tablename(self):
        assertRaises(
            ["group by", "order by", "having", "limit", "where"],
            lambda: parse("select * from coverage-summary.source.file.covered limit 20")
        )
