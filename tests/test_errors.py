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


class TestErrors(FuzzyTestCase):

    def test_dash_in_tablename(self):
        try:
            result = parse("select * from coverage-summary.source.file.covered limit 20")
            self.assertTrue(False, "expecting to fail")
        except Exception as e:
            self.assertTrue(
                all(v in str(e) for v in ["group by", "order by", "having", "limit", "where"]),
                "expecting mention of other expected clauses"
            )

