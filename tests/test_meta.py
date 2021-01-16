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
import sys
from unittest import TestCase
from moz_sql_parser import sql_parser

_ensure_imported = sql_parser


class TestSimple(TestCase):
    """
    THESE TESTS ARE FOR VERIFYING THE STATE OF THE REPO, NOT HTE STATE OF THE CODE
    """

    def test_recursion_limit(self):
        if os.environ.get("TRAVIS_BRANCH") == "master":
            limit = sys.getrecursionlimit()
            self.assertLess(limit, 1500)

