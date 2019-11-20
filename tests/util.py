# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#

from __future__ import absolute_import, division, unicode_literals

from mo_future import text


def assertRaises(expected_text_in_error, method):
    try:
        method()
        raise Exception("expecting an exception")
    except Exception as e:
        text_error = text(e)
        if not isinstance(expected_text_in_error, (list, tuple, set)):
            expected_text_in_error = [expected_text_in_error]

        if any(e not in text_error for e in expected_text_in_error):
            raise Exception("wrong error raised")
