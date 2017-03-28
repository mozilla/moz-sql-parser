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

from mo_files import File
from mo_logs import strings, Log

# directory = File("C:/Users/kyle/code/sqlite-parser/src/test/resources")
#
#
# for f in directory.children:
#     if not f.name.startswith("select"):
#         continue
#
#     parts = f.name.split("_")
#     parts[-1] = strings.right("000"+parts[-1], 3)
#     filename = "_".join(parts)+".txt"
#     destination = File.new_instance("tests/resources", filename)
#     acc = []
#     for line in f.read_lines():
#         if line.strip().startswith("--"):
#             continue
#         acc.append(line)
#     acc.append("--")
#     destination.write("\n".join(acc)+"\n")
from mo_logs.strings import expand_template

template="""
    def {{test_name}}(self):
        sql={{sql|quote}}
        result=parse(sql)
        expected = {{json|trim|indent(2)}}
        self.assertEqual(result, expected)

"""

acc=[]
for i, f in enumerate(File("tests/resources").children):
    if f.name == "README":
        continue
    content = f.read()
    sql, json = content.split("--\n")
    acc.append(expand_template(template, {
        "test_name":"test_"+unicode(i),
        "sql":sql,
        "json":json
    }))

Log.note("{{code}}", code="".join(acc))



