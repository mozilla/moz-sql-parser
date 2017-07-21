# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Author: Kyle Lahnakoski (kyle@lahnakoski.com)
#
from distutils.util import convert_path
import os
from setuptools import setup


root = os.path.abspath(os.path.dirname(__file__))
path = lambda *p: os.path.join(root, *p)
try:
    long_desc = open(path('README.txt')).read()
except Exception:
    long_desc = "<Missing README.txt>"
    print("Missing README.txt")


def find_packages(where='.', lib_prefix='', exclude=()):
    """
    SNAGGED FROM distribute-0.6.49-py2.7.egg/setuptools/__init__.py
    """
    out = []
    stack=[(convert_path(where), lib_prefix)]
    while stack:
        where,prefix = stack.pop(0)
        for name in os.listdir(where):
            fn = os.path.join(where,name)
            if ('.' not in name and os.path.isdir(fn) and
                os.path.isfile(os.path.join(fn,'__init__.py'))
            ):
                out.append(prefix+name); stack.append((fn,prefix+name+'.'))
    for pat in list(exclude)+['ez_setup', 'distribute_setup']:
        from fnmatch import fnmatchcase
        out = [item for item in out if not fnmatchcase(item,pat)]
    return out


setup(
    name='moz-sql-parser',
    version="0.1.17202",
    description='Extract Parse Tree from SQL',
    long_description=long_desc,
    author='Kyle Lahnakoski',
    author_email='kyle@lahnakoski.com',
    url='https://github.com/mozilla/moz-sql-parser',
    license='MPL 2.0',
    packages=find_packages(".", lib_prefix=""),
    install_requires=["pyparsing"],
    include_package_data=True,
    zip_safe=False,
    classifiers=[  #https://pypi.python.org/pypi?%3Aaction=list_classifiers
        "Development Status :: 3 - Alpha",
        "Topic :: Software Development :: Libraries",
        "Topic :: Software Development :: Libraries :: Python Modules",
        "Programming Language :: SQL",
        "Programming Language :: Python :: 2.7",
        "License :: OSI Approved :: Mozilla Public License 2.0 (MPL 2.0)",
    ]
)