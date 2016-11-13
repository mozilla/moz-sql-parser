# moz-sql-parser

Let's make a SQL parser so we can provide a familiar interface to non-sql datastores!

## Problem Statement

SQL is a familiar language used to access databases. Although, each database vendor has it's quirky implementation, the average developer does not know enough SQL to be concerned with those quirks. This familiar core SQL (lowest common denominator, if you will) is useful enough to explore data in a primitive ways. It is hoped that, once programmers have reviewed the datastore with basic SQL queries, and see the value of that data, they will be motivated to use the datastore's native query format.

## Objectives

The primary objective of this library is to convert some subset of [SQL-92](https://en.wikipedia.org/wiki/SQL-92) queries to JSON-izable parse trees. A big enough subset to provide superficial data access via SQL, but not so much as we must deal with the document-relational impedance mismatch.

## Non-Objectives 

* No plans to provide update statements, like `update` or `insert`
* No plans to expand the language to all of SQL:2011
* No plans to provide data access tools 


## Project Status

This is an early-version SQL parser: If your SQL does not work, at the very least, [please cut-and-paste it to a new issue](https://github.com/mozilla/moz-sql-parser/issues/new)


## Install

	pip install moz-sql-parser

## Usage

	>>> from moz_sql_parser import parse
	>>> import json
	>>> json.dumps(parse("select count(1) from jobs"))
	'{"from": "jobs", "select": {"value": {"count": {"literal": 1}}}}'
	
Each SQL query is parsed to an object: Each clause is assigned to an object property of the same name. 

	>>> json.dumps(parse("select a as hello, b as world from jobs"))
	'{"from": "jobs", "select": [{"name": "hello", "value": "a"}, {"name": "world", "value": "b"}]}'

The `SELECT` clause is an array of objects containing `name` and `value` properties. 

## Run Tests

	git clone https://github.com/mozilla/moz-sql-parser.git
	pip install -r requirements.txt
	set PYTHONPATH=.	
	python.exe -m unittest tests.test_simple

## About

SQL queries are translated to JSON objects: Each clause is assigned to an object property of the same name.  Expressions are also objects, but with only one property having the name of the operation, and the value holding (an array of) parameters for that operation. 

### Notes

* Uses the glorious `pyparsing` library (see http://pyparsing.wikispaces.com/) to define the grammar, and define the shape of the tokens it generates. 
* `[sqlparse](https://pypi.python.org/pypi/sqlparse)` Does not provide a tree, rather a list of tokens. 
