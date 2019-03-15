# Moz SQL Parser Tests

The test suite has over 160 tests, all passing. 

## Running Tests

For __Linux__:

	git clone https://github.com/mozilla/moz-sql-parser.git
	cd moz-sql-parser
	pip install -r requirements.txt
	set PYTHONPATH=.	
	python -m unittest discover tests

 For __Windows__:

	git clone https://github.com/mozilla/moz-sql-parser.git
	cd moz-sql-parser
	pip install -r requirements.txt
	set PYTHONPATH=.	
	python.exe -m unittest discover tests

### Debugging Suggestions

* Once you have written a failing test, you can `DEBUG=True` in the 
`sql_parser.py` to print out a trace of matching attempts. 
* If you added more `ParserElement`s, you may want to add `.setDebugActions(*debug)` 
to each, so they print out thier matching attempts too.
* Even though you can use Python strings for literals, they will not be
attempted in all cases; wrap as a `Literal` or `Keyword`.  This problem 
is known to lurk while matching `infixNotation`. 
* Feel free to leave in rulers, if you use them

 
## History

* **January 2018** - fixes for Python3
* **July 2017** - Add suite to start testing error messages, with hope of improving them
* **April 2017** - All tests pass
* **March 2017** - Added a number of test cases, most of which are missing the expected JSON parse tree, so they fail.


