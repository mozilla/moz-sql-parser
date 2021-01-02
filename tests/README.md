# Moz SQL Parser Tests

The test suite has over 400 tests.

## Running Tests

For __Linux__:

	git clone https://github.com/klahnakoski/moz-sql-parser.git
	cd moz-sql-parser
	pip install -r requirements.txt
	pip install -r tests/requirements.txt
	export PYTHONPATH=.	
	python -m unittest discover tests

 For __Windows__:

	git clone https://github.com/klahnakoski/moz-sql-parser.git
	cd moz-sql-parser
	pip install -r requirements.txt
	pip install -r tests\requirements.txt
	set PYTHONPATH=.	
	python.exe -m unittest discover tests

### Debugging Suggestions

Once you have written a failing test, you can use `with Debugger():` in your test to print out a trace of matching attempts. 
 
## History

* **November 2020** - Add `pip install` tests
* **October 2020** - Use mo-parsing for less recursion and faster parsing 
* **January 2018** - fixes for Python3
* **July 2017** - Add suite to start testing error messages, with hope of improving them
* **April 2017** - All tests pass
* **March 2017** - Added a number of test cases, most of which are missing the expected JSON parse tree, so they fail.


