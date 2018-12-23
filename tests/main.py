"""
Batch parsing a dataset.
"""
import collections
import json
import os
import sys

from moz_sql_parser import parse
from moz_sql_parser import format
from tests.test_simple import TestSimple
from tests.test_resources import TestResources
from tests.test_format_and_parse import TestFormatAndParse

from pyparsing import ParseException


data_dir = 'examples/'


# tests
def test_formatter():
    with open(os.path.join(data_dir, 'ast-0.json')) as f:
        ast = json.load(f)
    print(format(ast))
    print()


def test_parser():
    in_sql = sys.argv[1]
    with open(in_sql) as f:
        sql_query = f.read()
    sql_pt = parse(sql_query)
    print(json.dumps(sql_pt, indent=4))
    print()
    new_sql_query = format(sql_pt)
    print(sql_query.replace('\n', ' '))
    print()
    print(new_sql_query.replace('\n', ' '))
    print()


def parse_dataset():
    data_dir = sys.argv[1]
    dataset = sys.argv[2]     

    in_json = os.path.join(data_dir, '{}.json'.format(dataset))
    with open(in_json) as f:
        data = json.loads(f.read())
        for example in data:
            for sql_query in example['sql']:
                if sql_query.endswith(';'):
                    sql_query = sql_query[:-1]
                    print(sql_query)
                    print()
                    sql_query_pt = parse(sql_query)
                    print(json.dumps(sql_query_pt, indent=4))
                    print()
                    import pdb
                    pdb.set_trace()


def run_tests():
    ts = TestSimple()
    # ts.test_not_in_expression()
    ts.test_joined_table_name()
    tfp = TestFormatAndParse()
    tfp.test_086()
    tfp.test_087()
    tfp.test_088()
    tfp.test_089()
    tfp.test_joined_table_name()
    # ts.test_not_like_in_where()
    # ts.test_not_like_in_select()
    # tr = TestResources()
    # tr.test_098()


if __name__ == '__main__':
    # test_parser()
    # test_formatter()
    # test_column_analyzer()
    # test_denormalizer()
    # parse_dataset()
    # run()
    run_tests()

