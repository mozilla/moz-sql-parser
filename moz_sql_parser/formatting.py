# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Author: Beto Dealmeida (beto@dealmeida.net)
#

from __future__ import absolute_import
from __future__ import division
from __future__ import unicode_literals

import re

from six import string_types, text_type

from moz_sql_parser.sql_parser import RESERVED


VALID = re.compile(r'[a-zA-Z_]\w*')


def should_quote(identifier):
    """
    Return true if a given identifier should be quoted.

    This is usually true when the identifier:

      - is a reserved word
      - contain spaces
      - does not match the regex `[a-zA-Z_]\w*`

    """
    return (
        identifier != '*' and (
            not VALID.match(identifier) or identifier in RESERVED))


def escape(identifier, ansi_quotes, should_quote):
    """
    Escape identifiers.

    ANSI uses single quotes, but many databases use back quotes.

    """
    if not should_quote(identifier):
        return identifier

    quote = '"' if ansi_quotes else '`'
    identifier = identifier.replace(quote, 2*quote)
    return '{0}{1}{2}'.format(quote, identifier, quote)


def Operator(op, parentheses=False):
    op = ' {0} '.format(op)
    def func(self, json):
        out = op.join(self.dispatch(v) for v in json)
        if parentheses:
            out = '({0})'.format(out)
        return out
    return func


class Formatter:

    clauses = [
        'select',
        'from_',
        'where',
        'groupby',
        'having',
        'orderby',
        'limit',
        'offset',
    ]

    # simple operators
    _concat = Operator('||')
    _mult = Operator('*')
    _div = Operator('/', parentheses=True)
    _add = Operator('+')
    _sub = Operator('-', parentheses=True)
    _neq = Operator('<>')
    _gt = Operator('>')
    _lt = Operator('<')
    _gte = Operator('>=')
    _lte = Operator('<=')
    _eq = Operator('=')
    _or = Operator('OR')
    _and = Operator('AND')

    def __init__(self, ansi_quotes=True, should_quote=should_quote):
        self.ansi_quotes = ansi_quotes
        self.should_quote = should_quote

    def format(self, json):
        if 'union' in json:
            return self.union(json['union'])
        else:
            return self.query(json)

    def dispatch(self, json):
        if isinstance(json, list):
            return self.delimited_list(json)
        if isinstance(json, dict):
            if 'value' in json:
                return self.value(json)
            else:
                return self.op(json)
        if isinstance(json, string_types):
            return escape(json, self.ansi_quotes, self.should_quote)
        
        return text_type(json)

    def delimited_list(self, json):
        return ', '.join(self.dispatch(element) for element in json)

    def value(self, json):
        parts = [self.dispatch(json['value'])]
        if 'name' in json:
            parts.extend(['AS', self.dispatch(json['name'])])
        return ' '.join(parts)

    def op(self, json):
        if 'on' in json:
            return self._on(json)

        if len(json) > 1:
            raise Exception('Operators should have only one key!')
        key, value = list(json.items())[0]

        # check if the attribute exists, and call the corresponding method;
        # note that we disallow keys that start with `_` to avoid giving access
        # to magic methods
        attr = '_{0}'.format(key)
        if hasattr(self, attr) and not key.startswith('_'):
            method = getattr(self, attr)
            return method(value)

        return '{0}({1})'.format(key.upper(), value)

    def _exists(self, value):
        return '{0} IS NOT NULL'.format(self.dispatch(value))

    def _missing(self, value):
        return '{0} IS NULL'.format(self.dispatch(value))

    def _like(self, pair):
        return '{0} LIKE {1}'.format(self.dispatch(pair[0]), self.dispatch(pair[1]))

    def _is(self, pair):
        return '{0} IS {1}'.format(self.dispatch(pair[0]), self.dispatch(pair[1]))

    def _in(self, json):
        valid = self.dispatch(json[1])
        # `(10, 11, 12)` does not get parsed as literal, so it's formatted as
        # `10, 11, 12`. This fixes it.
        if not valid.startswith('('):
            valid = '({0})'.format(valid)

        return '{0} IN {1}'.format(json[0], valid)

    def _case(self, checks):
        parts = ['CASE']
        for check in checks:
            if isinstance(check, dict):
                parts.extend(['WHEN', self.dispatch(check['when'])])
                parts.extend(['THEN', self.dispatch(check['then'])])
            else:
                parts.extend(['ELSE', self.dispatch(check)])
        parts.append('END')
        return ' '.join(parts)

    def _literal(self, json):
        if isinstance(json, list):
            return '({0})'.format(', '.join(self._literal(v) for v in json))
        elif isinstance(json, string_types):
            return "'{0}'".format(json.replace("'", "''"))
        else:
            return str(json)

    def _on(self, json):
        return 'JOIN {0} ON {1}'.format(
            self.dispatch(json['join']), self.dispatch(json['on']))

    def union(self, json):
        return ' UNION '.join(self.query(query) for query in json)

    def query(self, json):
        parts = []
        for clause in self.clauses:
            method = getattr(self, clause, None)
            if method:
                parts.append(method(json))
        return ' '.join(part for part in parts if part)

    def select(self, json):
        if 'select' in json:
            return 'SELECT {0}'.format(self.dispatch(json['select']))

    def from_(self, json):
        is_join = False
        if 'from' in json:
            from_ = json['from']
            if not isinstance(from_, list):
                from_ = [from_]

            parts = []
            for token in from_:
                if 'join' in token:
                    is_join = True
                parts.append(self.dispatch(token))
            joiner = ' ' if is_join else ', '
            rest = joiner.join(parts)
            return 'FROM {0}'.format(rest)

    def where(self, json):
        if 'where' in json:
            return 'WHERE {0}'.format(self.dispatch(json['where']))

    def groupby(self, json):
        if 'groupby' in json:
            return 'GROUP BY {0}'.format(self.dispatch(json['groupby']))

    def having(self, json):
        if 'having' in json:
            return 'HAVING {0}'.format(self.dispatch(json['having']))

    def orderby(self, json):
        if 'orderby' in json:
            sort = json['orderby'].get('sort', '').upper()
            return 'ORDER BY {0} {1}'.format(
                self.dispatch(json['orderby']), sort).strip()

    def limit(self, json):
        if 'limit' in json:
            return 'LIMIT {0}'.format(self.dispatch(json['limit']))

    def offset(self, json):
        if 'offset' in json:
            return 'OFFSET {0}'.format(self.dispatch(json['offset']))
