# encoding: utf-8
from mo_future import text

from mo_parsing.cache import packrat_cache
from mo_parsing.core import ParserElement
from mo_parsing.exceptions import ParseException
from mo_parsing.utils import (
    quote,
    lineno,
    col,
    stack_depth,
    quote as plain_quote,
)


class Debugger(object):
    def __init__(self):
        self.previous_parse = None

    def __enter__(self):
        self.previous_parse = ParserElement._parse
        ParserElement._parse = _debug_parse

    def __exit__(self, exc_type, exc_val, exc_tb):
        ParserElement._parse = self.previous_parse


def _debug_parse(self, string, start, doActions=True):
    lookup = (self, string, start, doActions)
    value = packrat_cache.get(lookup)
    if value is not None:
        if isinstance(value, Exception):
            raise value
        return value

    try:
        _try(self, start, string)
        loc = self.engine.skip(string, start)
        try:
            tokens = self.parseImpl(string, loc, doActions)
        except Exception as cause:
            loc = self.engine.skip(string, start)
            self.parser_config.failAction and self.parser_config.failAction(self, start, string, cause)
            fail(self, start, string, cause)
            raise

        if self.parseAction and (doActions or self.parser_config.callDuringTry):
            try:
                for fn in self.parseAction:
                    tokens = fn(tokens, start, string)
            except Exception as cause:
                fail(self, start, string, cause)
                raise
        match(self, loc, tokens.end, string, tokens)
    except ParseException as cause:
        packrat_cache.set(lookup, cause)
        raise

    packrat_cache.set(lookup, tokens)
    return tokens


def _try(expr, start, string):
    print(
        "  Attempt "
        + quote(string, start)
        + " at loc "
        + text(start)
        + "(%d,%d)" % (lineno(start, string), col(start, string))
        + " for "
        + " " * stack_depth()
        + text(expr)
    )


def match(expr, start, end, string, tokens):
    print(
        "> Matched "
        + quote(string[start:end])
        + "between "
        + f"[{start}, {end}] for"
        + " " * stack_depth()
        + text(expr)
        + " -> "
        + str(tokens)
    )


def fail(expr, start, string, cause):
    print("  Except  " + plain_quote(text(cause)))


def quote(value, start=0, length=12):
    return (plain_quote(value[start : start + length - 2]) + (" " * length))[:length]
