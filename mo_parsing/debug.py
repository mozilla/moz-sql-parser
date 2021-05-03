# encoding: utf-8
from mo_future import text

from mo_parsing.core import ParserElement
from mo_parsing.utils import (
    quote,
    lineno,
    col,
    stack_depth,
    quote as plain_quote,
)

DEBUGGING = False


class Debugger(object):
    def __init__(self):
        self.previous_parse = None
        self.was_debugging = False

    def __enter__(self):
        global DEBUGGING
        self.was_debugging = DEBUGGING
        DEBUGGING = True
        self.previous_parse = ParserElement._parse
        ParserElement._parse = _debug_parse

    def __exit__(self, exc_type, exc_val, exc_tb):
        global DEBUGGING
        ParserElement._parse = self.previous_parse
        DEBUGGING = self.was_debugging


def _debug_parse(self, string, start, doActions=True):
    _try(self, start, string)
    loc = start
    try:
        tokens = self.parseImpl(string, loc, doActions)
    except Exception as cause:
        self.parser_config.failAction and self.parser_config.failAction(
            self, start, string, cause
        )
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
        + text(expr)[:300]
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
    quoted = plain_quote(text(cause))
    print("  Except  " + quoted)


def quote(value, start=0, length=12):
    return (plain_quote(value[start : start + length - 2]) + (" " * length))[:length]
