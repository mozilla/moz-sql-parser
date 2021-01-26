# encoding: utf-8
import re
from collections import namedtuple

from mo_future import is_text

from mo_parsing.utils import Log, indent, quote, regex_range, alphanums, regex_iso

ParserElement, Literal, Token = [None] * 3

CURRENT = None


class Engine:
    def __init__(self, white=" \n\r\t"):
        self.literal = Literal
        self.keyword_chars = alphanums + "_$"
        self.ignore_list = []
        self.debugActions = DebugActions(noop, noop, noop)
        self.all_exceptions = {}
        self.content = None
        self.skips = {}
        self.regex = None
        self.set_whitespace(white)
        self.previous = None  # WE MAINTAIN A STACK OF ENGINES

    def __enter__(self):
        global CURRENT
        self.previous = CURRENT  # WE MAINTAIN A STACK OF ENGINES
        CURRENT = self
        return self

    use = __enter__

    def __exit__(self, exc_type, exc_val, exc_tb):
        """
        ENSURE self IS NOT CURRENT
        :return:
        """
        global CURRENT
        if not self.previous:
            Log.error("expecting engine to be released just once")

        CURRENT = self.previous
        self.previous = None

    def release(self):
        self.__exit__(None, None, None)

    def normalize(self, expr):
        if expr == None:
            return None
        if is_text(expr):
            if issubclass(self.literal, Token):
                return self.literal(expr)
            else:
                return self.literal(Literal(expr))
        if isinstance(expr, type) and issubclass(expr, ParserElement):
            return expr()  # ALLOW Empty WHEN Empty() WAS INTENDED
        if not isinstance(expr, ParserElement):
            Log.error("expecting string, or ParserElemenet")

        return expr

    def record_exception(self, string, loc, expr, exc):
        es = self.all_exceptions.setdefault(loc, [])
        es.append(exc)

    def set_literal(self, literal):
        self.literal = literal

    def set_keyword_chars(self, chars):
        self.keyword_chars = "".join(sorted(set(chars)))

    def set_whitespace(self, chars):
        self.white_chars = "".join(sorted(set(chars)))
        self.content = None
        self.regex = re.compile(self.__regex__()[1])

    def add_ignore(self, *ignore_exprs):
        """
        ADD TO THE LIST OF IGNORED EXPRESSIONS
        :param ignore_expr:
        """
        for ignore_expr in ignore_exprs:
            ignore_expr = ignore_expr.suppress()
            self.ignore_list.append(ignore_expr)
            self.content = None
            self.regex = re.compile(self.__regex__()[1])
            return self

    def backup(self):
        return Backup(self)

    def skip(self, string, start):
        if not self.ignore_list and not self.white_chars:
            return start
        if string is self.content:
            try:
                end = self.skips[start]
                if end != -1:
                    return end
            except IndexError:
                return start
        else:
            num = len(string)
            self.skips = [-1] * num
            self.content = string
            if start >= num:
                return start

        end = start  # TO AVOID RECURSIVE LOOP
        found = self.regex.match(string, start)
        if found:
            end = found.end()
        self.skips[start] = end  # THE REAL VALUE
        return end

    def __regex__(self):
        white = regex_range(self.white_chars)
        if not self.ignore_list:
            if not white:
                return "*", ""
            else:
                return "*", white + "*"

        ignored = "|".join(regex_iso(*i.__regex__(), "|") for i in self.ignore_list)
        return "+", f"(?:{white}*(?:{ignored}))*{white}*"

    def __str__(self):
        output = ["{"]
        for k, v in self.__dict__.items():
            value = str(v)
            output.append(indent(quote(k) + ":" + value))
        output.append("}")
        return "\n".join(output)


class Backup(object):
    def __init__(self, engine):
        self.engine = engine
        self.content = engine.content
        self.skips = engine.skips

    def __enter__(self):
        pass

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.engine.content = self.content
        self.engine.skips = self.skips


def noop(*args):
    return


DebugActions = namedtuple("DebugActions", ["TRY", "MATCH", "FAIL"])

PLAIN_ENGINE = Engine("").use()
Engine().use()
