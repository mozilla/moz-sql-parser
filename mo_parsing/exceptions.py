# encoding: utf-8
import json
from functools import wraps

from mo_dots import coalesce
from mo_future import text, is_text
from mo_imports import export

from mo_parsing.utils import Log, listwrap, quote, indent
from mo_parsing.utils import wrap_parse_action, col, line, lineno


class ParseException(Exception):
    """base exception class for all parsing runtime exceptions"""

    # Performance tuning: we construct a *lot* of these, so keep this
    # constructor as small and fast as possible
    __slots__ = []

    def __init__(self, expr, start, string, msg="", cause=None):
        if not isinstance(string, str):
            Log.error("expecting string")
        self.expr = expr
        self.start = start
        self.string = string
        self._msg = msg
        self.unsorted_cause = cause
        self._causes = None

    @property
    def causes(self):
        if self._causes is None:
            self._causes = list(sorted(
                listwrap(self.unsorted_cause),
                key=lambda e: -e.loc if isinstance(e, ParseException) else 0,
            ))
        return self._causes

    @property
    def __cause__(self):
        if self.causes:
            return self._causes[0]
        else:
            return None

    @property
    def loc(self):
        causes = self.causes
        if not causes:
            return self.start
        first_cause = causes[0]
        if isinstance(first_cause, ParseException):
            return first_cause.loc
        return self.start

    @property
    def message(self):
        if self._msg:
            expecting = f"{self._msg} ({self.expr})"
        else:
            expecting = f"Expecting {self.expr}"

        if self.loc >= len(self.string):
            found = "end of text"
        else:
            found = (self.string[self.loc : self.loc + 10]).replace(r"\\", "\\")

        return (
            f"{expecting}, found {quote(found)} (at char {self.loc},"
            f" (line:{self.lineno}, col:{self.column})"
        )

    @message.setter
    def msg(self, value):
        self._msg = value

    @property
    def line(self):
        return line(self.loc, self.string)

    @property
    def lineno(self):
        return lineno(self.loc, self.string)

    @property
    def col(self):
        return col(self.loc, self.string)

    @property
    def column(self):
        return col(self.loc, self.string)

    def __contains__(self, item):
        if is_text(item) and item in text(self):
            return True
        if (
            isinstance(item, type)
            and issubclass(item, Exception)
            and isinstance(self, item)
        ):
            return True
        for c in self.causes:
            if item in c:
                return True
        return False

    def __str__(self):
        causes = indent("".join("\n" + str(c) for c in self.causes))
        return f"{self.message}{causes}"

    def __repr__(self):
        return text(self)

    def markInputline(self, markerString=">!<"):
        """Extracts the exception line from the input string, and marks
        the location of the exception with a special symbol.
        """
        line_str = self.line
        line_column = self.column - 1
        if markerString:
            line_str = "".join((
                line_str[:line_column],
                markerString,
                line_str[line_column:],
            ))
        return line_str.strip()

    def __dir__(self):
        return "lineno col line".split() + dir(type(self))


class ParseSyntaxException(ParseException):
    """
    just like `ParseFatalException`, but thrown internally
    when an `ErrorStop<And.SyntaxErrorGuard>` ('-' operator) indicates
    that parsing is to stop immediately because an unbacktrackable
    syntax error has been found.
    """

    __slots__ = []


# ~ class ReparseException(ParseException):
# ~ """Experimental class - parse actions can raise this exception to cause
# ~ mo_parsing to reparse the input string:
# ~ - with a modified input string, and/or
# ~ - with a modified start location
# ~ Set the values of the ReparseException in the constructor, and raise the
# ~ exception in a parse action to cause mo_parsing to use the new string/location.
# ~ Setting the values as None causes no change to be made.
# ~ """
# ~ def __init_( self, newstring, restartLoc ):
# ~ self.newParseText = newstring
# ~ self.reparseLoc = restartLoc


class RecursiveGrammarException(Exception):
    """exception thrown by `ParserElement.validate` if the
    grammar could be improperly recursive
    """

    __slots__ = []

    def __init__(self, cycle):
        self.parseElementTrace = cycle

    def __str__(self):
        return "RecursiveGrammarException: " + json.dumps([
            str(e) for e in self.parseElementTrace
        ])


export("mo_parsing.utils", ParseException)
