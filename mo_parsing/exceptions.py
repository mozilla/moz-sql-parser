# encoding: utf-8
from functools import wraps
import sys

from mo_dots import coalesce
from mo_future import text
from mo_parsing.utils import Log

from mo_parsing.utils import wrap_parse_action, col, line, lineno


class ParseBaseException(Exception):
    """base exception class for all parsing runtime exceptions"""

    # Performance tuning: we construct a *lot* of these, so keep this
    # constructor as small and fast as possible
    def __init__(self, tokens, loc, string):
        if not isinstance(string, str):
            Log.error("expecting string")
        self.pstr = string
        self.loc = loc
        self.parserElement = tokens
        self._msg = ""

    @property
    def msg(self):
        if not self._msg:
            self._msg = "Expecting " + text(self.parserElement)
        return self._msg

    @msg.setter
    def msg(self, value):
        self._msg = value

    def __getattr__(self, aname):
        """supported attributes by name are:
        - lineno - returns the line number of the exception text
        - col - returns the column number of the exception text
        - line - returns the line containing the exception text
        """
        if aname == "lineno":
            return lineno(self.loc, self.pstr)
        elif aname in ("col", "column"):
            return col(self.loc, self.pstr)
        elif aname == "line":
            return line(self.loc, self.pstr)
        else:
            raise AttributeError(aname)

    def __str__(self):
        if self.pstr:
            if self.loc >= len(self.pstr):
                foundstr = ", found end of text"
            else:
                foundstr = (", found %r" % self.pstr[self.loc : self.loc + 1]).replace(
                    r"\\", "\\"
                )
        else:
            foundstr = ""
        return "%s%s  (at char %d), (line:%d, col:%d)" % (
            self.msg,
            foundstr,
            self.loc,
            self.lineno,
            self.column,
        )

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


class ParseException(ParseBaseException):
    """
    Exception thrown when parse expressions don't match class;
    supported attributes by name are:
    - lineno - returns the line number of the exception text
    - col - returns the column number of the exception text
    - line - returns the line containing the exception text

    Example::

        try:
            Word(nums).set_parser_name("integer").parseString("ABC")
        except ParseException as pe:
            print(pe)
            print("column: {}".format(pe.col))

    prints::

       Expected integer (at char 0), (line:1, col:1)
        column: 1

    """

    @staticmethod
    def explain(exc, depth=16):
        """
        Method to take an exception and translate the Python internal traceback into a list
        of the mo_parsing expressions that caused the exception to be raised.

        Parameters:

         - exc - exception raised during parsing (need not be a ParseException, in support
           of Python exceptions that might be raised in a parse action)
         - depth (default=16) - number of levels back in the stack trace to list expression
           and function names; if None, the full stack trace names will be listed; if 0, only
           the failing input line, marker, and exception string will be shown

        Returns a multi-line string listing the ParserElements and/or function names in the
        exception's stack trace.

        Note: the diagnostic output will include string representations of the expressions
        that failed to parse. These representations will be more helpful if you use `set_parser_name` to
        give identifiable names to your expressions. Otherwise they will use the default string
        forms, which may be cryptic to read.

        explain() is only supported under Python 3.
        """
        import inspect
        from mo_parsing import ParserElement

        if depth is None:
            depth = sys.getrecursionlimit()
        ret = []
        if isinstance(exc, ParseBaseException):
            ret.append(exc.line)
            ret.append(" " * (exc.col - 1) + "^")
        ret.append("{0}: {1}".format(type(exc).__name__, exc))

        if depth > 0:
            callers = inspect.getinnerframes(exc.__traceback__, context=depth)
            seen = set()
            for i, ff in enumerate(callers[-depth:]):
                frm = ff[0]

                f_self = frm.f_locals.get("self", None)
                if isinstance(f_self, ParserElement):
                    if frm.f_code.co_name not in ("parseImpl", "_parseNoCache"):
                        continue
                    if f_self in seen:
                        continue
                    seen.add(f_self)

                    self_type = type(f_self)
                    ret.append("{0}.{1} - {2}".format(
                        self_type.__module__, self_type.__name__, f_self
                    ))
                elif f_self is not None:
                    self_type = type(f_self)
                    ret.append("{0}.{1}".format(
                        self_type.__module__, self_type.__name__
                    ))
                else:
                    code = frm.f_code
                    if code.co_name in ("wrapper", "<module>"):
                        continue

                    ret.append("{0}".format(code.co_name))

                depth -= 1
                if not depth:
                    break

        return "\n".join(ret)


class ParseFatalException(ParseBaseException):
    """user-throwable exception thrown when inconsistent parse content
    is found; stops all parsing immediately"""

    pass


class ParseSyntaxException(ParseFatalException):
    """just like :class:`ParseFatalException`, but thrown internally
    when an :class:`ErrorStop<And._ErrorStop>` ('-' operator) indicates
    that parsing is to stop immediately because an unbacktrackable
    syntax error has been found.
    """

    pass


# ~ class ReparseException(ParseBaseException):
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
    """exception thrown by :class:`ParserElement.validate` if the
    grammar could be improperly recursive
    """

    def __init__(self, parseElementList):
        self.parseElementTrace = parseElementList

    def __str__(self):
        return "RecursiveGrammarException: %s" % self.parseElementTrace


class OnlyOnce(object):
    """Wrapper for parse actions, to ensure they are only called once."""

    def __init__(self, methodCall):
        self.callable = wrap_parse_action(methodCall)
        self.called = False

    def __call__(self, t, l, s):
        if not self.called:
            results = self.callable(t, l, s)
            self.called = True
            return results
        raise ParseException("", l, s)

    def reset(self):
        self.called = False


def conditionAsParseAction(fn, message=None, fatal=False):
    msg = coalesce(message, "failed user-defined condition")
    exc_type = ParseFatalException if fatal else ParseException
    fn = wrap_parse_action(fn)

    @wraps(fn)
    def pa(t, l, s):
        if not bool(fn(t, l, s)[0]):
            raise exc_type(s, l, msg)
        return t

    return pa
