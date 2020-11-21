# encoding: utf-8
from collections import namedtuple
from threading import RLock

from mo_future import text

from mo_parsing.cache import packrat_cache
from mo_parsing.engine import Engine
from mo_parsing.exceptions import (
    ParseException,
    ParseFatalException,
    conditionAsParseAction,
)
from mo_parsing.results import ParseResults
from mo_parsing.utils import Log, MAX_INT, wrap_parse_action, empty_tuple, is_forward

# import later
(
    SkipTo,
    Many,
    ZeroOrMore,
    OneOrMore,
    Optional,
    NotAny,
    Suppress,
    _flatten,
    replaceWith,
    quotedString,
    And,
    MatchFirst,
    Or,
    Each,
    Empty,
    StringEnd,
    Literal,
    Token,
    Group,
) = [None] * 19

DEBUG = False


# TODO: Replace with a stack of parse state
_reset_actions = []


def add_reset_action(action):
    """
    ADD A FUNCTION THAT WILL RESET GLOBAL STATE THAT A PARSER MAY USE
    :param action:  CALLABLE
    """
    _reset_actions.append(action)


locker = RLock()


def entrypoint(func):
    def output(*args, **kwargs):
        with locker:
            for a in _reset_actions:
                try:
                    a()
                except Exception as e:
                    Log.error("reset action failed", cause=e)

            self = args[0]
            if not self.streamlined and (
                not is_forward(self) or not self.expr.streamlined
            ):
                Log.alert("Expecting expression to be streamlined before use")
                self = self.streamline()
            return func(self, *args[1:], **kwargs)

    return output


class ParserElement(object):
    """Abstract base level parser element class."""

    __slots__ = [
        "parseAction",
        "parser_name",
        "token_name",
        "engine",
        "streamlined",
        "min_cache",
        "parser_config",
    ]
    Config = namedtuple("Config", ["callDuringTry", "failAction", "lock_engine"])

    def __init__(self):
        self.parseAction = list()
        self.parser_name = ""
        self.token_name = ""
        self.engine = engine.CURRENT
        self.streamlined = False
        self.min_cache = -1

        self.parser_config = self.Config(*([None] * len(self.Config._fields)))
        self.set_config(callDuringTry=False, failAction=None, lock_engine=None)

    def set_config(self, **map):
        data = {
            **dict(zip(self.parser_config.__class__._fields, self.parser_config)),
            **map,
        }
        self.parser_config = self.Config(*(data[f] for f in self.Config._fields))

    def copy(self):
        output = object.__new__(self.__class__)
        le = self.parser_config.lock_engine
        output.engine = le or engine.CURRENT
        output.parseAction = self.parseAction[:]
        output.parser_name = self.parser_name
        output.token_name = self.token_name
        output.parser_config = self.parser_config
        output.streamlined = self.streamlined
        output.min_cache = -1
        return output

    def set_parser_name(self, name):
        """
        Define name for this expression, makes debugging and exception messages clearer.

        Example::

            Word(nums).parseString("ABC")  # -> Exception: Expected W:(0123...) (at char 0), (line:1, col:1)
            Word(nums).set_parser_name("integer").parseString("ABC")  # -> Exception: Expected integer (at char 0), (line:1, col:1)
        """
        self.parser_name = name
        return self

    def setBreak(self, breakFlag=True):
        """Method to invoke the Python pdb debugger when this element is
        about to be parsed. Set ``breakFlag`` to True to enable, False to
        disable.
        """
        if breakFlag:
            _parseMethod = self._parse

            def breaker(string, loc, doActions=True):
                import pdb

                # this call to pdb.set_trace() is intentional, not a checkin error
                pdb.set_trace()
                return _parseMethod(string, loc, doActions)

            breaker._originalParseMethod = _parseMethod
            self._parse = breaker
        else:
            if hasattr(self._parse, "_originalParseMethod"):
                self._parse = self._parse._originalParseMethod
        return self

    def clearParseAction(self):
        """
        Add one or more parse actions to expression's list of parse actions. See `setParseAction`.

        See examples in `copy`.
        """
        output = self.copy()
        output.parseAction = []
        return output

    def addParseAction(self, *fns, callDuringTry=False):
        """
        Add one or more parse actions to expression's list of parse actions. See `setParseAction`.

        See examples in `copy`.
        """
        output = self.copy()
        output.parseAction += list(map(wrap_parse_action, fns))
        output.set_config(
            callDuringTry=self.parser_config.callDuringTry or callDuringTry
        )
        return output

    def addCondition(
        self, *fns, message=None, fatal=False, callDuringTry=False, **kwargs
    ):
        """Add a boolean predicate function to expression's list of parse actions. See
        `setParseAction` for function call signatures. Unlike ``setParseAction``,
        functions passed to ``addCondition`` need to return boolean success/fail of the condition.

        Optional keyword arguments:
        - message = define a custom message to be used in the raised exception
        - fatal   = if True, will raise ParseFatalException to stop parsing immediately; otherwise will raise ParseException

        """
        output = self.copy()
        for fn in fns:
            output.parseAction.append(conditionAsParseAction(
                fn, message=message, fatal=fatal
            ))

        output.set_config(
            callDuringTry=self.parser_config.callDuringTry or callDuringTry
        )
        return output

    def setFailAction(self, fn):
        """Define action to perform if parsing fails at this expression.
        Fail acton fn is a callable function that takes the arguments
        ``fn(s, loc, expr, err)`` where:
        - expr = the parse expression that failed
        - loc = location where expression match was attempted and failed
        - s = string being parsed
        - err = the exception thrown
        The function returns no value.  It may throw `ParseFatalException`
        if it is desired to stop parsing immediately."""
        self.set_config(failAction=fn)
        return self

    def is_annotated(self):
        return self.parseAction or self.token_name or self.parser_name

    def min_length(self):
        if not hasattr(self, "min_cache"):
            Log.error("should not happen")
        if self.min_cache >= 0:
            return self.min_cache
        min_ = self._min_length()
        if self.streamlined:
            self.min_cache = min_
        return min_

    def _min_length(self):
        return 0

    def parseImpl(self, string, start, doActions=True):
        return ParseResults(self, start, start, [])

    def _parse(self, string, start, doActions=True):
        lookup = (self, string, start, doActions)
        value = packrat_cache.get(lookup)
        if value is not None:
            if isinstance(value, Exception):
                raise value
            return value

        try:
            index = self.engine.skip(string, start)
            try:
                result = self.parseImpl(string, index, doActions)
            except Exception as cause:
                self.parser_config.failAction and self.parser_config.failAction(
                    self, start, string, cause
                )
                raise

            if self.parseAction and (doActions or self.parser_config.callDuringTry):
                for fn in self.parseAction:
                    next_result = fn(result, index, string)
                    if next_result.end < result.end:
                        Log.error("parse action not allowed to roll back the end of parsing")
                    result = next_result
        except ParseException as cause:
            packrat_cache.set(lookup, cause)
            raise

        packrat_cache.set(lookup, result)
        return result

    def tryParse(self, string, start):
        try:
            return self._parse(string, start, doActions=False).end
        except ParseFatalException as cause:
            raise ParseException(self, start, string, cause=cause)

    def canParseNext(self, string, start):
        try:
            return self.tryParse(string, start)
        except ParseException:
            return False

    @entrypoint
    def parseString(self, string, parseAll=False):
        """
        Parse a string with respect to the parser definition. This function is intended as the primary interface to the
        client code.

        :param string: The input string to be parsed.
        :param parseAll: If set, the entire input string must match the grammar.
        :raises ParseException: Raised if ``parseAll`` is set and the input string does not match the whole grammar.
        :returns: the parsed data as a `ParseResults` object, which may be accessed as a `list`, a `dict`, or
          an object with attributes if the given parser includes results names.

        If the input string is required to match the entire grammar, ``parseAll`` flag must be set to True. This
        is also equivalent to ending the grammar with ``StringEnd()``.

        To report proper column numbers, ``parseString`` operates on a copy of the input string where all tabs are
        converted to spaces (8 spaces per tab, as per the default in ``string.expandtabs``). If the input string
        contains tabs and the grammar uses parse actions that use the ``loc`` argument to index into the string
        being parsed, one can ensure a consistent view of the input string by doing one of the following:

        - define your parse action using the full ``(s,loc,toks)`` signature, and reference the input string using the
          parse action's ``s`` argument, or
        - explicitly expand the tabs in your input string before calling ``parseString``.

        """
        cache.resetCache()
        expr = self.streamline()
        for e in expr.engine.ignore_list:
            e.streamline()
        if expr.token_name:
            # TOP LEVEL NAMES ARE NOT ALLOWED
            expr = Group(expr)
        tokens = expr._parse(string, 0)
        end = tokens.end
        if parseAll:
            end = expr.engine.skip(string, end)
            StringEnd()._parse(string, end)
        return tokens

    @entrypoint
    def scanString(self, string, maxMatches=MAX_INT, overlap=False):
        """
        Scan the input string for expression matches.  Each match will return the
        matching tokens, start location, and end location.  May be called with optional
        ``maxMatches`` argument, to clip scanning after 'n' matches are found.  If
        ``overlap`` is specified, then overlapping matches will be reported.

        Note that the start and end locations are reported relative to the string
        being parsed.  See `parseString` for more information on parsing
        strings with embedded tabs.
        """
        if not self.streamlined:
            self.streamline()
            for e in self.engine.ignore_list:
                e.streamline()

        instrlen = len(string)
        end = 0
        cache.resetCache()
        matches = 0
        while end <= instrlen and matches < maxMatches:
            start = self.engine.skip(string, end)
            try:
                tokens = self._parse(string, start)
            except ParseException:
                end = start + 1
            else:
                matches += 1
                yield tokens, start, tokens.end
                if overlap or tokens.end <= end:
                    end += 1
                else:
                    end = tokens.end

    def transformString(self, string):
        """
        Extension to `scanString`, to modify matching text with modified tokens that may
        be returned from a parse action.  To use ``transformString``, define a grammar and
        attach a parse action to it that modifies the returned token list.
        Invoking ``transformString()`` on a target string will then scan for matches,
        and replace the matched text patterns according to the logic in the parse
        action.  ``transformString()`` returns the resulting transformed string.

        Example::

            wd = Word(alphas)
            wd.addParseAction(lambda toks: toks[0].title())

            print(wd.transformString("now is the winter of our discontent made glorious summer by this sun of york."))

        prints::

            Now Is The Winter Of Our Discontent Made Glorious Summer By This Sun Of York.
        """
        out = []
        lastE = 0
        # force preservation of <TAB>s, to minimize unwanted transformation of string, and to
        # keep string locs straight between transformString and scanString
        for t, s, e in self.scanString(string):
            out.append(string[lastE:s])
            if t:
                if isinstance(t, ParseResults):
                    out.append("".join(t))
                elif isinstance(t, list):
                    out.append("".join(t))
                else:
                    out.append(t)
            lastE = e
        out.append(string[lastE:])
        out = [o for o in out if o]
        return "".join(map(text, _flatten(out)))

    def searchString(self, string, maxMatches=MAX_INT):
        """
        Another extension to `scanString`, simplifying the access to the tokens found
        to match the given parse expression.  May be called with optional
        ``maxMatches`` argument, to clip searching after 'n' matches are found.
        """

        if isinstance(self, Group):
            g = self
            scanned = [t for t, s, e in self.scanString(string, maxMatches)]
        else:
            g = Group(self)
            scanned = [
                ParseResults(g, s, e, [t])
                for t, s, e in self.scanString(string, maxMatches)
            ]

        if not scanned:
            return ParseResults(ZeroOrMore(g), -1, -1, [])
        else:
            return ParseResults(
                ZeroOrMore(g), scanned[0].start, scanned[-1].end, scanned
            )

    def split(self, string, maxsplit=MAX_INT, includeSeparators=False):
        """
        Generator method to split a string using the given expression as a separator.
        May be called with optional ``maxsplit`` argument, to limit the number of splits;
        and the optional ``includeSeparators`` argument (default= ``False``), if the separating
        matching text should be included in the split results.

        Example::

            punc = oneOf(list(".,;:/-!?"))
            print(list(punc.split("This, this?, this sentence, is badly punctuated!")))

        prints::

            ['This', ' this', '', ' this sentence', ' is badly punctuated', '']
        """
        splits = 0
        last = 0
        for t, s, e in self.scanString(string, maxMatches=maxsplit):
            yield string[last:s]
            if includeSeparators:
                yield t[0]
            last = e
        yield string[last:]

    def __add__(self, other):
        """
        Implementation of + operator - returns `And`. Adding strings to a ParserElement
        converts them to `Literal`s by default.
        """
        if other is Ellipsis:
            return _PendingSkip(self)

        return And([self, engine.CURRENT.normalize(other)]).streamline()

    def __radd__(self, other):
        """
        Implementation of + operator when left operand is not a `ParserElement`
        """
        if other is Ellipsis:
            return SkipTo(self)("_skipped") + self

        return engine.CURRENT.normalize(other) + self

    def __sub__(self, other):
        """
        Implementation of - operator, returns `And` with error stop
        """
        return self + And._ErrorStop() + engine.CURRENT.normalize(other)

    def __rsub__(self, other):
        """
        Implementation of - operator when left operand is not a `ParserElement`
        """
        return engine.CURRENT.normalize(other) - self

    def __mul__(self, other):
        """
        Implementation of * operator, allows use of ``expr * 3`` in place of
        ``expr + expr + expr``.  Expressions may also me multiplied by a 2-integer
        tuple, similar to ``{min, max}`` multipliers in regular expressions.  Tuples
        may also include ``None`` as in:
         - ``expr*(n, None)`` or ``expr*(n, )`` is equivalent
              to ``expr*n + ZeroOrMore(expr)``
              (read as "at least n instances of ``expr``")
         - ``expr*(None, n)`` is equivalent to ``expr*(0, n)``
              (read as "0 to n instances of ``expr``")
         - ``expr*(None, None)`` is equivalent to ``ZeroOrMore(expr)``
         - ``expr*(1, None)`` is equivalent to ``OneOrMore(expr)``

        Note that ``expr*(None, n)`` does not raise an exception if
        more than n exprs exist in the input stream; that is,
        ``expr*(None, n)`` does not enforce a maximum number of expr
        occurrences.  If this behavior is desired, then write
        ``expr*(None, n) + ~expr``
        """
        if isinstance(other, tuple):
            minElements, maxElements = (other + (None, None))[:2]
        else:
            minElements, maxElements = other, other

        if minElements == Ellipsis or not minElements:
            minElements = 0
        elif not isinstance(minElements, int):
            raise TypeError(
                "cannot multiply 'ParserElement' and ('%s', '%s') objects",
                type(other[0]),
                type(other[1]),
            )
        elif minElements < 0:
            raise ValueError("cannot multiply ParserElement by negative value")

        if maxElements == Ellipsis or not maxElements:
            maxElements = MAX_INT
        elif (
            not isinstance(maxElements, int)
            or maxElements < minElements
            or maxElements == 0
        ):
            raise TypeError(
                "cannot multiply 'ParserElement' and ('%s', '%s') objects",
                type(other[0]),
                type(other[1]),
            )

        ret = Many(self, min_match=minElements, max_match=maxElements).streamline()
        return ret

    def __rmul__(self, other):
        return self.__mul__(other)

    def __or__(self, other):
        """
        Implementation of | operator - returns `MatchFirst`
        """
        if other is Ellipsis:
            return _PendingSkip(Optional(self))

        return MatchFirst([self, engine.CURRENT.normalize(other)]).streamline()

    def __ror__(self, other):
        """
        Implementation of | operator when left operand is not a `ParserElement`
        """
        return engine.CURRENT.normalize(other) | self

    def __xor__(self, other):
        """
        Implementation of ^ operator - returns `Or`
        """
        return Or([self, engine.CURRENT.normalize(other)])

    def __rxor__(self, other):
        """
        Implementation of ^ operator when left operand is not a `ParserElement`
        """
        return engine.CURRENT.normalize(other) ^ self

    def __and__(self, other):
        """
        Implementation of & operator - returns `Each`
        """
        return Each([self, engine.CURRENT.normalize(other)])

    def __rand__(self, other):
        """
        Implementation of & operator when left operand is not a `ParserElement`
        """
        return engine.CURRENT.normalize(other) & self

    def __invert__(self):
        """
        Implementation of ~ operator - returns `NotAny`
        """
        return NotAny(self)

    def __getitem__(self, key):
        if isinstance(key, slice):
            return self * (key.start, key.stop)
        return self * key

    def __call__(self, name):
        """
        Shortcut for `.set_token_name`, with ``listAllMatches=False``.
        """
        if not name:
            return self
        return self.set_token_name(name)

    def set_token_name(self, name):
        """
        SET name AS PART OF A LARGER GROUP
        :param name:
        """
        output = self.copy()
        output.token_name = name
        if not output.parser_name:
            output.parser_name = name
        return output

    def suppress(self):
        """
        Suppresses the output of this `ParserElement`; useful to keep punctuation from
        cluttering up returned output.
        """
        return Suppress(self)

    def leaveWhitespace(self):
        """
        Disables the skipping of whitespace before matching the characters in the
        `ParserElement`'s defined pattern.  This is normally only used internally by
        the mo_parsing module, but may be needed in some whitespace-sensitive grammars.
        """
        with Engine(""):
            output = self.copy()
        return output

    def __str__(self):
        return self.parser_name

    def __repr__(self):
        return text(self)

    def streamline(self):
        self.streamlined = True
        return self

    def checkRecursion(self, seen=empty_tuple):
        pass

    def parseFile(self, file_or_filename, parseAll=False):
        """
        Execute the parse expression on the given file or filename.
        If a filename is specified (instead of a file object),
        the entire file is opened, read, and closed before parsing.
        """
        try:
            file_contents = file_or_filename.read()
        except AttributeError:
            with open(file_or_filename, "r") as f:
                file_contents = f.read()
        return self.parseString(file_contents, parseAll)

    def __eq__(self, other):
        return self is other

    def __ne__(self, other):
        return not (self == other)

    def __hash__(self):
        return id(self)

    def __req__(self, other):
        return self == other

    def __rne__(self, other):
        return not (self == other)

    def matches(self, testString, parseAll=True):
        """
        Method for quick testing of a parser against a test string. Good for simple
        inline microtests of sub expressions while building up larger parser.

        Parameters:
         - testString - to test against this expression for a match
         - parseAll - (default= ``True``) - flag to pass to `parseString` when running tests

        Example::

            expr = Word(nums)
            assert expr.matches("100")
        """
        try:
            self.parseString(text(testString), parseAll=parseAll)
            return True
        except ParseException:
            return False


class _PendingSkip(ParserElement):
    # internal placeholder class to hold a place were '...' is added to a parser element,
    # once another ParserElement is added, this placeholder will be replaced with a SkipTo
    def __init__(self, expr):
        super(_PendingSkip, self).__init__()
        self.anchor = expr
        self.parser_name = "pending_skip"

    def __add__(self, other):
        if isinstance(other, _PendingSkip):
            return self.anchor + other

        skipper = SkipTo(other)("_skipped")
        return self.anchor + skipper + other

    def parseImpl(self, *args):
        Log.error("use of `...` expression without following SkipTo target expression")


# export
from mo_parsing import cache, engine, results

engine.ParserElement = ParserElement
results.ParserElement = ParserElement

NO_PARSER = (
    ParserElement().set_parser_name("<nothing>")
)  # USE THIS WHEN YOU DO NOT CARE ABOUT THE PARSER TYPE
NO_RESULTS = ParseResults(NO_PARSER, -1, 0, [])

results.NO_PARSER = NO_PARSER
results.NO_RESULTS = NO_RESULTS
del results
