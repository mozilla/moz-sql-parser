# encoding: utf-8
from collections import namedtuple
from threading import RLock

from mo_future import text
from mo_imports import expect, export
from mo_parsing.results import ParseResults, _flatten

from mo_parsing.exceptions import ParseException
from mo_parsing.utils import Log, MAX_INT, wrap_parse_action, empty_tuple, is_forward

(
    SkipTo,
    Many,
    ZeroOrMore,
    OneOrMore,
    Optional,
    NotAny,
    Suppress,
    And,
    MatchFirst,
    Or,
    MatchAll,
    Empty,
    StringEnd,
    Literal,
    Token,
    Group,
    regex_parameters
) = expect(
    "SkipTo",
    "Many",
    "ZeroOrMore",
    "OneOrMore",
    "Optional",
    "NotAny",
    "Suppress",
    "And",
    "MatchFirst",
    "Or",
    "MatchAll",
    "Empty",
    "StringEnd",
    "Literal",
    "Token",
    "Group",
    "regex_parameters"
)

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
streamlined = {}


def entrypoint(func):
    def output(*args, **kwargs):
        with locker:
            for a in _reset_actions:
                try:
                    a()
                except Exception as e:
                    Log.error("reset action failed", cause=e)

            self = args[0]
            if (
                not self.streamlined
                and (not is_forward(self) or not self.expr.streamlined)
                and not streamlined.get(id(self))
            ):
                Log.alert("Expecting expression to be streamlined before use")
                _id = id(self)
                self = self.streamline()
                streamlined[_id] = self
            return func(self, *args[1:], **kwargs)

    return output


class ParserElement(object):
    """Abstract base level parser element class."""

    __slots__ = [
        "parseAction",
        "parser_name",
        "token_name",
        "streamlined",
        "min_length_cache",
        "parser_config",
    ]
    Config = namedtuple("Config", ["callDuringTry", "failAction", "lock_engine"])

    def __init__(self):
        self.parseAction = list()
        self.parser_name = ""
        self.token_name = ""
        self.streamlined = False
        self.min_length_cache = -1

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
        output.parseAction = self.parseAction[:]
        output.parser_name = self.parser_name
        output.token_name = self.token_name
        output.parser_config = self.parser_config
        output.streamlined = self.streamlined
        output.min_length_cache = -1
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

    def addCondition(self, *fns, message=None, callDuringTry=False, fatal=False):
        """
        Add a boolean predicate function to expression's list of parse actions. See
        `setParseAction` for function call signatures. Unlike ``setParseAction``,
        functions passed to ``addCondition`` need to return boolean success/fail of the condition.

        Optional keyword arguments:
        - message = define a custom message to be used in the raised exception
        - fatal   = if True, will raise ParseFatalException to stop parsing immediately; otherwise will raise ParseException

        """

        def make_cond(fn):
            def cond(token, index, string):
                result = fn(token, index, string)
                if not bool(result.tokens[0]):
                    if fatal:
                        Log.error(
                            "fatal error",
                            casue=ParseException(
                                token.type, index, string, msg=message
                            ),
                        )
                    raise ParseException(token.type, index, string, msg=message)
                return token

            return cond

        output = self.copy()
        for fn in fns:
            output.parseAction.append(make_cond(wrap_parse_action(fn)))

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

    def expecting(self):
        """
        RETURN EXPECTED CHARACTER SEQUENCE, IF ANY
        :return:
        """
        return {}

    def min_length(self):
        if self.min_length_cache >= 0:
            return self.min_length_cache
        min_ = self._min_length()
        if self.streamlined:
            self.min_length_cache = min_
        return min_

    def _min_length(self):
        return 0

    def parseImpl(self, string, start, doActions=True):
        return ParseResults(self, start, start, [])

    def _parse(self, string, start, doActions=True):
        try:
            result = self.parseImpl(string, start, doActions)
        except Exception as cause:
            self.parser_config.failAction and self.parser_config.failAction(
                self, start, string, cause
            )
            raise

        if doActions or self.parser_config.callDuringTry:
            for fn in self.parseAction:
                next_result = fn(result, result.start, string)
                if next_result.end < result.end:
                    Log.error(
                        "parse action not allowed to roll back the end of parsing"
                    )
                result = next_result
        return result

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
        return self._parseString(string, parseAll=parseAll)

    def _parseString(self, string, parseAll=False):
        # TODO: PUT THIS streamline IN THE ENTRY POINT
        start = 0
        e = expr = self.streamline()
        while is_forward(e):
            e = e.expr

        eng = None
        if isinstance(e, Many) or isinstance(e, And):
            eng = e.parser_config.engine
            start = eng.skip(string, start)
        if expr.token_name:
            # TOP LEVEL NAMES ARE NOT ALLOWED
            expr = Group(expr)
        tokens = expr._parse(string, start)
        end = tokens.end
        if parseAll:
            if eng:
                end = eng.skip(string, end)
            StringEnd()._parse(string, end)
        return tokens

    @entrypoint
    def scanString(self, string, maxMatches=MAX_INT, overlap=False):
        """
        :param string: TO BE SCANNED
        :param maxMatches: MAXIMUM NUMBER MATCHES TO RETURN
        :param overlap: IF MATCHES CAN OVERLAP
        :return: SEQUENCE OF ParseResults, start, end
        """
        return self._scanString(string, maxMatches=maxMatches, overlap=overlap)

    def _scanString(self, string, maxMatches=MAX_INT, overlap=False):
        instrlen = len(string)
        end = 0
        matches = 0
        while end <= instrlen and matches < maxMatches:
            start = end
            # start = self.engine.skip(string, end)
            try:
                tokens = self._parse(string, start)
            except ParseException:
                end = start + 1
            else:
                matches += 1
                yield tokens, tokens.start, tokens.end
                if overlap or tokens.end <= end:
                    end += 1
                else:
                    end = tokens.end

    @entrypoint
    def transformString(self, string):
        """
        Modify matching text with results of a parse action.

        To use ``transformString``, define a grammar and
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
        return self._transformString(string)

    def _transformString(self, string):
        out = []
        end = 0
        # force preservation of <TAB>s, to minimize unwanted transformation of string, and to
        # keep string locs straight between transformString and scanString
        for t, s, e in self._scanString(string):
            out.append(string[end:s])
            if t:
                if isinstance(t, ParseResults):
                    out.append("".join(t))
                elif isinstance(t, list):
                    out.append("".join(t))
                else:
                    out.append(t)
            end = e
        out.append(string[end:])
        out = [o for o in out if o]
        return "".join(map(text, _flatten(out)))

    @entrypoint
    def searchString(self, string, maxMatches=MAX_INT):
        """
        :param string: Content to scan
        :param maxMatches: Limit number of matches
        :return: All the matches, packaged as ParseResults
        """
        return self._searchString(string, maxMatches=maxMatches)

    def _searchString(self, string, maxMatches=MAX_INT):
        if isinstance(self, Group):
            g = self
            scanned = [t for t, s, e in self._scanString(string, maxMatches)]
        else:
            g = Group(self)
            scanned = [
                ParseResults(g, s, e, [t])
                for t, s, e in self._scanString(string, maxMatches)
            ]

        if not scanned:
            return ParseResults(ZeroOrMore(g), -1, 0, [])
        else:
            return ParseResults(
                ZeroOrMore(g), scanned[0].start, scanned[-1].end, scanned
            )

    @entrypoint
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
        return self._split(
            string, maxsplit=maxsplit, includeSeparators=includeSeparators
        )

    def _split(self, string, maxsplit=MAX_INT, includeSeparators=False):
        last = 0
        for t, s, e in self._scanString(string, maxMatches=maxsplit):
            yield string[last:s]
            if includeSeparators:
                yield t[0]
            last = e
        yield string[last:]

    def replace_with(self, replacement):
        """
        Add parse action that replaces the token with replacement

        RegEx variables are accepted:
        \1
        \g<1>
        \g<name>
        """

        # FIND NAMES IN replacement
        parts = list(regex_parameters.split(replacement, includeSeparators=True))

        def replacer(tokens):
            acc = []
            for s, n in zip(parts, parts[1:]):
                acc.append(s)
                acc.append(text(tokens[n]))
            acc.append(parts[-1])
            return "".join(acc)

        return self.addParseAction(replacer)

    sub = replace_with

    def __add__(self, other):
        """
        Implementation of + operator - returns `And`. Adding strings to a ParserElement
        converts them to `Literal`s by default.
        """
        if other is Ellipsis:
            return _PendingSkip(self)

        return And([self, engine.CURRENT.normalize(other)], engine.CURRENT).streamline()

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
        return self + And.SyntaxErrorGuard() + engine.CURRENT.normalize(other)

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

        ret = Many(
            self, engine.CURRENT, min_match=minElements, max_match=maxElements
        ).streamline()
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
        return MatchAll([self, engine.CURRENT.normalize(other)])

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


NO_PARSER = (
    ParserElement().set_parser_name("<nothing>")
)  # USE THIS WHEN YOU DO NOT CARE ABOUT THE PARSER TYPE
NO_RESULTS = ParseResults(NO_PARSER, -1, 0, [])


export("mo_parsing.results", ParserElement)
export("mo_parsing.results", NO_PARSER)
export("mo_parsing.results", NO_RESULTS)

from mo_parsing import engine