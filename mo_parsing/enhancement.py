# encoding: utf-8
import re
from collections import namedtuple

from mo_dots import Null, is_null
from mo_future import text, is_text

from mo_parsing.core import ParserElement
from mo_parsing.engine import Engine
from mo_parsing.exceptions import (
    ParseException,
    RecursiveGrammarException,
)
from mo_parsing.results import ParseResults, Annotation
from mo_parsing.utils import Log, listwrap, empty_tuple, regex_iso, append_config, regex_compile
from mo_parsing.utils import MAX_INT, is_forward

# import later
(
    Token,
    NoMatch,
    Literal,
    Keyword,
    Word,
    CharsNotIn,
    _PositionToken,
    StringEnd,
    Empty,
    Char,
) = [None] * 10

_get = object.__getattribute__


class ParseElementEnhance(ParserElement):
    """Abstract subclass of `ParserElement`, for combining and
    post-processing parsed tokens.
    """

    __slots__ = ["expr"]

    def __init__(self, expr):
        ParserElement.__init__(self)
        self.expr = expr = engine.CURRENT.normalize(expr)
        if is_forward(expr):
            expr.track(self)

    def copy(self):
        output = ParserElement.copy(self)
        if self.engine is engine.CURRENT:
            output.expr = self.expr
        else:
            output.expr = self.expr.copy()
        return output

    def _min_length(self):
        return self.expr.min_length()

    def parseImpl(self, string, start, doActions=True):
        result = self.expr._parse(string, start, doActions)
        return ParseResults(self, result.start, result.end, [result])

    def leaveWhitespace(self):
        with Engine(""):
            output = self.copy()
            output.expr = self.expr.leaveWhitespace()
            return output

    def streamline(self):
        if self.streamlined:
            return self

        expr = self.expr.streamline()
        if expr is self.expr:
            self.streamlined = True
            return self

        if not expr or isinstance(expr, Empty) and not self.is_annotated():
            return Empty()

        output = self.copy()
        output.expr = expr
        output.streamlined = True
        return output

    def checkRecursion(self, seen=empty_tuple):
        if self in seen:
            raise RecursiveGrammarException(seen + (self,))
        if self.expr != None:
            self.expr.checkRecursion(seen + (self,))

    def __str__(self):
        if self.parser_name:
            return self.parser_name
        return f"{self.__class__.__name__}:({self.expr})"


class FollowedBy(ParseElementEnhance):
    """Lookahead matching of the given parse expression.
    ``FollowedBy`` does *not* advance the parsing position within
    the input string, it only verifies that the specified parse
    expression matches at the current position.  ``FollowedBy``
    always returns a null token list. If any results names are defined
    in the lookahead expression, those *will* be returned for access by
    name.
    """

    __slots__ = []

    def __init__(self, expr):
        super(FollowedBy, self).__init__(expr)

    def parseImpl(self, string, start, doActions=True):
        # by using self._expr.parse and deleting the contents of the returned ParseResults list
        # we keep any named results that were defined in the FollowedBy expression
        result = self.expr._parse(string, start, doActions=doActions)
        result.__class__ = Annotation

        return ParseResults(self, start, start, [result])

    def __regex__(self):
        return "*", f"(?={self.expr.__regex__()[1]})"


class NotAny(ParseElementEnhance):
    """Lookahead to disallow matching with the given parse expression.
    ``NotAny`` does *not* advance the parsing position within the
    input string, it only verifies that the specified parse expression
    does *not* match at the current position.  Also, ``NotAny`` does
    *not* skip over leading whitespace. ``NotAny`` always returns
    a null token list.  May be constructed using the '~' operator.
    """

    __slots__ = ["regex"]

    def __init__(self, expr):
        super(NotAny, self).__init__(expr)
        prec, pattern = self.expr.__regex__()
        try:
            self.regex = regex_compile(f"(?!{pattern})")
        except Exception:
            pass

    def parseImpl(self, string, start, doActions=True):
        if self.regex:
            found = self.regex.match(string, start)
            if found:
                return ParseResults(self, start, start, [])
            raise ParseException(self, start, string)
        else:
            try:
                self.expr.parse(string, start, doActions=True)
                raise ParseException(self, start, string)
            except:
                return ParseResults(self, start, start, [])

    def streamline(self):
        output = ParseElementEnhance.streamline(self)
        if isinstance(output.expr, NoMatch):
            return Empty()
        if isinstance(output.expr, Empty):
            return NoMatch()
        return output

    def min_length(self):
        return 0

    def __regex__(self):
        return "*", self.regex.pattern

    def __str__(self):
        if self.parser_name:
            return self.parser_name
        return "~{" + str(self.expr) + "}"


class Many(ParseElementEnhance):
    __slots__ = []
    Config = append_config(ParseElementEnhance, "min_match", "max_match", "end")

    def __init__(self, expr, stopOn=None, min_match=0, max_match=MAX_INT, exact=None):
        """
        MATCH expr SOME NUMBER OF TIMES (OR UNTIL stopOn IS REACHED
        :param expr: THE EXPRESSION TO MATCH
        :param stopOn: THE PATTERN TO INDICATE STOP MATCHING (NOT REQUIRED IN PATTERN, JUST A QUICK STOP)
        :param min_match: MINIMUM MATCHES REQUIRED FOR SUCCESS (-1 IS INVALID)
        :param max_match: MAXIMUM MATCH REQUIRED FOR SUCCESS (-1 IS INVALID)
        """
        ParseElementEnhance.__init__(self, expr)
        if exact is not None:
            min_match = exact
            max_match = exact

        self.set_config(min_match=min_match, max_match=max_match)
        self.stopOn(stopOn)

    def stopOn(self, ender):
        if ender:
            end = self.engine.normalize(ender)
            self.set_config(end=regex_compile(end.__regex__()[1]))
        return self

    def _min_length(self):
        if self.parser_config.min_match == 0:
            return 0
        return self.expr.min_length()

    def parseImpl(self, string, start, doActions=True):
        acc = []
        end = start
        max = self.parser_config.max_match
        stopper = self.parser_config.end
        count = 0
        try:
            while end < len(string) and count < max:
                if stopper:
                    end = self.engine.skip(string, end)
                    if stopper.match(string, end):
                        if self.parser_config.min_match <= count:
                            break
                        else:
                            raise ParseException(
                                self, end, string, msg="found stopper too soon"
                            )
                result = self.expr._parse(string, end, doActions)
                end = result.end
                if result:
                    acc.append(result)
                    count += 1
        except ParseException:
            if self.parser_config.min_match <= count <= max:
                pass
            else:
                ParseException(self, start, string, msg="Not correct amount of matches")
        if count:
            if (
                count < self.parser_config.min_match
                or self.parser_config.max_match < count
            ):
                raise ParseException(
                    self,
                    acc[0].start,
                    string,
                    msg=(
                        f"Expecting between {self.parser_config.min_match} and"
                        f" {self.parser_config.max_match} of {self.expr}"
                    ),
                )
            else:
                return ParseResults(self, acc[0].start, acc[-1].end, acc)
        else:
            if not self.parser_config.min_match:
                return ParseResults(self, start, start, [])
            else:
                raise ParseException(
                    self,
                    start,
                    string,
                    msg=f"Expecting at least {self.parser_config.min_match} of {self}",
                )

    def streamline(self):
        if self.streamlined:
            return self
        expr = self.expr.streamline()
        if (
            self.parser_config.min_match == self.parser_config.max_match
            and not self.is_annotated()
        ):
            if self.parser_config.min_match == 0:
                return Empty()
            elif self.parser_config.min_match == 1:
                return expr

        if self.expr is expr:
            self.streamlined = True
            return self
        if expr.is_annotated() or not isinstance(expr, Empty):
            output = self.copy()
            output.expr = expr
            output.streamlined = True
            return output
        return Empty()

    def __regex__(self):
        end = self.parser_config.end.pattern if self.parser_config.end else None
        prec, regex = self.expr.__regex__()
        regex = regex_iso(prec, regex, "*")

        if self.parser_config.max_match == MAX_INT:
            if self.parser_config.min_match == 0:
                suffix = "*"
            elif self.parser_config.min_match == 1:
                suffix = "+"
            else:
                suffix = "{" + text(self.parser_config.min_match) + ",}"
        elif self.parser_config.min_match == self.parser_config.max_match:
            if self.parser_config.min_match == 1:
                suffix = ""
            else:
                suffix = "{" + text(self.parser_config.min_match) + "}"
        else:
            suffix = (
                "{"
                + text(self.parser_config.min_match)
                + ","
                + text(self.parser_config.max_match)
                + "}"
            )

        if end:
            return "+", regex + suffix + end
        else:
            return "*", regex + suffix

    def __call__(self, name):
        if not name:
            return self

        for e in [self.expr]:
            if isinstance(e, ParserElement) and e.token_name == name:
                Log.error(
                    "can not set token name, already set in one of the other"
                    " expressions"
                )

        return ParseElementEnhance.__call__(self, name)

    def __str__(self):
        if self.parser_name:
            return self.parser_name
        return f"{self.__class__.__name__}:({self.expr})"


class OneOrMore(Many):
    """Repetition of one or more of the given expression.

    Parameters:
     - expr - expression that must match one or more times
     - stopOn - (default= ``None``) - expression for a terminating sentinel
          (only required if the sentinel would ordinarily match the repetition
          expression)
    """

    __slots__ = []

    def __init__(self, expr, stopOn=None):
        Many.__init__(self, expr, stopOn, min_match=1, max_match=MAX_INT)
        self.set_config(lock_engine=self.expr.parser_config.lock_engine,)
        self.engine = self.expr.engine

    def __str__(self):
        if self.parser_name:
            return self.parser_name
        return "{" + text(self.expr) + "}..."


class ZeroOrMore(Many):
    """Optional repetition of zero or more of the given expression.

    Parameters:
     - expr - expression that must match zero or more times
     - stopOn - (default= ``None``) - expression for a terminating sentinel
          (only required if the sentinel would ordinarily match the repetition
          expression)

    Example: similar to `OneOrMore`
    """

    __slots__ = []

    def __init__(self, expr, stopOn=None):
        super(ZeroOrMore, self).__init__(
            expr, stopOn=stopOn, min_match=0, max_match=MAX_INT
        )
        self.set_config(lock_engine=self.expr.parser_config.lock_engine)
        self.engine = self.expr.engine

    def parseImpl(self, string, start, doActions=True):
        try:
            return super(ZeroOrMore, self).parseImpl(string, start, doActions)
        except ParseException:
            return ParseResults(self, start, start, [])

    def __str__(self):
        if self.parser_name:
            return self.parser_name

        return "[" + text(self.expr) + "]..."


class Optional(Many):
    """Optional matching of the given expression.

    Parameters:
     - expr - expression that must match zero or more times
     - default (optional) - value to be returned if the optional expression is not found.
    """

    __slots__ = []
    Config = append_config(Many, "defaultValue")

    def __init__(self, expr, default=None):
        Many.__init__(self, expr, stopOn=None, min_match=0, max_match=1)
        self.set_config(defaultValue=listwrap(default))

    def parseImpl(self, string, start, doActions=True):
        try:
            tokens = self.expr._parse(string, start, doActions)
            return ParseResults(self, tokens.start, tokens.end, [tokens])
        except ParseException:
            return ParseResults(self, start, start, self.parser_config.defaultValue)

    def __str__(self):
        if self.parser_name:
            return self.parser_name

        return "[" + text(self.expr) + "]"


class SkipTo(ParseElementEnhance):
    """Token for skipping over all undefined text until the matched expression is found."""

    __slots__ = []
    Config = append_config(ParseElementEnhance, "include", "fail", "ignore")

    def __init__(self, expr, include=False, ignore=None, failOn=None):
        """
        :param expr: target expression marking the end of the data to be skipped
        :param include: if True, the target expression is also parsed
          (the skipped text and target expression are returned as a 2-element list).
        :param ignore: used to define grammars (typically quoted strings and
          comments) that might contain false matches to the target expression
        :param failOn: define expressions that are not allowed to be
          included in the skipped test; if found before the target expression is found,
          the SkipTo is not a match
        """
        ParseElementEnhance.__init__(self, expr)
        self.set_config(
            include=include, fail=engine.CURRENT.normalize(failOn), ignore=ignore
        )
        self.parser_name = str(self)

    def min_length(self):
        return 0

    def parseImpl(self, string, start, doActions=True):
        instrlen = len(string)
        fail = self.parser_config.fail
        ignore = self.parser_config.ignore

        loc = start
        while loc <= instrlen:
            if fail:
                # break if failOn expression matches
                if fail.canParseNext(string, loc):
                    before_end = loc
                    break

            if ignore:
                # advance past ignore expressions
                while 1:
                    try:
                        loc = ignore.tryParse(string, loc)
                        if loc == None:
                            Log.error("")
                    except ParseException:
                        break
            try:
                before_end = loc
                loc = self.expr._parse(string, loc, doActions=False).end
                if loc == None:
                    Log.error("")
            except ParseException:
                # no match, advance loc in string
                loc += 1
            else:
                # matched skipto expr, done
                break

        else:
            # ran off the end of the input string without matching skipto expr, fail
            raise ParseException(self, start, string)

        # build up return values
        end = loc
        skiptext = string[start:before_end]
        skip_result = []
        if skiptext:
            skip_result.append(skiptext)

        if self.parser_config.include:
            end_result = self.expr._parse(string, before_end, doActions)
            skip_result.append(end_result)
            return ParseResults(self, start, end, skip_result)
        else:
            return ParseResults(self, start, before_end, skip_result)


class Forward(ParserElement):
    """Forward declaration of an expression to be defined later -
    used for recursive grammars, such as algebraic infix notation.
    When the expression is known, it is assigned to the ``Forward``
    variable using the '<<' operator.

    Note: take care when assigning to ``Forward`` not to overlook
    precedence of operators.

    Specifically, '|' has a lower precedence than '<<', so that::

        fwdExpr << a | b | c

    will actually be evaluated as::

        (fwdExpr << a) | b | c

    thereby leaving b and c out as parseable alternatives.  It is recommended that you
    explicitly group the values inserted into the ``Forward``::

        fwdExpr << (a | b | c)

    Converting to use the '<<=' operator instead will avoid this problem.

    See `ParseResults.pprint` for an example of a recursive
    parser created using ``Forward``.
    """

    __slots__ = ["expr", "used_by", "_str"]

    def __init__(self, expr=Null):
        ParserElement.__init__(self)
        self.expr = None
        self.used_by = []

        self._str = None  # avoid recursion
        if expr:
            self << engine.CURRENT.normalize(expr)

    def copy(self):
        output = ParserElement.copy(self)
        output.expr = self
        output._str = None

        output.used_by = []
        return output

    @property
    def name(self):
        return self.type.expr.token_name

    def track(self, expr):
        self.used_by.append(expr)

    def __lshift__(self, other):
        self._str = ""
        if is_forward(self.expr):
            return self.expr << other

        while is_forward(other):
            other = other.expr
        self.expr = engine.CURRENT.normalize(other).streamline()
        self.checkRecursion()
        return self

    def addParseAction(self, action):
        if not self.expr:
            Log.error("not allowed")
        self.expr = self.expr.addParseAction(action)

    def leaveWhitespace(self):
        with Engine(""):
            output = self.copy()
            output.expr = self.expr.leaveWhitespace()
            return output

    def streamline(self):
        if not self.expr or self.expr.streamlined:
            return self

        self.expr = self.expr.streamline()
        self.checkRecursion()
        return self

    def checkRecursion(self, seen=empty_tuple):
        if self in seen:
            raise RecursiveGrammarException(seen + (self,))
        if self.expr != None:
            self.expr.checkRecursion(seen + (self,))

    def min_length(self):
        if self.min_cache is None and self.expr:
            self.min_cache = 0  # BREAK CYCLE
            try:
                return self.expr.min_length()
            finally:
                self.min_cache = None
        return 0

    def parseImpl(self, string, loc, doActions=True):
        try:
            result = self.expr._parse(string, loc, doActions)
            return ParseResults(self, result.start, result.end, [result])
        except Exception as cause:
            if is_null(self.expr):
                Log.warning(
                    "Ensure you have assigned a ParserElement (<<) to this Forward",
                    cause=cause,
                )
            raise cause

    def __str__(self):
        if self.parser_name:
            return self.parser_name

        if self._str:
            return self._str

        # Avoid infinite recursion by setting a temporary strRepr
        self._str = "Forward: ..."
        try:
            self._str = "Forward: " + text(self.expr)[:1000]
        except Exception:
            pass
        return self._str

    def __call__(self, name):
        output = self.copy()
        output.token_name = name
        return output


class TokenConverter(ParseElementEnhance):
    """
    Abstract subclass of `ParseExpression`, for converting parsed results.
    """

    __slots__ = []

    def __regex__(self):
        return self.expr.__regex__()


class Combine(TokenConverter):
    """
    Converter to concatenate all matching tokens to a single string.
    """

    __slots__ = []
    Config = append_config(TokenConverter, "separator")

    def __init__(self, expr, separator=""):
        super(Combine, self).__init__(expr.streamline())
        self.set_config(separator=separator)
        self.parseAction.append(_combine)
        self.streamlined = True


def _combine(tokens, start, string):
    output = ParseResults(
        tokens.type,
        tokens.start,
        tokens.end,
        [tokens.asString(sep=tokens.type.parser_config.separator)],
    )
    return output


class Group(TokenConverter):
    """
    MARK A CLOSED PARSE RESULT
    """

    __slots__ = []

    def __init__(self, expr):
        ParserElement.__init__(self)
        self.expr = self.engine.normalize(expr)

    def is_annotated(self):
        return True


class Dict(Group):
    """
    Convert a list of tuples [(name, v1, v2, ...), ...]
    int dict-like lookup     {name: [v1, v2, ...], ...}

    mo-parsing uses the names of the ParserElement to name ParseResults,
    but this is a static naming scheme. Dict allows dynamic naming;
    Effectively defining new named ParserElements (called Annotations)
    at parse time
    """

    __slots__ = []

    def __init__(self, expr):
        Group.__init__(self, expr)
        self.parseAction.append(_dict_post_parse)


class OpenDict(TokenConverter):
    """
    Same as Dict, but not grouped: Open to previous (or subsequent) name: value pairs
    """

    __slots__ = []

    def __init__(self, expr):
        TokenConverter.__init__(self, expr)
        self.parseAction.append(_dict_post_parse)


def _dict_post_parse(tokens, loc, string):
    acc = tokens.tokens
    for a in list(acc):
        for tok in list(a):
            if not tok:
                continue
            if is_text(tok):
                new_tok = Annotation(tok, a.start, a.end, [])
            else:
                kv = list(tok)
                key = kv[0]
                value = kv[1:]
                new_tok = Annotation(text(key), tok.start, tok.end, value)
            acc.append(new_tok)

    return tokens


class Suppress(TokenConverter):
    """
    Converter for ignoring the results of a parsed expression.
    """

    __slots__ = []

    def __init__(self, expr):
        TokenConverter.__init__(self, expr)
        self.parseAction.append(_suppress_post_parse)

    def suppress(self):
        return self

    def __regex__(self):
        return self.expr.__regex__()

    def __str__(self):
        if self.parser_name:
            return self.parser_name
        return text(self.expr)


def _suppress_post_parse(tokens, start, string):
    return ParseResults(tokens.type, tokens.start, tokens.end, [])


class PrecededBy(ParseElementEnhance):
    """Lookbehind matching of the given parse expression.
    ``PrecededBy`` does not advance the parsing position within the
    input string, it only verifies that the specified parse expression
    matches prior to the current position.  ``PrecededBy`` always
    returns a null token list, but if a results name is defined on the
    given expression, it is returned.

    Parameters:

     - expr - expression that must match prior to the current parse
       location
     - retreat - (default= ``None``) - (int) maximum number of characters
       to lookbehind prior to the current parse location

    If the lookbehind expression is a string, Literal, Keyword, or
    a Word or CharsNotIn with a specified exact or maximum length, then
    the retreat parameter is not required. Otherwise, retreat must be
    specified to give a maximum number of characters to look back from
    the current parse position for a lookbehind match.
    """

    __slots__ = []
    Config = append_config(ParseElementEnhance, "retreat", "exact")

    def __init__(self, expr, retreat=None):
        super(PrecededBy, self).__init__(expr)
        expr = self.expr = self.expr.leaveWhitespace()

        if isinstance(expr, (Literal, Keyword, Char)):
            self.set_config(retreat=expr.min_length(), exact=True)
        elif isinstance(expr, (Word, CharsNotIn)):
            self.set_config(retreat=expr.min_length(), exact=False)
        elif isinstance(expr, _PositionToken):
            self.set_config(retreat=0, exact=True)
        else:
            self.set_config(retreat=expr.min_length(), exact=False)

    def parseImpl(self, string, start=0, doActions=True):
        if self.parser_config.exact:
            loc = start - self.parser_config.retreat
            if loc < 0:
                raise ParseException(self, start, string)
            ret = self.expr._parse(string, loc)
        else:
            # retreat specified a maximum lookbehind window, iterate
            test_expr = self.expr + StringEnd()
            instring_slice = string[:start]
            last_cause = ParseException(self, start, string)

            with self.engine.backup():
                for offset in range(self.parser_config.retreat, start + 1):
                    try:
                        ret = test_expr._parse(instring_slice, start - offset)
                        break
                    except ParseException as cause:
                        last_cause = cause
                else:
                    raise last_cause
        # return empty list of tokens, but preserve any defined results names

        ret.__class__ = Annotation
        return ParseResults(self, start, start, [ret])

    def __regex__(self):
        if self.parser_config.exact:
            return "*", f"(?<={self.expr.__regex__()[1]})"
        raise NotImplemented()


# export
from mo_parsing import core, engine, results

core.SkipTo = SkipTo
core.Many = Many
core.ZeroOrMore = ZeroOrMore
core.OneOrMore = OneOrMore
core.Optional = Optional
core.NotAny = NotAny
core.Suppress = Suppress
core.Group = Group

results.Group = Group
results.Dict = Dict
results.Suppress = Suppress
