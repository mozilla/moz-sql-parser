# encoding: utf-8
import re
import sre_constants
import warnings

from mo_future import text

from mo_parsing.core import ParserElement
from mo_parsing.engine import Engine, PLAIN_ENGINE
from mo_parsing.exceptions import ParseException
from mo_parsing.results import ParseResults
from mo_parsing.utils import (
    Log,
    regex_range,
    append_config,
    regex_type,
    regex_caseless, regex_compile,
)
from mo_parsing.utils import (
    MAX_INT,
    col,
    printables,
)


class Token(ParserElement):
    __slots__ = []
    Config = append_config(ParserElement, "match", "regex")

    def __init__(self):
        ParserElement.__init__(self)
        self.streamlined = True


class Empty(Token):
    """
    An empty token, will always match.
    Often used to consume-and-suppress trailing whitespace
    """

    __slots__ = []

    def __init__(self, name=None):
        Token.__init__(self)
        self.parser_name = name

    def is_annotated(self):
        return self.parseAction or self.token_name

    def min_length(self):
        return 0

    def parseImpl(self, string, start, doActions=True):
        end = self.engine.skip(string, start)
        return ParseResults(self, start, end, [])

    def __regex__(self):
        return self.engine.__regex__()

    def __str__(self):
        return self.parser_name or "Empty"


class NoMatch(Token):
    """A token that will never match."""

    __slots__ = []

    def __init__(self):
        super(NoMatch, self).__init__()
        self.parser_name = "NoMatch"

    def parseImpl(self, string, loc, doActions=True):
        raise ParseException(self, loc, string)

    def min_length(self):
        return 0

    def __regex__(self):
        return "+", "a^"


class AnyChar(Token):
    __slots__ = []

    def __init__(self):
        """
        Match any single character
        """
        Token.__init__(self)
        self.parser_name = "AnyChar"

    def parseImpl(self, string, loc, doActions=True):
        if loc >= len(string):
            raise ParseException(self, loc, string)
        return ParseResults(self, loc, loc + 1, [string[loc]])

    def min_length(self):
        return 1

    def __regex__(self):
        return "*", "."


class Literal(Token):
    """Token to exactly match a specified string."""

    __slots__ = []

    def __init__(self, match):
        Token.__init__(self)
        self.set_config(match=match)

        if len(match) == 0:
            Log.error("Literal must be at least one character")
        elif len(match) == 1:
            self.__class__ = SingleCharLiteral

    def parseImpl(self, string, start, doActions=True):
        match = self.parser_config.match
        if string.startswith(match, start):
            end = start + len(match)
            return ParseResults(self, start, end, [match])
        raise ParseException(self, start, string)

    def _min_length(self):
        return len(self.parser_config.match)

    def __regex__(self):
        return "+", re.escape(self.parser_config.match)

    def __str__(self):
        return self.parser_config.match


class SingleCharLiteral(Literal):
    __slots__ = []

    def parseImpl(self, string, start, doActions=True):
        try:
            if string[start] == self.parser_config.match:
                return ParseResults(self, start, start + 1, [self.parser_config.match])
        except IndexError:
            pass

        raise ParseException(self, start, string)

    def min_length(self):
        return 1

    def __regex__(self):
        return "*", re.escape(self.parser_config.match)


class Keyword(Token):
    """Token to exactly match a specified string as a keyword, that is,
    it must be immediately followed by a non-keyword character.  Compare
    with `Literal`:

     - ``Literal("if")`` will match the leading ``'if'`` in
       ``'ifAndOnlyIf'``.
     - ``Keyword("if")`` will not; it will only match the leading
       ``'if'`` in ``'if x=1'``, or ``'if(y==2)'``

    Accepts two optional constructor arguments in addition to the
    keyword string:

     - ``ident_chars`` is a string of characters that would be valid
       identifier characters, defaulting to all alphanumerics + "_" and
       "$"
     - ``caseless`` allows case-insensitive matching, default is ``False``.

    For case-insensitive matching, use `CaselessKeyword`.
    """

    __slots__ = []
    Config = append_config(Token, "ident_chars")

    def __init__(self, match, ident_chars=None, caseless=None):
        Token.__init__(self)
        if ident_chars is None:
            ident_chars = self.engine.keyword_chars
        else:
            ident_chars = "".join(sorted(set(ident_chars)))

        if caseless:
            pattern = regex_caseless(match)
        else:
            pattern = re.escape(match)

        non_word = "($|(?!" + regex_range(ident_chars) + "))"
        self.set_config(
            ident_chars=ident_chars, match=match, regex=regex_compile(pattern + non_word)
        )

        self.parser_name = match
        if caseless:
            self.__class__ = CaselessKeyword

    def parseImpl(self, string, start, doActions=True):
        found = self.parser_config.regex.match(string, start)
        if found:
            return ParseResults(self, start, found.end(), [self.parser_config.match])
        raise ParseException(self, start, string)

    def _min_length(self):
        return len(self.parser_config.match)

    def __regex__(self):
        return "+", self.parser_config.regex.pattern


class CaselessKeyword(Keyword):
    __slots__ = []

    def __init__(self, matchString, ident_chars=None):
        Keyword.__init__(self, matchString, ident_chars, caseless=True)


class CaselessLiteral(Literal):
    """Token to match a specified string, ignoring case of letters.
    Note: the matched results will always be in the case of the given
    match string, NOT the case of the input text.
    """

    __slots__ = []

    def __init__(self, match):
        Literal.__init__(self, match.upper())
        self.set_config(
            match=match,
            regex=regex_compile(regex_caseless(match)),
        )
        self.parser_name = repr(self.parser_config.regex.pattern)

    def parseImpl(self, string, start, doActions=True):
        found = self.parser_config.regex.match(string, start)
        if found:
            return ParseResults(self, start, found.end(), [self.parser_config.match],)
        raise ParseException(self, start, string)


class CloseMatch(Token):
    """
    A variation on `Literal` which matches "close" matches,
    that is, strings with at most 'n' mismatching characters.
    `CloseMatch` takes parameters:

     - ``match_string`` - string to be matched
     - ``maxMismatches`` - (``default=1``) maximum number of
       mismatches allowed to count as a match

    The results from a successful parse will contain the matched text
    from the input string and the following named results:

     - ``mismatches`` - a list of the positions within the
       match_string where mismatches were found
     - ``original`` - the original match_string used to compare
       against the input string

    If ``mismatches`` is an empty list, then the match was an exact
    match.
    """

    __slots__ = []
    Config = append_config(Token, "maxMismatches")

    def __init__(self, match_string, maxMismatches=1):
        super(CloseMatch, self).__init__()
        self.parser_name = match_string
        self.set_config(match=match_string, maxMismatches=maxMismatches)

    def parseImpl(self, string, start, doActions=True):
        end = start
        instrlen = len(string)
        maxloc = start + len(self.parser_config.match)

        if maxloc <= instrlen:
            match = self.parser_config.match
            match_stringloc = 0
            mismatches = []
            maxMismatches = self.parser_config.maxMismatches

            for match_stringloc, (src, mat) in enumerate(zip(
                string[end:maxloc], match
            )):
                if src != mat:
                    mismatches.append(match_stringloc)
                    if len(mismatches) > maxMismatches:
                        break
            else:
                end = match_stringloc + 1
                results = ParseResults(self, start, end, [string[start:end]])
                results["original"] = match
                results["mismatches"] = mismatches
                return results

        raise ParseException(self, start, string)


class Word(Token):
    """Token for matching words composed of allowed character sets.
    Defined with string containing all allowed initial characters, an
    optional string containing allowed body characters (if omitted,
    defaults to the initial character set), and an optional minimum,
    maximum, and/or exact length.  The default value for ``min`` is
    1 (a minimum value < 1 is not valid); the default values for
    ``max`` and ``exact`` are 0, meaning no maximum or exact
    length restriction. An optional ``excludeChars`` parameter can
    list characters that might be found in the input ``body_chars``
    string; useful to define a word of all printables except for one or
    two characters, for instance.

    `srange` is useful for defining custom character set strings
    for defining ``Word`` expressions, using range notation from
    regular expression character sets.

    A common mistake is to use `Word` to match a specific literal
    string, as in ``Word("Address")``. Remember that `Word`
    uses the string argument to define *sets* of matchable characters.
    This expression would match "Add", "AAA", "dAred", or any other word
    made up of the characters 'A', 'd', 'r', 'e', and 's'. To match an
    exact literal string, use `Literal` or `Keyword`.

    mo_parsing includes helper strings for building Words:

     - `alphas`
     - `nums`
     - `alphanums`
     - `hexnums`
     - `alphas8bit` (alphabetic characters in ASCII range 128-255
       - accented, tilded, umlauted, etc.)
     - `punc8bit` (non-alphabetic characters in ASCII range
       128-255 - currency, symbols, superscripts, diacriticals, etc.)
     - `printables` (any non-whitespace character)

    """

    __slots__ = []
    Config = append_config(Token, "min")

    def __init__(
        self,
        init_chars,
        body_chars=None,
        min=1,
        max=None,
        exact=0,
        asKeyword=False,
        excludeChars=None,
    ):
        Token.__init__(self)

        if body_chars is None:
            body_chars = init_chars
        if exact:
            min = max = exact

        if min < 1:
            raise ValueError(
                "cannot specify a minimum length < 1; use Optional(Word()) if"
                " zero-length word is permitted"
            )

        if body_chars == init_chars:
            prec, regexp = Char(
                init_chars, excludeChars=excludeChars
            )[min:max].__regex__()
        elif max is None or max == MAX_INT:
            prec, regexp = (
                Char(init_chars, excludeChars=excludeChars)
                + Char(body_chars, excludeChars=excludeChars)[min - 1 :]
            ).__regex__()
        else:
            prec, regexp = (
                Char(init_chars, excludeChars=excludeChars)
                + Char(body_chars, excludeChars=excludeChars)[min - 1 : max - 1]
            ).__regex__()

        if asKeyword:
            regexp = r"\b" + regexp + r"\b"

        self.set_config(regex=regex_compile(regexp), min=min)

    def parseImpl(self, string, start, doActions=True):
        found = self.parser_config.regex.match(string, start)
        if found:
            return ParseResults(self, start, found.end(), [found.group()])

        raise ParseException(self, start, string)

    def min_length(self):
        return self.parser_config.min

    def __regex__(self):
        return "+", self.parser_config.regex.pattern

    def __str__(self):
        if self.parser_name:
            return self.parser_name
        return f"W:({self.parser_config.regex.pattern})"


class Char(Token):
    __slots__ = []
    Config = append_config(Token, "charset")

    def __init__(self, charset, asKeyword=False, excludeChars=None):
        """
        Represent one character in a given charset
        """
        Token.__init__(self)
        if excludeChars:
            charset = set(charset) - set(excludeChars)
        regex = regex_range(charset)
        if asKeyword:
            regex = r"\b%s\b" % self
        self.set_config(
            regex=regex_compile(regex),
            charset="".join(sorted(set(charset))),
        )

    def parseImpl(self, string, start, doActions=True):
        found = self.parser_config.regex.match(string, start)
        if found:
            return ParseResults(self, start, found.end(), [found.group()])

        raise ParseException(self, start, string)

    def min_length(self):
        return 1

    def __regex__(self):
        return "*", self.parser_config.regex.pattern

    def __str__(self):
        return self.parser_config.regex.pattern


class Regex(Token):
    r"""Token for matching strings that match a given regular
    expression. Defined with string specifying the regular expression in
    a form recognized by the stdlib Python  `re module <https://docs.python.org/3/library/re.html>`_.
    If the given regex contains named groups (defined using ``(?P<name>...)``),
    these will be preserved as named parse results.
    """
    __slots__ = []
    Config = append_config(Token, "flags")

    def __new__(cls, pattern, flags=0, asGroupList=False, asMatch=False):
        if asGroupList:
            return object.__new__(_RegExAsGroup)
        elif asMatch:
            return object.__new__(_RegExAsMatch)
        else:
            return object.__new__(cls)

    def __init__(self, pattern, flags=0, asGroupList=False, asMatch=False):
        """The parameters ``pattern`` and ``flags`` are passed
        to the ``regex_compile()`` function as-is. See the Python
        `re module <https://docs.python.org/3/library/re.html>`_ module for an
        explanation of the acceptable patterns and flags.
        """
        Token.__init__(self)

        if isinstance(pattern, text):
            if not pattern:
                warnings.warn(
                    "null string passed to Regex; use Empty() instead",
                    SyntaxWarning,
                    stacklevel=2,
                )

            try:
                self.set_config(flags=flags, regex=re.compile(pattern, flags))
            except sre_constants.error as cause:
                Log.error(
                    "invalid pattern {{pattern}} passed to Regex",
                    pattern=pattern,
                    cause=cause,
                )
        elif isinstance(pattern, regex_type):
            self.set_config(flags=flags, regex=pattern)
        else:
            Log.error(
                "Regex may only be constructed with a string or a compiled RE object"
            )

        self.parser_name = text(self)

    def parseImpl(self, string, start, doActions=True):
        found = self.parser_config.regex.match(string, start)
        if found:
            ret = ParseResults(self, start, found.end(), [found.group()])
            d = found.groupdict()
            if d:
                for k, v in d.items():
                    ret[k] = v
            return ret

        raise ParseException(self, start, string)

    def min_length(self):
        return 0

    def __regex__(self):
        return "|", self.parser_config.regex.pattern

    def __str__(self):
        return self.parser_config.regex.pattern

    def sub(self, repl):
        r"""
        Return Regex with an attached parse action to transform the parsed
        result as if called using `re.sub(expr, repl, string) <https://docs.python.org/3/library/re.html#re.sub>`_.

        Example::

            make_html = Regex(r"(\w+):(.*?):").sub(r"<\1>\2</\1>")
            print(make_html.transformString("h1:main title:"))
            # prints "<h1>main title</h1>"
        """

        def pa(tokens):
            return self.parser_config.regex.sub(repl, tokens[0])

        return self.addParseAction(pa)


class _RegExAsGroup(Regex):
    __slots__ = []

    def parseImpl(self, string, start, doActions=True):
        result = self.parser_config.regex.match(string, start)
        if not result:
            raise ParseException(self, start, string)

        return ParseResults(self, start, result.end(), [result.groups()])

    def sub(self, repl):
        raise SyntaxError("cannot use sub() with Regex(asGroupList=True)")


class _RegExAsMatch(Regex):
    __slots__ = []

    def parseImpl(self, string, start, doActions=True):
        result = self.parser_config.regex.match(string, start)
        if not result:
            raise ParseException(self, start, string)

        return ParseResults(self, start, result.end(), [result])

    def sub(self, repl):
        if callable(repl):
            raise SyntaxError(
                "cannot use sub() with a callable with Regex(asMatch=True)"
            )

        def pa(tokens):
            return tokens[0].expand(repl)

        return self.addParseAction(pa)


class QuotedString(Token):
    r"""
    Token for matching strings that are delimited by quoting characters.

    Defined with the following parameters:

        - quoteChar - string of one or more characters defining the
          quote delimiting string
        - escChar - character to escape quotes, typically backslash
          (default= ``None``)
        - escQuote - special quote sequence to escape an embedded quote
          string (such as SQL's ``""`` to escape an embedded ``"``)
          (default= ``None``)
        - multiline - boolean indicating whether quotes can span
          multiple lines (default= ``False``)
        - unquoteResults - boolean indicating whether the matched text
          should be unquoted (default= ``True``)
        - endQuoteChar - string of one or more characters defining the
          end of the quote delimited string (default= ``None``  => same as
          quoteChar)
        - convertWhitespaceEscapes - convert escaped whitespace
          (``'\t'``, ``'\n'``, etc.) to actual whitespace
          (default= ``True``)

    """
    __slots__ = []
    Config = append_config(
        Token,
        "quote_char",
        "end_quote_char",
        "esc_char",
        "esc_quote",
        "multiline",
        "unquoteResults",
        "convertWhitespaceEscapes",
        "escCharReplacePattern",
    )

    def __init__(
        self,
        quoteChar,
        escChar=None,
        escQuote=None,
        multiline=False,
        unquoteResults=True,
        endQuoteChar=None,
        convertWhitespaceEscapes=True,
    ):
        super(QuotedString, self).__init__()

        # remove white space from quote chars - wont work anyway
        quoteChar = quoteChar.strip()
        if not quoteChar:
            warnings.warn(
                "quoteChar cannot be the empty string", SyntaxWarning, stacklevel=2
            )
            raise SyntaxError()

        if endQuoteChar is None:
            endQuoteChar = quoteChar
        else:
            endQuoteChar = endQuoteChar.strip()
            if not endQuoteChar:
                warnings.warn(
                    "endQuoteChar cannot be the empty string",
                    SyntaxWarning,
                    stacklevel=2,
                )
                raise SyntaxError()

        self.set_config(
            quote_char=quoteChar,
            end_quote_char=endQuoteChar,
            esc_char=escChar,
            esc_quote=escQuote,
            unquoteResults=unquoteResults,
            convertWhitespaceEscapes=convertWhitespaceEscapes,
        )
        # TODO: FIX THIS MESS. WE SHOULD BE ABLE TO CONSTRUCT REGEX FROM ParserElements
        included = Empty()
        excluded = Literal(self.parser_config.end_quote_char)

        if not multiline:
            excluded |= Char("\r\n")
        if escQuote:
            included |= Literal(escQuote)
        if escChar:
            excluded |= Literal(self.parser_config.esc_char)
            included = included | escChar + Char(printables)
            self.set_config(
                escCharReplacePattern=re.escape(self.parser_config.esc_char) + "(.)"
            )

        prec, pattern = (
            Literal(quoteChar)
            + ((~excluded + AnyChar()) | included)[0:]
            + Literal(self.parser_config.end_quote_char)
        ).__regex__()

        self.set_config(multiline=multiline, regex=regex_compile(pattern))

        self.parser_name = text(self)

    def parseImpl(self, string, start, doActions=True):
        found = self.parser_config.regex.match(string, start)
        if not found:
            raise ParseException(self, start, string)

        end = found.end()
        ret = found.group()

        if self.parser_config.unquoteResults:

            # strip off quotes
            ret = ret[
                len(self.parser_config.quote_char) : -len(self.parser_config.end_quote_char)
            ]

            if isinstance(ret, text):
                # replace escaped whitespace
                if "\\" in ret and self.parser_config.convertWhitespaceEscapes:
                    ws_map = {
                        r"\t": "\t",
                        r"\n": "\n",
                        r"\f": "\f",
                        r"\r": "\r",
                    }
                    for wslit, wschar in ws_map.items():
                        ret = ret.replace(wslit, wschar)

                # replace escaped characters
                if self.parser_config.esc_char:
                    ret = re.sub(
                        self.parser_config.escCharReplacePattern, r"\g<1>", ret
                    )

                # replace escaped quotes
                if self.parser_config.esc_quote:
                    ret = ret.replace(
                        self.parser_config.esc_quote, self.parser_config.end_quote_char
                    )

        return ParseResults(self, start, end, [ret])

    def min_length(self):
        return 2

    def __str__(self):
        try:
            return super(QuotedString, self).__str__()
        except Exception:
            pass

        return "quoted string, starting with %s ending with %s" % (
            self.parser_config.quote_char,
            self.parser_config.end_quote_char,
        )


class CharsNotIn(Token):
    """Token for matching words composed of characters *not* in a given
    set (will include whitespace in matched characters if not listed in
    the provided exclusion set - see example). Defined with string
    containing all disallowed characters, and an optional minimum,
    maximum, and/or exact length.  The default value for ``min`` is
    1 (a minimum value < 1 is not valid); the default values for
    ``max`` and ``exact`` are 0, meaning no maximum or exact
    length restriction.
    """

    __slots__ = []
    Config = append_config(Token, "min_len", "max_len", "not_chars")

    def __init__(self, notChars, min=1, max=0, exact=0):
        Token.__init__(self)
        not_chars = "".join(sorted(set(notChars)))

        if min < 1:
            raise ValueError(
                "cannot specify a minimum length < 1; use "
                "Optional(CharsNotIn()) if zero-length char group is permitted"
            )

        max = max if max > 0 else MAX_INT
        if exact:
            min = exact
            max = exact

        if len(notChars) == 1:
            regex = "[^" + regex_range(notChars) + "]"
        else:
            regex = "[^" + regex_range(notChars)[1:]

        if not max or max == MAX_INT:
            if min == 0:
                suffix = "*"
            elif min == 1:
                suffix = "+"
            else:
                suffix = "{" + str(min) + ":}"
        elif min == 1 and max == 1:
            suffix = ""
        else:
            suffix = "{" + str(min) + ":" + str(max) + "}"

        self.set_config(
            regex=regex_compile(regex + suffix),
            min_len=min,
            max_len=max,
            not_chars=not_chars,
        )
        self.parser_name = text(self)

    def parseImpl(self, string, start, doActions=True):
        found = self.parser_config.regex.match(string, start)
        if found:
            return ParseResults(self, start, found.end(), [found.group()])

        raise ParseException(self, start, string)

    def min_length(self):
        return self.parser_config.min_len

    def __regex__(self):
        return "*", self.parser_config.regex.pattern

    def __str__(self):
        return self.parser_config.regex.pattern


class White(Token):
    """Special matching class for matching whitespace.  Normally,
    whitespace is ignored by mo_parsing grammars.  This class is included
    when some whitespace structures are significant.  Define with
    a string containing the whitespace characters to be matched; default
    is ``" \\t\\r\\n"``.  Also takes optional ``min``,
    ``max``, and ``exact`` arguments, as defined for the
    `Word` class.
    """

    whiteStrs = {
        " ": "<SP>",
        "\t": "<TAB>",
        "\n": "<LF>",
        "\r": "<CR>",
        "\f": "<FF>",
        "u\00A0": "<NBSP>",
        "u\1680": "<OGHAM_SPACE_MARK>",
        "u\180E": "<MONGOLIAN_VOWEL_SEPARATOR>",
        "u\2000": "<EN_QUAD>",
        "u\2001": "<EM_QUAD>",
        "u\2002": "<EN_SPACE>",
        "u\2003": "<EM_SPACE>",
        "u\2004": "<THREE-PER-EM_SPACE>",
        "u\2005": "<FOUR-PER-EM_SPACE>",
        "u\2006": "<SIX-PER-EM_SPACE>",
        "u\2007": "<FIGURE_SPACE>",
        "u\2008": "<PUNCTUATION_SPACE>",
        "u\2009": "<THIN_SPACE>",
        "u\200A": "<HAIR_SPACE>",
        "u\200B": "<ZERO_WIDTH_SPACE>",
        "u\202F": "<NNBSP>",
        "u\205F": "<MMSP>",
        "u\3000": "<IDEOGRAPHIC_SPACE>",
    }

    __slots__ = []
    Config = append_config(Token, "min_len", "max_len", "white_chars")

    def __init__(self, ws=" \t\r\n", min=1, max=0, exact=0):
        with Engine(white="".join(
            c for c in self.engine.white_chars if c not in ws
        )) as e:
            super(White, self).__init__()
            self.set_config(lock_engine=e)
        white_chars = "".join(sorted(set(ws)))
        self.parser_name = "|".join(White.whiteStrs[c] for c in white_chars)

        max = max if max > 0 else MAX_INT
        if exact > 0:
            max = exact
            min = exact
        self.set_config(min_len=min, max_len=max, white_chars=white_chars)

    def parseImpl(self, string, start, doActions=True):
        if string[start] not in self.parser_config.white_chars:
            raise ParseException(self, start, string)
        end = start
        end += 1
        maxloc = start + self.parser_config.max_len
        maxloc = min(maxloc, len(string))
        while end < maxloc and string[end] in self.parser_config.white_chars:
            end += 1

        if end - start < self.parser_config.min_len:
            raise ParseException(self, end, string)

        return ParseResults(self, start, end, string[start:end])


class _PositionToken(Token):
    __slots__ = []

    def __init__(self):
        super(_PositionToken, self).__init__()
        self.parser_name = self.__class__.__name__

    def min_length(self):
        return 0


class GoToColumn(_PositionToken):
    """Token to advance to a specific column of input text; useful for
    tabular report scraping.
    """

    __slots__ = []

    def __init__(self, colno):
        super(GoToColumn, self).__init__()
        self.col = colno

    def preParse(self, string, loc):
        if col(loc, string) != self.col:
            instrlen = len(string)
            loc = self._skipIgnorables(string, loc)
            while (
                loc < instrlen
                and string[loc].isspace()
                and col(loc, string) != self.col
            ):
                loc += 1
        return loc

    def parseImpl(self, string, start, doActions=True):
        thiscol = col(start, string)
        if thiscol > self.col:
            raise ParseException(self, start, string, "Text not in expected column")
        newloc = start + self.col - thiscol
        ret = string[start:newloc]
        return newloc, ret


class LineStart(_PositionToken):
    r"""Matches if current position is at the beginning of a line within
    the parse string
    """
    __slots__ = []

    def __init__(self):
        super(LineStart, self).__init__()

    def parseImpl(self, string, start, doActions=True):
        if col(start, string) == 1:
            return ParseResults(self, start, start, [])
        raise ParseException(self, start, string)

    def __regeex__(self):
        return "^"


class LineEnd(_PositionToken):
    """Matches if current position is at the end of a line within the
    parse string
    """

    __slots__ = []

    def __init__(self):
        with Engine(" \t") as e:
            super(LineEnd, self).__init__()
            self.set_config(
                lock_engine=e, regex=regex_compile("\\r?(\\n|$)")
            )

    def parseImpl(self, string, start, doActions=True):
        found = self.parser_config.regex.match(string, start)
        if found:
            return ParseResults(self, start, found.end(), ["\n"])
        raise ParseException(self, start, string)

    def min_length(self):
        return 0

    def __regex__(self):
        return "|", self.parser_config.regex.pattern


class StringStart(_PositionToken):
    """Matches if current position is at the beginning of the parse
    string
    """

    __slots__ = []

    def __init__(self):
        super(StringStart, self).__init__()

    def parseImpl(self, string, loc, doActions=True):
        if loc != 0:
            # see if entire string up to here is just whitespace and ignoreables
            if loc != self.engine.skip(string, 0):
                raise ParseException(self, loc, string)
        return []


class StringEnd(_PositionToken):
    """
    Matches if current position is at the end of the parse string
    """

    __slots__ = []

    def __init__(self):
        with Engine() as e:
            super(StringEnd, self).__init__()
            self.set_config(lock_engine=e)

    def parseImpl(self, string, start, doActions=True):
        end = len(string)
        if start >= end:
            return ParseResults(self, end, end, [])

        raise ParseException(self, start, string)


class WordStart(_PositionToken):
    """Matches if the current position is at the beginning of a Word,
    and is not preceded by any character in a given set of
    ``wordChars`` (default= ``printables``). To emulate the
    ``\b`` behavior of regular expressions, use
    ``WordStart(alphanums)``. ``WordStart`` will also match at
    the beginning of the string being parsed, or at the beginning of
    a line.
    """

    __slots__ = []
    Config = append_config(_PositionToken, "word_chars")

    def __init__(self, wordChars=printables):
        super(WordStart, self).__init__()
        self.set_config(
            regex=regex_compile(
                f"(?:(?<={(CharsNotIn(wordChars, exact=1)).__regex__()[1]})|^)(?={Char(wordChars).__regex__()[1]})"
            ),
            word_chars="".join(sorted(set(wordChars))),
        )
        self.streamlined = True

    def parseImpl(self, string, start, doActions=True):
        found = self.parser_config.regex.match(string, start)
        if found:
            return ParseResults(self, start, start, [])
        raise ParseException(self, start, string)

    def min_length(self):
        return 0

    def __regex__(self):
        return "+", self.parser_config.regex.pattern


class WordEnd(_PositionToken):
    """Matches if the current position is at the end of a Word, and is
    not followed by any character in a given set of ``wordChars``
    (default= ``printables``). To emulate the ``\b`` behavior of
    regular expressions, use ``WordEnd(alphanums)``. ``WordEnd``
    will also match at the end of the string being parsed, or at the end
    of a line.
    """

    __slots__ = []
    Config = append_config(_PositionToken, "word_chars")

    def __init__(self, wordChars=printables):
        super(WordEnd, self).__init__()
        self.engine = PLAIN_ENGINE
        self.set_config(
            word_chars="".join(sorted(set(wordChars))),
            regex=regex_compile(
                f"(?<={Char(wordChars).__regex__()[1]})({(~Char(wordChars)).__regex__()[1]}|$)"
            ),
        )

    def copy(self):
        output = _PositionToken.copy(self)
        output.engine = PLAIN_ENGINE
        return output

    def min_length(self):
        return 0

    def parseImpl(self, string, start, doActions=True):
        word_chars = self.parser_config.word_chars
        instrlen = len(string)
        if instrlen > 0 and start < instrlen:
            if string[start] in word_chars or string[start - 1] not in word_chars:
                raise ParseException(self, start, string)
        return ParseResults(self, start, start, [])

    def __regex__(self):
        return "+", self.parser_config.regex.pattern


# export
from mo_parsing import core, enhancement, engine, results

core.Empty = Empty
core.StringEnd = StringEnd
core.Literal = Literal
core.Token = Token

engine.Token = Token
engine.Literal = Literal
engine.CURRENT.literal = Literal

enhancement.Token = Token
enhancement.Literal = Literal
enhancement.Keyword = Keyword
enhancement.Word = Word
enhancement.CharsNotIn = CharsNotIn
enhancement._PositionToken = _PositionToken
enhancement.StringEnd = StringEnd
enhancement.Empty = Empty
enhancement.NoMatch = NoMatch
enhancement.Char = Char

results.Token = Token
results.Empty = Empty
