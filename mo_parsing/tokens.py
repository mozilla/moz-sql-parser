# encoding: utf-8
import re
from collections import OrderedDict

from mo_future import text, is_text

from mo_parsing.core import ParserElement
from mo_parsing.engine import Engine, PLAIN_ENGINE
from mo_parsing.exceptions import ParseException
from mo_parsing.results import ParseResults
from mo_parsing.utils import *


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
        if not is_text(match):
            Log.error("Expecting string for literal")
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

    def expecting(self):
        return {self.parser_config.match.lower(): [self]}

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
            ident_chars=ident_chars,
            match=match,
            regex=regex_compile(pattern + non_word),
        )

        self.parser_name = match
        if caseless:
            self.__class__ = CaselessKeyword

    def parseImpl(self, string, start, doActions=True):
        found = self.parser_config.regex.match(string, start)
        if found:
            return ParseResults(self, start, found.end(), [self.parser_config.match])
        raise ParseException(self, start, string)

    def expecting(self):
        return {self.parser_config.match.lower(): [self]}

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
            match=match, regex=regex_compile(regex_caseless(match)),
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
    __slots__ = ["regex"]
    Config = append_config(Token, "min", "init_chars")

    def __init__(
        self,
        init_chars,
        body_chars=None,
        min=1,
        max=None,
        exact=0,
        as_keyword=False,  # IF WE EXPECT NON-WORD CHARACTERS BEFORE AND AFTER
        exclude="",
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
            prec, regexp = Char(init_chars, exclude=exclude)[min:max].__regex__()
        elif max is None or max == MAX_INT:
            prec, regexp = (
                Char(init_chars, exclude=exclude)
                + Char(body_chars, exclude=exclude)[min - 1 :]
            ).__regex__()
        else:
            prec, regexp = (
                Char(init_chars, exclude=exclude)
                + Char(body_chars, exclude=exclude)[min - 1 : max - 1]
            ).__regex__()

        if as_keyword:
            regexp = r"\b" + regexp + r"\b"

        self.regex = regex_compile(regexp)
        self.set_config(min=min, init_chars=init_chars)

    def copy(self):
        output = Token.copy(self)
        output.regex = self.regex
        return output

    def parseImpl(self, string, start, doActions=True):
        found = self.regex.match(string, start)
        if found:
            return ParseResults(self, start, found.end(), [found.group()])
        else:
            raise ParseException(self, start, string)

    def min_length(self):
        return self.parser_config.min

    # def expecting(self):
    #     return OrderedDict((c, [self]) for c in self.parser_config.init_chars)

    def __regex__(self):
        return "+", self.regex.pattern

    def __str__(self):
        if self.parser_name:
            return self.parser_name
        return f"W:({self.regex.pattern})"


class Char(Token):
    __slots__ = []
    Config = append_config(Token, "include", "exclude")

    def __init__(self, include="", asKeyword=False, exclude=""):
        """
        Represent one character in a given include
        """
        Token.__init__(self)
        include = set(include) if include else set()
        exclude = set(exclude) if exclude else set()
        include = "".join(sorted(include - exclude))
        exclude = "".join(sorted(exclude))

        if not include:
            regex = regex_range(exclude, exclude=True)
        else:
            regex = regex_range(include)

        if asKeyword:
            regex = r"\b%s\b" % self
        self.set_config(regex=regex_compile(regex), include=include, exclude=exclude)

    def parseImpl(self, string, start, doActions=True):
        found = self.parser_config.regex.match(string, start)
        if found:
            return ParseResults(self, start, found.end(), [found.group()])

        raise ParseException(self, start, string)

    def expecting(self):
        return {c: [self] for c in self.parser_config.include}

    def min_length(self):
        return 1

    def __regex__(self):
        return "*", self.parser_config.regex.pattern

    def __str__(self):
        return self.parser_config.regex.pattern


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
            self.set_config(lock_engine=e, regex=regex_compile("\\r?(\\n|$)"))

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
engine.PLAIN_ENGINE.literal = Literal

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
