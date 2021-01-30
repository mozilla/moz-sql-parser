# encoding: utf-8

# UNDER THE MIT LICENSE
#
# Contact: kyle@lahnakoski.comd
from collections import OrderedDict
from string import whitespace

from mo_future import unichr, is_text

from mo_parsing.core import add_reset_action
from mo_parsing.engine import Engine, PLAIN_ENGINE
from mo_parsing.enhancement import (
    Char,
    NotAny,
    ZeroOrMore,
    OneOrMore,
    Optional,
    Many,
    Combine,
    Group,
    Forward,
    FollowedBy,
    ParseEnhancement,
)
from mo_parsing.expressions import MatchFirst, And
from mo_parsing.infix import delimitedList
from mo_parsing.results import ParseResults, Annotation
from mo_parsing.tokens import (
    Literal,
    AnyChar,
    LineStart,
    LineEnd,
    Word,
    SingleCharLiteral,
)
from mo_parsing.utils import (
    printables,
    alphas,
    alphanums,
    nums,
    hexnums,
    Log,
    listwrap,
    regex_compile,
    ParseException,
)

__all__ = ["Regex"]


def hex_to_char(t):
    return Literal(unichr(int(t.value().lower().split("x")[1], 16)))


def to_range(tokens):
    min_ = tokens["min"].parser_config.match
    max_ = tokens["max"].parser_config.match
    return Char("".join(unichr(i) for i in range(ord(min_), ord(max_) + 1)))


def to_bracket(tokens):
    acc = []
    for e in listwrap(tokens["body"].value()):
        if isinstance(e, SingleCharLiteral):
            acc.append(e.parser_config.match)
        elif isinstance(e, Char):
            acc.extend(e.parser_config.include)
        else:
            Log.error("programmer error")
    if tokens["negate"]:
        return Char(exclude=acc)
    else:
        return Char(acc)


num_captures = 0


def _reset():
    global num_captures
    num_captures = 0


add_reset_action(_reset)


def INC():
    global num_captures
    num_captures += 1


def DEC():
    global num_captures
    num_captures -= 1


def name_token(tokens):
    with PLAIN_ENGINE:
        n = tokens["name"]
        v = tokens["value"]
        if not n:
            n = str(num_captures)
        return Combine(v).set_token_name(n)


def repeat(tokens):
    if tokens.length() == 1:
        return tokens.value()

    operand, operator = tokens
    mode = operator["mode"]
    if not mode:
        if operator["exact"]:
            return Many(operand, exact=int(operator["exact"]))
        else:
            return Many(
                operand, min_match=int(operator["min"]), max_match=int(operator["max"])
            )
    elif mode in "*?":
        return ZeroOrMore(operand)
    elif mode in "+?":
        return OneOrMore(operand)
    elif mode == "?":
        return Optional(operand)
    else:
        Log.error("not expected")


PLAIN_ENGINE.use()

#########################################################################################
# SQUARE BRACKETS

any_whitechar = Literal("\\s").addParseAction(lambda: Char(whitespace))
not_whitechar = Literal("\\S").addParseAction(lambda: Char(exclude=whitespace))
any_wordchar = Literal("\\w").addParseAction(lambda: Char(alphanums + "_"))
not_wordchar = Literal("\\W").addParseAction(lambda: Char(exclude=alphanums + "_"))
any_digitchar = Literal("\\d").addParseAction(lambda: Char(nums))
not_digitchar = Literal("\\D").addParseAction(lambda: Char(exclude=nums))
bs_char = Literal("\\\\").addParseAction(lambda: Literal("\\"))
tab_char = Literal("\\t").addParseAction(lambda: Literal("\t"))
CR = Literal("\\n").addParseAction(lambda: Literal("\n"))
LF = Literal("\\r").addParseAction(lambda: Literal("\r"))
any_char = Literal(".").addParseAction(lambda: AnyChar())

macro = (
    any_whitechar
    | any_wordchar
    | any_digitchar
    | not_digitchar
    | not_wordchar
    | not_whitechar
    | CR
    | LF
    | any_char
    | bs_char
    | tab_char
)
escapedChar = (
    ~macro + Combine("\\" + AnyChar())
).addParseAction(lambda t: Literal(t.value()[1]))
plainChar = Char(exclude=r"\]").addParseAction(lambda t: Literal(t.value()))

escapedHexChar = Combine(
    (Literal("\\0x") | Literal("\\x") | Literal("\\X"))  # lookup literals is faster
    + OneOrMore(Char(hexnums))
).addParseAction(hex_to_char)

escapedOctChar = Combine(
    Literal("\\0") + OneOrMore(Char("01234567"))
).addParseAction(lambda t: Literal(unichr(int(t.value()[2:], 8))))

singleChar = escapedHexChar | escapedOctChar | escapedChar | plainChar

charRange = Group(singleChar("min") + "-" + singleChar("max")).addParseAction(to_range)

brackets = (
    "["
    + Optional("^")("negate")
    + OneOrMore(Group(charRange | singleChar | macro)("body"))
    + "]"
).addParseAction(to_bracket)

#########################################################################################
# REGEX
regex = Forward()

line_start = Literal("^").addParseAction(lambda: LineStart())
line_end = Literal("$").addParseAction(lambda: LineEnd())
word_edge = Literal("\\b").addParseAction(lambda: NotAny(any_wordchar))
simple_char = Word(
    printables, exclude=r".^$*+{}[]\|()"
).addParseAction(lambda t: Literal(t.value()))
esc_char = ("\\" + AnyChar()).addParseAction(lambda t: Literal(t.value()[1]))

with Engine():
    # ALLOW SPACES IN THE RANGE
    repetition = (
        Word(nums)("exact") + "}"
        | Word(nums)("min") + "," + Word(nums)("max") + "}"
        | Word(nums)("min") + "," + "}"
        | "," + Word(nums)("max") + "}"
    )

repetition = Group(
    "{" + repetition | (Literal("*?") | Literal("+?") | Char("*+?"))("mode")
)


LB = Char("(")

ahead = ("(?=" + regex + ")").addParseAction(lambda t: FollowedBy(t["value"]))
not_ahead = ("(?!" + regex + ")").addParseAction(lambda t: NotAny(t["value"]))
behind = ("(?<=" + regex + ")").addParseAction(lambda t: Log.error("not supported"))
not_behind = ("(?<!" + regex + ")").addParseAction(lambda t: Log.error("not supported"))
non_capture = ("(?:" + regex + ")").addParseAction(lambda t: t["value"])


named = (
    (
        Literal("(?P<").addParseAction(INC)
        + Word(alphanums + "_")("name")
        + ">"
        + regex
        + ")"
    )
    .addParseAction(name_token)
    .addParseAction(DEC)
)
group = (
    (LB.addParseAction(INC) + regex + ")")
    .addParseAction(name_token)
    .addParseAction(DEC)
)

term = (
    macro
    | simple_char
    | esc_char
    | word_edge
    | brackets
    | ahead
    | not_ahead
    | behind
    | not_behind
    | non_capture
    | named
    | group
)


more = (term + Optional(repetition)).addParseAction(repeat)
sequence = OneOrMore(more).addParseAction(lambda t: And(t))
regex << (
    delimitedList(sequence, separator="|")
    .set_token_name("value")
    .addParseAction(lambda t: MatchFirst(listwrap(t.value())).streamline())
    .streamline()
)


def srange(expr):
    pattern = brackets.parseString(expr).value()
    chars = set()

    def drill(e):
        if isinstance(e, Literal):
            chars.add(e.parser_config.match)
        elif isinstance(e, Char):
            chars.update(c for c in e.parser_config.include)
        elif isinstance(e, MatchFirst):
            for ee in e.exprs:
                drill(ee)
        elif isinstance(e, And):
            drill(e.exprs[0].expr)
        else:
            Log.error("logic error")

    drill(pattern)
    return "".join(sorted(chars))


parameters = (
    "\\" + Char(alphanums)("name") | "\\g<" + Word(alphas, alphanums)("name") + ">"
).addParseAction(lambda t: t["name"])
PLAIN_ENGINE.release()


class Regex(ParseEnhancement):
    """
    Converter to concatenate all matching tokens to a single string.
    """

    __slots__ = ["regex"]

    def __init__(self, pattern):
        """
        :param pattern:  THE REGEX PATTERN
        :param asGroupList: RETURN A LIST OF CAPTURED GROUPS /1, /2, /3, ...
        """
        parsed = regex.parseString(pattern)
        ParseEnhancement.__init__(self, parsed.value().streamline())
        # WE ASSUME IT IS SAFE TO ASSIGN regex (NO SERIOUS BACKTRACKING PROBLEMS)
        self.streamlined = True
        self.regex = regex_compile(pattern)

    def copy(self):
        output = ParseEnhancement.copy(self)
        output.regex = self.regex
        return output

    def capture_groups(self):
        """
        ADD A SPECIAL PARSE ACTION TO PROVIDE THE NUMBERED (eg "1") AND
        NAMED GROUPS (eg (?P<name>...)
        """

        def group_list(tokens):
            start, end = tokens.start, tokens.end
            # RE-MATCH  :(
            sub_string = tokens.tokens[0]
            found = tokens.type.regex.match(sub_string)
            lookup = dict(reversed(p) for p in found.re.groupindex.items())
            ann = []
            pe = 0
            for i, (s, e) in enumerate(found.regs[1:], start=1):
                if s == -1:
                    continue  # NOT FOUND
                g = sub_string[s:e]
                if pe <= s:
                    ann.append(g)
                pe = e
                ii = chr(i + ord("0"))
                ann.append(Annotation(ii, s + start, e + start, [g]))
                n = lookup.get(i)
                if n:
                    ann.append(Annotation(n, s + start, e + start, [g]))
            return ParseResults(_plain_group, start, end, ann)

        return self.addParseAction(group_list)

    def sub(self, replacement):
        # MIMIC re.sub
        if is_text(replacement):
            # USE PYTHON REGEX
            def pa(tokens):
                return tokens.type.regex.sub(replacement, tokens[0])

            output = self.addParseAction(pa)
            return output
        else:
            # A FUNCTION
            def pf(tokens):
                regex_result = tokens.type.regex.match(tokens.tokens[0])
                return replacement(regex_result)

            return self.addParseAction(pf)

    def parseImpl(self, string, start, doActions=True):
        found = self.regex.match(string, start)
        if found:
            return ParseResults(self, start, found.end(), [found[0]])
        else:
            raise ParseException(self, start, string)

    def streamline(self):
        # WE RUN THE DANGER OF MAKING PATHELOGICAL REGEX, SO WE DO NOT TRY
        if self.streamlined:
            return self

        expr = self.expr.streamline()
        if expr is self:
            self.streamlined = True
            return self
        output = self.copy()
        output.expr = expr
        output.streamlined = True
        return output

    def expecting(self):
        return OrderedDict((k, [self]) for k in self.expr.expecting().keys())

    def min_length(self):
        return self.expr.min_length()

    def __regex__(self):
        if self.regex:
            return "|", self.regex.pattern
        else:
            return self.expr.__regex__()


_plain_group = Group(None)


from mo_parsing import core

core.regex_parameters = parameters
del core
