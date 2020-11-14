# encoding: utf-8
import re
import warnings
from collections import Iterable
from datetime import datetime

from mo_dots import listwrap
from mo_future import text

from mo_parsing.core import add_reset_action
from mo_parsing.engine import Engine
from mo_parsing.enhancement import (
    Combine,
    Dict,
    Forward,
    Group,
    OneOrMore,
    Optional,
    Suppress,
    TokenConverter,
    ZeroOrMore,
    OpenDict,
    Many,
)
from mo_parsing.exceptions import ParseException
from mo_parsing.results import ParseResults, Annotation, NO_PARSER
from mo_parsing.tokens import (
    CaselessKeyword,
    CaselessLiteral,
    CharsNotIn,
    Empty,
    Keyword,
    LineEnd,
    NoMatch,
    Regex,
    Word,
    Literal,
)
from mo_parsing.utils import (
    alphanums,
    alphas,
    col,
    hexnums,
    nums,
    printables,
    unichr,
    wrap_parse_action,
)
from mo_parsing.utils import regex_range

# import later
And, Or, MatchFirst = [None] * 3


dblQuotedString = Combine(
    Regex(r'"(?:[^"\n\r\\]|(?:"")|(?:\\(?:[^x]|x[0-9a-fA-F]+)))*') + '"'
).set_parser_name("string enclosed in double quotes")
sglQuotedString = Combine(
    Regex(r"'(?:[^'\n\r\\]|(?:'')|(?:\\(?:[^x]|x[0-9a-fA-F]+)))*") + "'"
).set_parser_name("string enclosed in single quotes")
quotedString = Combine(
    Regex(r'"(?:[^"\n\r\\]|(?:"")|(?:\\(?:[^x]|x[0-9a-fA-F]+)))*') + '"'
    | Regex(r"'(?:[^'\n\r\\]|(?:'')|(?:\\(?:[^x]|x[0-9a-fA-F]+)))*") + "'"
).set_parser_name("quotedString using single or double quotes")
unicodeString = Combine(
    Literal("u") + quotedString
).set_parser_name("unicode string literal")


def delimitedList(expr, separator=",", combine=False):
    """
    PARSE DELIMITED LIST OF expr
    Example::

        delimitedList(Word(alphas)).parseString("aa,bb,cc") # -> ['aa', 'bb', 'cc']
        delimitedList(Word(hexnums), delim=':', combine=True).parseString("AA:BB:CC:DD:EE") # -> ['AA:BB:CC:DD:EE']
    """
    if combine:
        return Combine(expr + ZeroOrMore(separator + expr))
    else:
        return expr + ZeroOrMore(Suppress(separator) + expr)


def countedArray(expr, intExpr=None):
    """Helper to define a counted list of expressions.

    This helper defines a pattern of the form::

        integer expr expr expr...

    where the leading integer tells how many expr expressions follow.
    The matched tokens returns the array of expr tokens as a list - the
    leading count token is suppressed.

    If ``intExpr`` is specified, it should be a mo_parsing expression
    that produces an integer value.

    Example::

        countedArray(Word(alphas)).parseString('2 ab cd ef')  # -> ['ab', 'cd']

        # in this parser, the leading integer value is given in binary,
        # '10' indicating that 2 values are in the array
        binaryConstant = Word('01').addParseAction(lambda t: int(t[0], 2))
        countedArray(Word(alphas), intExpr=binaryConstant).parseString('10 ab cd ef')  # -> ['ab', 'cd']
    """
    if intExpr is None:
        intExpr = Word(nums).addParseAction(lambda t: int(t[0]))

    arrayExpr = Forward()

    def countFieldParseAction(t, l, s):
        n = t[0]
        arrayExpr << Group(Many(expr, exact=n))
        return []

    intExpr = (
        intExpr
        .set_parser_name("arrayLen")
        .addParseAction(countFieldParseAction, callDuringTry=True)
    )
    return (intExpr + arrayExpr).set_parser_name("(len) " + text(expr) + "...")


def _flatten(L):
    ret = []
    for i in L:
        if isinstance(i, list):
            ret.extend(_flatten(i))
        else:
            ret.append(i)
    return ret


def matchPreviousLiteral(expr):
    """Helper to define an expression that is indirectly defined from
    the tokens matched in a previous expression, that is, it looks for
    a 'repeat' of a previous expression.  For example::

        first = Word(nums)
        second = matchPreviousLiteral(first)
        matchExpr = first + ":" + second

    will match ``"1:1"``, but not ``"1:2"``.  Because this
    matches a previous literal, will also match the leading
    ``"1:1"`` in ``"1:10"``. If this is not desired, use
    `matchPreviousExpr`. Do *not* use with packrat parsing
    enabled.
    """
    rep = Forward()

    def copyTokenToRepeater(t, l, s):
        if t:
            if len(t) == 1:
                rep << t[0]
            else:
                # flatten t tokens
                tflat = _flatten(t)
                rep << And(Literal(tt) for tt in tflat)
        else:
            rep << Empty()

    expr.addParseAction(copyTokenToRepeater, callDuringTry=True)
    rep.set_parser_name("(prev) " + text(expr))
    return rep


def matchPreviousExpr(expr):
    """Helper to define an expression that is indirectly defined from
    the tokens matched in a previous expression, that is, it looks for
    a 'repeat' of a previous expression.  For example::

        first = Word(nums)
        second = matchPreviousExpr(first)
        matchExpr = first + ":" + second

    will match ``"1:1"``, but not ``"1:2"``.  Because this
    matches by expressions, will *not* match the leading ``"1:1"``
    in ``"1:10"``; the expressions are evaluated first, and then
    compared, so ``"1"`` is compared with ``"10"``. Do *not* use
    with packrat parsing enabled.
    """
    rep = Forward()
    e2 = expr.copy()
    rep <<= e2

    def copyTokenToRepeater(t, l, s):
        matchTokens = _flatten(t)

        def mustMatchTheseTokens(t, l, s):
            theseTokens = _flatten(t)
            if theseTokens != matchTokens:
                raise ParseException("", 0, "")

        rep.addParseAction(mustMatchTheseTokens, callDuringTry=True)

    expr.addParseAction(copyTokenToRepeater, callDuringTry=True)
    rep.set_parser_name("(prev) " + text(expr))
    return rep


def oneOf(strs, caseless=False, asKeyword=False):
    """Helper to quickly define a set of alternative Literals, and makes
    sure to do longest-first testing when there is a conflict,
    regardless of the input order, but returns
    a `MatchFirst` for best performance.

    Parameters:

     - strs - a string of space-delimited literals, or a collection of
       string literals
     - caseless - (default= ``False``) - treat all literals as caseless
     - asKeyword - (default=``False``) - enforce Keyword-style matching on the
       generated expressions
    """
    if isinstance(caseless, text):
        warnings.warn(
            "More than one string argument passed to oneOf, pass "
            "choices as a list or space-delimited string",
            stacklevel=2,
        )

    if caseless:
        isequal = lambda a, b: a.upper() == b.upper()
        masks = lambda a, b: b.upper().startswith(a.upper())
        parseElementClass = CaselessKeyword if asKeyword else CaselessLiteral
    else:
        isequal = lambda a, b: a == b
        masks = lambda a, b: b.startswith(a)
        parseElementClass = Keyword if asKeyword else Literal

    symbols = []
    if isinstance(strs, text):
        symbols = strs.split()
    elif isinstance(strs, Iterable):
        symbols = list(strs)
    else:
        warnings.warn(
            "Invalid argument to oneOf, expected string or iterable",
            SyntaxWarning,
            stacklevel=2,
        )
    if not symbols:
        return NoMatch()

    if not asKeyword:
        # if not producing keywords, need to reorder to take care to avoid masking
        # longer choices with shorter ones
        i = 0
        while i < len(symbols) - 1:
            cur = symbols[i]
            for j, other in enumerate(symbols[i + 1 :]):
                if isequal(other, cur):
                    del symbols[i + j + 1]
                    break
                elif masks(cur, other):
                    del symbols[i + j + 1]
                    symbols.insert(i, other)
                    break
            else:
                i += 1

    if caseless or asKeyword:
        return (
            MatchFirst(parseElementClass(sym) for sym in symbols)
            .set_parser_name(" | ".join(symbols))
            .streamline()
        )

    # CONVERT INTO REGEX
    singles = [s for s in symbols if len(s) == 1]
    rest = list(sorted([s for s in symbols if len(s) != 1], key=lambda s: -len(s)))

    acc = []
    acc.extend(re.escape(sym) for sym in rest)
    if singles:
        acc.append(regex_range("".join(singles)))
    regex = "|".join(acc)

    return Regex(regex).set_parser_name(" | ".join(symbols)).streamline()


def dictOf(key, value):
    """Helper to easily and clearly define a dictionary by specifying
    the respective patterns for the key and value.  Takes care of
    defining the `Dict`, `ZeroOrMore`, and
    `Group` tokens in the proper order.  The key pattern
    can include delimiting markers or punctuation, as long as they are
    suppressed, thereby leaving the significant key text.  The value
    pattern can include named results, so that the `Dict` results
    can include named token fields.

    Example::

        text = "shape: SQUARE posn: upper left color: light blue texture: burlap"
        attr_expr = (label + Suppress(':') + OneOrMore(data_word, stopOn=label).addParseAction(' '.join))
        print(OneOrMore(attr_expr).parseString(text))

        attr_label = label
        attr_value = Suppress(':') + OneOrMore(data_word, stopOn=label).addParseAction(' '.join)

        # similar to Dict, but simpler call format
        result = dictOf(attr_label, attr_value).parseString(text)
        print(result)
        print(result['shape'])
        print(result.shape)  # object attribute access works too
        print(result)

    prints::

        [['shape', 'SQUARE'], ['posn', 'upper left'], ['color', 'light blue'], ['texture', 'burlap']]
        - color: light blue
        - posn: upper left
        - shape: SQUARE
        - texture: burlap
        SQUARE
        SQUARE
        {'color': 'light blue', 'shape': 'SQUARE', 'posn': 'upper left', 'texture': 'burlap'}
    """
    return Dict(OneOrMore(Group(key + value)))


def originalTextFor(expr, asString=True):
    """Helper to return the original, untokenized text for a given
    expression.  Useful to restore the parsed fields of an HTML start
    tag into the raw tag text itself, or to revert separate tokens with
    intervening whitespace back to the original matching input text. By
    default, returns astring containing the original parsed text.

    If the optional ``asString`` argument is passed as
    ``False``, then the return value is
    a `ParseResults` containing any results names that
    were originally matched, and a single token containing the original
    matched text from the input string.  So if the expression passed to
    `originalTextFor` contains expressions with defined
    results names, you must set ``asString`` to ``False`` if you
    want to preserve those results name values.

    Example::

        src = "this is test <b> bold <i>text</i> </b> normal text "
        for tag in ("b", "i"):
            opener, closer = makeHTMLTags(tag)
            patt = originalTextFor(opener + SkipTo(closer) + closer)
            print(patt.searchString(src)[0])

    prints::

        ['<b> bold <i>text</i> </b>']
        ['<i>text</i>']
    """
    locMarker = Empty().addParseAction(lambda t, l, s: l)
    matchExpr = locMarker("_original_start") + Group(expr) + locMarker("_original_end")
    matchExpr = matchExpr.addParseAction(extractText)
    return matchExpr


def extractText(tokens, loc, string):
    start, d, end = tokens
    content = string[start:end]
    annotations = [Annotation(k, v[0].start, v[-1].end, v) for k, v in d.items()]
    return ParseResults(d.type, start, end, [content] + annotations)


def ungroup(expr):
    """Helper to undo mo_parsing's default grouping of And expressions,
    even if all but one are non-empty.
    """
    return TokenConverter(expr).addParseAction(lambda t: t[0])


def locatedExpr(expr):
    """Helper to decorate a returned token with its starting and ending
    locations in the input string.

    This helper adds the following results names:

     - locn_start = location where matched expression begins
     - locn_end = location where matched expression ends
     - value = the actual parsed results

    Be careful if the input text contains ``<TAB>`` characters, you
    may want to call `ParserElement.parseWithTabs`

    Example::

        wd = Word(alphas)
        for match in locatedExpr(wd).searchString("ljsdf123lksdjjf123lkkjj1222"):
            print(match)

    prints::

        [[0, 'ljsdf', 5]]
        [[8, 'lksdjjf', 15]]
        [[18, 'lkkjj', 23]]
    """
    locator = Empty().addParseAction(lambda t, l, s: l)
    return Group(locator("locn_start") + Group(expr)("value") + locator("locn_end"))


def nestedExpr(opener="(", closer=")", content=None, ignoreExpr=quotedString):
    """Helper method for defining nested lists enclosed in opening and
    closing delimiters ("(" and ")" are the default).

    Parameters:
     - opener - opening character for a nested list
       (default= ``"("``); can also be a mo_parsing expression
     - closer - closing character for a nested list
       (default= ``")"``); can also be a mo_parsing expression
     - content - expression for items within the nested lists
       (default= ``None``)
     - ignoreExpr - expression for ignoring opening and closing
       delimiters (default= `quotedString`)

    If an expression is not provided for the content argument, the
    nested expression will capture all whitespace-delimited content
    between delimiters as a list of separate values.

    Use the ``ignoreExpr`` argument to define expressions that may
    contain opening or closing characters that should not be treated as
    opening or closing characters for nesting, such as quotedString or
    a comment expression.  Specify multiple expressions using an
    `Or` or `MatchFirst`. The default is
    `quotedString`, but if no expressions are to be ignored, then
    pass ``None`` for this argument.

    """
    if opener == closer:
        raise ValueError("opening and closing strings cannot be the same")
    if content is None:
        if not isinstance(opener, text) or not isinstance(closer, text):
            raise ValueError(
                "opening and closing arguments must be strings if no content expression"
                " is given"
            )

        ignore_chars = engine.CURRENT.white_chars
        with Engine(""):

            def scrub(t):
                return t[0].strip()

            if len(opener) == 1 and len(closer) == 1:
                if ignoreExpr is not None:
                    content = Combine(OneOrMore(
                        ~ignoreExpr
                        + CharsNotIn(opener + closer + "".join(ignore_chars), exact=1,)
                    )).addParseAction(scrub)
                else:
                    content = Empty + CharsNotIn(
                        opener + closer + "".join(ignore_chars)
                    ).addParseAction(scrub)
            else:
                if ignoreExpr is not None:
                    content = Combine(OneOrMore(
                        ~ignoreExpr
                        + ~Literal(opener)
                        + ~Literal(closer)
                        + CharsNotIn(ignore_chars, exact=1)
                    )).addParseAction(scrub)
                else:
                    content = Combine(OneOrMore(
                        ~Literal(opener)
                        + ~Literal(closer)
                        + CharsNotIn(ignore_chars, exact=1)
                    )).addParseAction(scrub)
    ret = Forward()
    if ignoreExpr is not None:
        ret <<= Group(
            Suppress(opener) + ZeroOrMore(ignoreExpr | ret | content) + Suppress(closer)
        )
    else:
        ret <<= Group(Suppress(opener) + ZeroOrMore(ret | content) + Suppress(closer))
    ret.set_parser_name("nested %s%s expression" % (opener, closer))
    return ret


with Engine(""):
    _escapedPunc = Word(
        "\\", r"\[]-*.$+^?()~ ", exact=2
    ).addParseAction(lambda t, l, s: t[0][1])
    _escapedHexChar = Regex(r"\\0?[xX][0-9a-fA-F]+").addParseAction(lambda t: unichr(int(
        t[0].lstrip("\\").lstrip("0").lstrip("xX"), 16
    )))
    _escapedOctChar = Regex(r"\\0[0-7]+").addParseAction(lambda t, l, s: unichr(int(
        t[0][1:], 8
    )))
    _singleChar = (
        _escapedPunc | _escapedHexChar | _escapedOctChar | CharsNotIn(r"\]", exact=1)
    )
    _charRange = Group(_singleChar + Suppress("-") + _singleChar)
    _reBracketExpr = (
        "["
        + Optional("^").set_token_name("negate")
        + Group(OneOrMore(_charRange | _singleChar)).set_token_name("body")
        + "]"
    )


def srange(s):
    r"""Helper to easily define string ranges for use in Word
    construction. Borrows syntax from regexp '[]' string range
    definitions::

        srange("[0-9]")   -> "0123456789"
        srange("[a-z]")   -> "abcdefghijklmnopqrstuvwxyz"
        srange("[a-z$_]") -> "abcdefghijklmnopqrstuvwxyz$_"

    The input string must be enclosed in []'s, and the returned string
    is the expanded character set joined into a single string. The
    values enclosed in the []'s may be:

     - a single character
     - an escaped character with a leading backslash (such as ``\-``
       or ``\]``)
     - an escaped hex character with a leading ``'\x'``
       (``\x21``, which is a ``'!'`` character) (``\0x##``
       is also supported for backwards compatibility)
     - an escaped octal character with a leading ``'\0'``
       (``\041``, which is a ``'!'`` character)
     - a range of any of the above, separated by a dash (``'a-z'``,
       etc.)
     - any combination of the above (``'aeiouy'``,
       ``'a-zA-Z0-9_$'``, etc.)
    """

    def _expanded(p):
        if not isinstance(p, ParseResults):
            return p
        else:
            return "".join(unichr(c) for c in range(ord(p[0]), ord(p[1]) + 1))

    try:
        return "".join(
            _expanded(part) for part in _reBracketExpr.parseString(s)["body"]
        )
    except Exception as cause:
        return ""


def matchOnlyAtCol(n):
    """Helper method for defining parse actions that require matching at
    a specific column in the input text.
    """

    def verifyCol(strg, locn, toks):
        if col(locn, strg) != n:
            raise ParseException(strg, locn, "matched token not at column %d" % n)

    return verifyCol


def replaceWith(value):
    """Helper method for common parse actions that simply return
    a literal value.  Especially useful when used with
    `transformString<ParserElement.transformString>` ().
    """

    def replacer():
        return [value]

    return replacer


def removeQuotes(t, l, s):
    """Helper parse action for removing quotation marks from parsed
    quoted strings.

    Example::

        # by default, quotation marks are included in parsed results
        quotedString.parseString("'Now is the Winter of our Discontent'") # -> ["'Now is the Winter of our Discontent'"]

        # use removeQuotes to strip quotation marks from parsed results
        quotedString.addParseAction(removeQuotes)
        quotedString.parseString("'Now is the Winter of our Discontent'") # -> ["Now is the Winter of our Discontent"]
    """
    return t[0][1:-1]


def tokenMap(func, *args):
    """
    APPLY func OVER ALL GIVEN TOKENS
    :param func: ACCEPT ParseResults
    :param args: ADDITIONAL PARAMETERS TO func
    :return:  map(func(e), token)
    """

    def pa(t, l, s):
        return [func(tokn, *args) for tokn in t]

    try:
        func_name = getattr(func, "__name__", getattr(func, "__class__").__name__)
    except Exception:
        func_name = str(func)
    pa.__name__ = func_name

    return pa


upcaseTokens = tokenMap(lambda t: text(t).upper())
"""(Deprecated) Helper parse action to convert tokens to upper case.
Deprecated in favor of `upcaseTokens`"""

downcaseTokens = tokenMap(lambda t: text(t).lower())
"""(Deprecated) Helper parse action to convert tokens to lower case.
Deprecated in favor of `downcaseTokens`"""


def makeHTMLTags(tagStr, suppress_LT=Suppress("<"), suppress_GT=Suppress(">")):
    """Helper to construct opening and closing tag expressions for HTML,
    given a tag name. Matches tags in either upper or lower case,
    attributes with namespaces and with quoted or unquoted values.
    """
    if isinstance(tagStr, text):
        resname = tagStr
        tagStr = Keyword(tagStr, caseless=True)
    else:
        resname = tagStr.parser_name

    tagAttrName = Word(alphas, alphanums + "_-:")
    tagAttrValue = quotedString.addParseAction(removeQuotes) | Word(
        printables, excludeChars=">"
    )
    simpler_name = "".join(resname.replace(":", " ").title().split())

    openTag = (
        (
            suppress_LT
            + tagStr("tag")
            + OpenDict(ZeroOrMore(Group(
                tagAttrName.addParseAction(downcaseTokens)
                + Optional(Suppress("=") + tagAttrValue)
            )))
            + Optional(
                "/", default=[False]
            )("empty").addParseAction(lambda t, l, s: t[0] == "/")
            + suppress_GT
        )
        .set_token_name("start" + simpler_name)
        .set_parser_name("<%s>" % resname)
    )

    closeTag = (
        Combine(Literal("</") + tagStr + ">")
        .set_token_name("end" + simpler_name)
        .set_parser_name("</%s>" % resname)
    )

    # openTag.tag = resname
    # closeTag.tag = resname
    # openTag.tag_body = SkipTo(closeTag)

    return openTag, closeTag


def withAttribute(*args, **attrDict):
    """Helper to create a validating parse action to be used with start
    tags created with `makeXMLTags` or
    `makeHTMLTags`. Use ``withAttribute`` to qualify
    a starting tag with a required attribute value, to avoid false
    matches on common tags such as ``<TD>`` or ``<DIV>``.

    Call ``withAttribute`` with a series of attribute names and
    values. Specify the list of filter attributes names and values as:

     - keyword arguments, as in ``(align="right")``, or
     - as an explicit dict with ``**`` operator, when an attribute
       name is also a Python reserved word, as in ``**{"class":"Customer", "align":"right"}``
     - a list of name-value tuples, as in ``(("ns1:class", "Customer"), ("ns2:align", "right"))``

    For attribute names with a namespace prefix, you must use the second
    form.  Attribute names are matched insensitive to upper/lower case.

    If just testing for ``class`` (with or without a namespace), use
    `withClass`.

    To verify that the attribute exists, but without specifying a value,
    pass ``withAttribute.ANY_VALUE`` as the value.

    Example::

        html = '''
            <div>
            Some text
            <div type="grid">1 4 0 1 0</div>
            <div type="graph">1,3 2,3 1,1</div>
            <div>this has no type</div>
            </div>

        '''
        div,div_end = makeHTMLTags("div")

        # only match div tag having a type attribute with value "grid"
        div_grid = div().addParseAction(withAttribute(type="grid"))
        grid_expr = div_grid + SkipTo(div | div_end)("body")
        for grid_header in grid_expr.searchString(html):
            print(grid_header.body)

        # construct a match with any div tag having a type attribute, regardless of the value
        div_any_type = div().addParseAction(withAttribute(type=withAttribute.ANY_VALUE))
        div_expr = div_any_type + SkipTo(div | div_end)("body")
        for div_header in div_expr.searchString(html):
            print(div_header.body)

    prints::

        1 4 0 1 0

        1 4 0 1 0
        1,3 2,3 1,1
    """
    if args:
        attrs = args[:]
    else:
        attrs = attrDict.items()
    attrs = [(k, v) for k, v in attrs]

    def pa(tokens, loc, string):
        for attrName, attrValue in attrs:
            if attrName not in tokens:
                raise ParseException(tokens.type, loc, string, "no matching attribute ")
            if attrValue != withAttribute.ANY_VALUE and tokens[attrName] != attrValue:
                raise ParseException(
                    tokens.type,
                    loc,
                    string,
                    f"attribute '{attrName}' has value '{tokens[attrName]}', must be"
                    f" '{attrValue}'",
                )

    return pa


withAttribute.ANY_VALUE = object()


def withClass(classname, namespace=""):
    """Simplified version of `withAttribute` when
    matching on a div class - made difficult because ``class`` is
    a reserved word in Python.

    Example::

        html = '''
            <div>
            Some text
            <div class="grid">1 4 0 1 0</div>
            <div class="graph">1,3 2,3 1,1</div>
            <div>this &lt;div&gt; has no class</div>
            </div>

        '''
        div,div_end = makeHTMLTags("div")
        div_grid = div().addParseAction(withClass("grid"))

        grid_expr = div_grid + SkipTo(div | div_end)("body")
        for grid_header in grid_expr.searchString(html):
            print(grid_header.body)

        div_any_type = div().addParseAction(withClass(withAttribute.ANY_VALUE))
        div_expr = div_any_type + SkipTo(div | div_end)("body")
        for div_header in div_expr.searchString(html):
            print(div_header.body)

    prints::

        1 4 0 1 0

        1 4 0 1 0
        1,3 2,3 1,1
    """
    classattr = "%s:class" % namespace if namespace else "class"
    return withAttribute(**{classattr: classname})


LEFT_ASSOC = object()
RIGHT_ASSOC = object()


def infixNotation(baseExpr, spec, lpar=Suppress("("), rpar=Suppress(")")):
    """
    :param baseExpr: expression representing the most basic element for the
       nested
    :param spec: list of tuples, one for each operator precedence level
       in the expression grammar; each tuple is of the form ``(opExpr,
       numTerms, rightLeftAssoc, parseAction)``, where:

       - opExpr is the mo_parsing expression for the operator; may also
         be a string, which will be converted to a Literal; if numTerms
         is 3, opExpr is a tuple of two expressions, for the two
         operators separating the 3 terms
       - numTerms is the number of terms for this operator (must be 1,
         2, or 3)
       - rightLeftAssoc is the indicator whether the operator is right
         or left associative, using the mo_parsing-defined constants
         ``RIGHT_ASSOC`` and ``LEFT_ASSOC``.
       - parseAction is the parse action to be associated with
         expressions matching this operator expression (the parse action
         tuple member may be omitted); if the parse action is passed
         a tuple or list of functions, this is equivalent to calling
         ``setParseAction(*fn)``
         (`ParserElement.addParseAction`)
    :param lpar: expression for matching left-parentheses
       (default= ``Suppress('(')``)
    :param rpar: expression for matching right-parentheses
       (default= ``Suppress(')')``)
    :return: ParserElement
    """

    all_op = {}

    def norm(op):
        output = all_op.get(id(op))
        if output:
            return output

        def record_self(tok):
            ParseResults(tok.type, tok.start, tok.end, [tok.type.parser_name])

        output = engine.CURRENT.normalize(op)
        is_suppressed = isinstance(output, Suppress)
        if is_suppressed:
            output = output.expr
        output = output.addParseAction(record_self)
        all_op[id(op)] = is_suppressed, output
        return is_suppressed, output

    opList = []
    """
    SCRUBBED LIST OF OPERATORS
    * expr - used exclusively for ParseResult(expr, [...]), not used to match
    * op - used to match 
    * arity - same
    * assoc - same
    * parse_actions - same
    """

    for operDef in spec:
        op, arity, assoc, rest = operDef[0], operDef[1], operDef[2], operDef[3:]
        parse_actions = list(map(wrap_parse_action, listwrap(rest[0]))) if rest else []
        if arity == 1:
            is_suppressed, op = norm(op)
            if assoc == RIGHT_ASSOC:
                opList.append((
                    Group(baseExpr + op),
                    op,
                    is_suppressed,
                    arity,
                    assoc,
                    parse_actions,
                ))
            else:
                opList.append((
                    Group(op + baseExpr),
                    op,
                    is_suppressed,
                    arity,
                    assoc,
                    parse_actions,
                ))
        elif arity == 2:
            is_suppressed, op = norm(op)
            opList.append((
                Group(baseExpr + op + baseExpr),
                op,
                is_suppressed,
                arity,
                assoc,
                parse_actions,
            ))
        elif arity == 3:
            is_suppressed, op = zip(norm(op[0]), norm(op[1]))
            opList.append((
                Group(baseExpr + op[0] + baseExpr + op[1] + baseExpr),
                op,
                is_suppressed,
                arity,
                assoc,
                parse_actions,
            ))
    opList = tuple(opList)

    def record_op(op):
        def output(tokens):
            return ParseResults(NO_PARSER, tokens.start, tokens.end, [(tokens, op)])

        return output

    prefix_ops = MatchFirst([
        op.addParseAction(record_op(op))
        for expr, op, is_suppressed, arity, assoc, pa in opList
        if arity == 1 and assoc == RIGHT_ASSOC
    ])
    suffix_ops = MatchFirst([
        op.addParseAction(record_op(op))
        for expr, op, is_suppressed, arity, assoc, pa in opList
        if arity == 1 and assoc == LEFT_ASSOC
    ])
    ops = Or([
        opPart.addParseAction(record_op(opPart))
        for expr, op, is_suppressed, arity, assoc, pa in opList
        if arity > 1
        for opPart in (op if isinstance(op, tuple) else [op])
    ])

    def make_tree(tokens, loc, string):
        flat_tokens = list(tokens)
        num = len(opList)
        op_index = 0
        while len(flat_tokens) > 1 and op_index < num:
            expr, op, is_suppressed, arity, assoc, parse_actions = opList[op_index]
            if arity == 1:
                if assoc == RIGHT_ASSOC:
                    # PREFIX OPERATOR -3
                    todo = list(reversed(list(enumerate(flat_tokens[:-1]))))
                    for i, (r, o) in todo:
                        if o == op:
                            tok = flat_tokens[i + 1][0]
                            if is_suppressed:
                                result = ParseResults(expr, tok.start, tok.end, (tok,))
                            else:
                                result = ParseResults(expr, r.start, tok.end, (r, tok))
                            break
                    else:
                        op_index += 1
                        continue
                else:
                    # SUFFIX OPERATOR 3!
                    todo = list(enumerate(flat_tokens[1:]))
                    for i, (r, o) in todo:
                        if o == op:
                            tok = flat_tokens[i][0]
                            if is_suppressed:
                                result = ParseResults(expr, tok.start, tok.end, (tok,))
                            else:
                                result = ParseResults(expr, tok.start, r.end, (tok, r,))
                            break
                    else:
                        op_index += 1
                        continue
            elif arity == 2:
                todo = list(enumerate(flat_tokens[1:-1]))
                if assoc == RIGHT_ASSOC:
                    todo = list(reversed(todo))

                for i, (r, o) in todo:
                    if o == op:
                        if is_suppressed:
                            result = ParseResults(
                                expr,
                                flat_tokens[i][0].start,
                                flat_tokens[i + 2][0].end,
                                (flat_tokens[i][0], flat_tokens[i + 2][0]),
                            )
                        else:
                            result = ParseResults(
                                expr,
                                flat_tokens[i][0].start,
                                flat_tokens[i + 2][0].end,
                                (flat_tokens[i][0], r, flat_tokens[i + 2][0]),
                            )
                        break
                else:
                    op_index += 1
                    continue

            else:  # arity==3
                todo = list(enumerate(flat_tokens[1:-3]))
                if assoc == RIGHT_ASSOC:
                    todo = list(reversed(todo))

                for i, (r0, o0) in todo:
                    if o0 == op[0]:
                        r1, o1 = flat_tokens[i + 3]
                        if o1 == op[1]:
                            seq = [
                                flat_tokens[i][0],
                                flat_tokens[i + 2][0],
                                flat_tokens[i + 4][0],
                            ]
                            s0, s1 = is_suppressed
                            if not s1:
                                seq.insert(2, r1)
                            if not s0:
                                seq.insert(1, r0)

                            result = ParseResults(expr, seq[0].start, seq[-1].end, seq)
                            break
                else:
                    op_index += 1
                    continue

            for p in parse_actions:
                result = p(result, -1, string)
            offset = (0, 2, 3, 5)[arity]
            flat_tokens[i : i + offset] = [(result, (expr,))]
            op_index = 0

        return flat_tokens[0][0]

    flat = Forward()
    iso = lpar.suppress() + flat + rpar.suppress()
    atom = (baseExpr | iso).addParseAction(record_op(baseExpr))
    modified = ZeroOrMore(prefix_ops) + atom + ZeroOrMore(suffix_ops)
    flat << (
        modified + ZeroOrMore(ops + modified)
    ).addParseAction(make_tree).streamline()

    return flat.streamline()


_indent_stack = [(1, None, None)]


def reset_stack():
    global _indent_stack
    _indent_stack = [(1, None, None)]


add_reset_action(reset_stack)


def indentedBlock(blockStatementExpr, indent=True):
    """Helper method for defining space-delimited indentation blocks,
    such as those used to define block statements in Python source code.

    Parameters:

     - blockStatementExpr - expression defining syntax of statement that
       is repeated within the indented block
     - indentStack - list created by caller to manage indentation stack
       (multiple statementWithIndentedBlock expressions within a single
       grammar should share a common indentStack)
     - indent - boolean indicating whether block must be indented beyond
       the current level; set to False for block of left-most
       statements (default= ``True``)

    A valid block must contain at least one ``blockStatement``.
    """
    blockStatementExpr.engine.add_ignore("\\" + LineEnd())

    PEER = Forward()
    DEDENT = Forward()

    def _reset_stack(p=None, l=None, s=None, ex=None):
        oldCol, oldPeer, oldDedent = _indent_stack.pop()
        PEER << oldPeer
        DEDENT << oldDedent

    def peer_stack(expectedCol):
        def output(t, l, s):
            if l >= len(s):
                return
            curCol = col(l, s)
            if curCol != expectedCol:
                if curCol > expectedCol:
                    raise ParseException(t.type, s, l, "illegal nesting")
                raise ParseException(t.type, s, l, "not a peer entry")

        return output

    def dedent_stack(expectedCol):
        def output(t, l, s):
            if l >= len(s):
                return
            curCol = col(l, s)
            if curCol not in (i for i, _, _ in _indent_stack):
                raise ParseException(s, l, "not an unindent")
            if curCol < _indent_stack[-1][0]:
                oldCol, oldPeer, oldDedent = _indent_stack.pop()
                PEER << oldPeer
                DEDENT << oldDedent

        return output

    def indent_stack(t, l, s):
        curCol = col(l, s)
        if curCol > _indent_stack[-1][0]:
            PEER << Empty().addParseAction(peer_stack(curCol))
            DEDENT << Empty().addParseAction(dedent_stack(curCol))
            _indent_stack.append((curCol, PEER, DEDENT))
        else:
            raise ParseException(t.type, l, s, "not a subentry")

    def nodent_stack(t, l, s):
        curCol = col(l, s)
        if curCol == _indent_stack[-1][0]:
            PEER << Empty().addParseAction(peer_stack(curCol))
            DEDENT << Empty().addParseAction(dedent_stack(curCol))
            _indent_stack.append((curCol, PEER, DEDENT))
        else:
            raise ParseException(t.type, s, l, "not a subentry")

    NL = OneOrMore(LineEnd().suppress())
    INDENT = Empty().addParseAction(indent_stack)
    NODENT = Empty().addParseAction(nodent_stack)

    if indent:
        smExpr = Group(
            Optional(NL)
            + INDENT
            + OneOrMore(PEER + Group(blockStatementExpr) + Optional(NL))
            + DEDENT
        )
    else:
        smExpr = Group(
            Optional(NL)
            + NODENT
            + OneOrMore(PEER + Group(blockStatementExpr) + Optional(NL))
            + DEDENT
        )
    return smExpr.setFailAction(_reset_stack).set_parser_name("indented block")


alphas8bit = srange(r"[\0xc0-\0xd6\0xd8-\0xf6\0xf8-\0xff]")
punc8bit = srange(r"[\0xa1-\0xbf\0xd7\0xf7]")

anyOpenTag, anyCloseTag = makeHTMLTags(
    Word(alphas, alphanums + "_:").set_parser_name("any tag")
)
_htmlEntityMap = dict(zip("gt lt amp nbsp quot apos".split(), "><& \"'"))
commonHTMLEntity = Regex(
    "&(?P<entity>" + "|".join(_htmlEntityMap.keys()) + ");"
).set_parser_name("common HTML entity")


def replaceHTMLEntity(t):
    """Helper parser action to replace common HTML entities with their special characters"""
    return _htmlEntityMap.get(t.entity)


# it's easy to get these comment structures wrong - they're very common, so may as well make them available
cStyleComment = Combine(
    Regex(r"/\*(?:[^*]|\*(?!/))*") + "*/"
).set_parser_name("C style comment")
"Comment of the form ``/* ... */``"

htmlComment = Regex(r"<!--[\s\S]*?-->").set_parser_name("HTML comment")
"Comment of the form ``<!-- ... -->``"

with Engine() as engine:
    engine.set_whitespace("")
    restOfLine = Regex(r"[^\n]*").leaveWhitespace().set_parser_name("rest of line")

    dblSlashComment = Regex(r"//(?:\\\n|[^\n])*").set_parser_name("// comment")

    cppStyleComment = Combine(
        Regex(r"/\*(?:[^*]|\*(?!/))*") + "*/" | dblSlashComment
    ).set_parser_name("C++ style comment")

    javaStyleComment = cppStyleComment

    pythonStyleComment = Regex(r"#[^\n]*").set_parser_name("Python style comment")

_commasepitem = (
    Combine(OneOrMore(
        Word(printables, excludeChars=",")
        + Optional(Word(" \t") + ~Literal(",") + ~LineEnd())
    ))
    .addParseAction(lambda t: text(t).strip())
    .set_parser_name("commaItem")
)
commaSeparatedList = delimitedList(Optional(
    quotedString | _commasepitem, default=""
)).set_parser_name("commaSeparatedList")

"""Here are some common low-level expressions that may be useful in
jump-starting parser development:

 - numeric forms (`integers<integer>`, `reals<real>`,
   `scientific notation<sci_real>`)
 - common `programming identifiers<identifier>`
 - network addresses (`MAC<mac_address>`,
   `IPv4<ipv4_address>`, `IPv6<ipv6_address>`)
 - ISO8601 `dates<iso8601_date>` and
   `datetime<iso8601_datetime>`
 - `UUID<uuid>`
 - `comma-separated list<comma_separated_list>`

Parse actions:

 - `convertToInteger`
 - `convertToFloat`
 - `convertToDate`
 - `convertToDatetime`
 - `stripHTMLTags`
 - `upcaseTokens`
 - `downcaseTokens`

Example::

    test.runTests(number, '''
        # any int or real number, returned as the appropriate type
        100
        -100
        +100
        3.14159
        6.02e23
        1e-12
        ''')

    test.runTests(fnumber, '''
        # any int or real number, returned as float
        100
        -100
        +100
        3.14159
        6.02e23
        1e-12
        ''')

    test.runTests(hex_integer, '''
        # hex numbers
        100
        FF
        ''')

    test.runTests(fraction, '''
        # fractions
        1/2
        -3/4
        ''')

    test.runTests(mixed_integer, '''
        # mixed fractions
        1
        1/2
        -3/4
        1-3/4
        ''')

    import uuid
    uuid.addParseAction(tokenMap(uuid.UUID))
    test.runTests(uuid, '''
        # uuid
        12345678-1234-5678-1234-567812345678
        ''')

prints::

    # any int or real number, returned as the appropriate type
    100
    [100]

    -100
    [-100]

    +100
    [100]

    3.14159
    [3.14159]

    6.02e23
    [6.02e+23]

    1e-12
    [1e-12]

    # any int or real number, returned as float
    100
    [100.0]

    -100
    [-100.0]

    +100
    [100.0]

    3.14159
    [3.14159]

    6.02e23
    [6.02e+23]

    1e-12
    [1e-12]

    # hex numbers
    100
    [256]

    FF
    [255]

    # fractions
    1/2
    [0.5]

    -3/4
    [-0.75]

    # mixed fractions
    1
    [1]

    1/2
    [0.5]

    -3/4
    [-0.75]

    1-3/4
    [1.75]

    # uuid
    12345678-1234-5678-1234-567812345678
    [UUID('12345678-1234-5678-1234-567812345678')]
"""

convertToInteger = tokenMap(int)

convertToFloat = tokenMap(float)

integer = Word(nums).set_parser_name("integer").addParseAction(convertToInteger)
"""expression that parses an unsigned integer, returns an int"""

hex_integer = (
    Word(hexnums).set_parser_name("hex integer").addParseAction(tokenMap(int, 16))
)
"""expression that parses a hexadecimal integer, returns an int"""

signed_integer = (
    Regex(r"[+-]?\d+")
    .set_parser_name("signed integer")
    .addParseAction(convertToInteger)
)

fraction = (
    signed_integer.addParseAction(convertToFloat)
    + "/"
    + signed_integer.addParseAction(convertToFloat)
).set_parser_name("fraction")
fraction.addParseAction(lambda t: t[0] / t[-1])

mixed_integer = (
    fraction | signed_integer + Optional(Optional("-").suppress() + fraction)
).set_parser_name("fraction or mixed integer-fraction")
"""mixed integer of the form 'integer - fraction', with optional leading integer, returns float"""
mixed_integer.addParseAction(sum)

real = (
    Regex(r"[+-]?(:?\d+\.\d*|\.\d+)")
    .set_parser_name("real number")
    .addParseAction(convertToFloat)
)
"""expression that parses a floating point number and returns a float"""

sci_real = (
    Regex(r"[+-]?(:?\d+(:?[eE][+-]?\d+)|(:?\d+\.\d*|\.\d+)(:?[eE][+-]?\d+)?)")
    .set_parser_name("real number with scientific notation")
    .addParseAction(convertToFloat)
)
"""expression that parses a floating point number with optional
scientific notation and returns a float"""

# streamlining this expression makes the docs nicer-looking
number = (sci_real | real | signed_integer).streamline()
"""any numeric expression, returns the corresponding Python type"""

fnumber = (
    Regex(r"[+-]?\d+\.?\d*([eE][+-]?\d+)?")
    .set_parser_name("fnumber")
    .addParseAction(convertToFloat)
)
"""any int or real number, returned as float"""

identifier = Word(alphas + "_", alphanums + "_").set_parser_name("identifier")
"""typical code identifier (leading alpha or '_', followed by 0 or more alphas, nums, or '_')"""

ipv4_address = Regex(
    r"(25[0-5]|2[0-4][0-9]|1?[0-9]{1,2})(\.(25[0-5]|2[0-4][0-9]|1?[0-9]{1,2})){3}"
).set_parser_name("IPv4 address")
"IPv4 address (``0.0.0.0 - 255.255.255.255``)"

_ipv6_part = Regex(r"[0-9a-fA-F]{1,4}").set_parser_name("hex_integer")
_full_ipv6_address = (
    _ipv6_part + (":" + _ipv6_part) * 7
).set_parser_name("full IPv6 address")
_short_ipv6_address = (
    Optional(_ipv6_part + (":" + _ipv6_part) * (0, 6))
    + "::"
    + Optional(_ipv6_part + (":" + _ipv6_part) * (0, 6))
).set_parser_name("short IPv6 address")
_short_ipv6_address.addCondition(lambda t: sum(
    1 for tt in t if _ipv6_part.matches(tt)
) < 8)
_mixed_ipv6_address = ("::ffff:" + ipv4_address).set_parser_name("mixed IPv6 address")
ipv6_address = Combine(
    (
        _full_ipv6_address | _mixed_ipv6_address | _short_ipv6_address
    ).set_parser_name("IPv6 address")
).set_parser_name("IPv6 address")
"IPv6 address (long, short, or mixed form)"

mac_address = (
    Regex(r"[0-9a-fA-F]{2}([:.-])[0-9a-fA-F]{2}(?:\1[0-9a-fA-F]{2}){4}").set_parser_name("MAC address")
)
"MAC address xx:xx:xx:xx:xx (may also have '-' or '.' delimiters)"


def convertToDate(fmt="%Y-%m-%d"):
    """
    Helper to create a parse action for converting parsed date string to Python datetime.date

    Params -
     - fmt - format to be passed to datetime.strptime (default= ``"%Y-%m-%d"``)

    Example::

        date_expr = iso8601_date.copy()
        date_expr.addParseAction(convertToDate())
        print(date_expr.parseString("1999-12-31"))

    prints::

        [datetime.date(1999, 12, 31)]
    """

    def cvt_fn(t, l, s):
        try:
            return datetime.strptime(t[0], fmt).date()
        except ValueError as ve:
            raise ParseException(t.type, l, s, str(ve))

    return cvt_fn


def convertToDatetime(fmt="%Y-%m-%dT%H:%M:%S.%f"):
    """Helper to create a parse action for converting parsed
    datetime string to Python datetime.datetime

    Params -
     - fmt - format to be passed to datetime.strptime (default= ``"%Y-%m-%dT%H:%M:%S.%f"``)

    Example::

        dt_expr = iso8601_datetime.copy()
        dt_expr.addParseAction(convertToDatetime())
        print(dt_expr.parseString("1999-12-31T23:59:59.999"))

    prints::

        [datetime.datetime(1999, 12, 31, 23, 59, 59, 999000)]
    """

    def cvt_fn(t, l, s):
        try:
            return datetime.strptime(t[0], fmt)
        except ValueError as ve:
            raise ParseException(t.type, l, s, str(ve))

    return cvt_fn


iso8601_date = (
    Regex(r"(?P<year>\d{4})(?:-(?P<month>\d\d)(?:-(?P<day>\d\d))?)?").set_parser_name("ISO8601 date")
)
"ISO8601 date (``yyyy-mm-dd``)"

iso8601_datetime = Regex(
    r"(?P<year>\d{4})-(?P<month>\d\d)-(?P<day>\d\d)[T"
    r" ](?P<hour>\d\d):(?P<minute>\d\d)(:(?P<second>\d\d(\.\d*)?)?)?(?P<tz>Z|[+-]\d\d:?\d\d)?"
).set_parser_name("ISO8601 datetime")
"ISO8601 datetime (``yyyy-mm-ddThh:mm:ss.s(Z|+-00:00)``) - trailing seconds, milliseconds, and timezone optional; accepts separating ``'T'`` or ``' '``"

uuid = (
    Regex(r"[0-9a-fA-F]{8}(-[0-9a-fA-F]{4}){3}-[0-9a-fA-F]{12}").set_parser_name("UUID")
)
"UUID (``xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx``)"

_html_stripper = anyOpenTag.suppress() | anyCloseTag.suppress()


def stripHTMLTags(tokens, l, s):
    """Parse action to remove HTML tags from web page HTML source

    Example::

        # strip HTML links from normal text
        text = '<td>More info at the <a href="https://github.com/mo_parsing/mo_parsing/wiki">mo_parsing</a> wiki page</td>'
        td, td_end = makeHTMLTags("TD")
        table_text = td + SkipTo(td_end).addParseAction(stripHTMLTags)("body") + td_end
        print(table_text.parseString(text).body)

    Prints::

        More info at the mo_parsing wiki page
    """
    return _html_stripper.transformString(tokens[0])


def _strip(tok):
    return "".join(tok).strip()


_commasepitem = (
    Word(printables + " \t", excludeChars=",")
    .set_parser_name("commaItem")
    .addParseAction(_strip)
)
comma_separated_list = delimitedList(Optional(
    quotedString | _commasepitem, default=""
)).set_parser_name("comma separated list")
"""Predefined expression of 1 or more printable words or quoted strings, separated by commas."""


# export
from mo_parsing import core, engine

core._flatten = _flatten
core.replaceWith = replaceWith
core.quotedString = quotedString
