# encoding: utf-8
import re
import warnings
from collections import Iterable

from mo_dots import listwrap
from mo_future import text
from mo_imports import delay_import

from mo_parsing import engine
from mo_parsing.enhancement import (
    Combine,
    Forward,
    Group,
    Suppress,
    ZeroOrMore,
)
from mo_parsing.expressions import MatchFirst, Or
from mo_parsing.results import ParseResults, NO_PARSER
from mo_parsing.tokens import (
    CaselessKeyword,
    CaselessLiteral,
    Keyword,
    NoMatch,
    Literal,
    Empty,
)
from mo_parsing.utils import regex_range, wrap_parse_action

Regex = delay_import("mo_parsing.regex.Regex")


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


def oneOf(strs, caseless=False, asKeyword=False):
    """
    Helper to quickly define a set of alternative Literals, and makes
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
        return MatchFirst(parseElementClass(sym) for sym in symbols).streamline()

    # CONVERT INTO REGEX
    singles = [s for s in symbols if len(s) == 1]
    rest = list(sorted([s for s in symbols if len(s) != 1], key=lambda s: -len(s)))

    acc = []
    acc.extend(re.escape(sym) for sym in rest)
    if singles:
        acc.append(regex_range("".join(singles)))
    regex = "|".join(acc)

    return Regex(regex).streamline()


LEFT_ASSOC = object()
RIGHT_ASSOC = object()
_no_op = Empty()


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
        if op == None:
            op = _no_op
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
        for opPart in set(
            opPart
            for expr, op, is_suppressed, arity, assoc, pa in opList
            if arity > 1
            for opPart in (op if isinstance(op, tuple) else [op])
        )
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

        result = flat_tokens[0][0]
        result.end = tokens.end
        return result

    flat = Forward()
    iso = lpar.suppress() + flat + rpar.suppress()
    atom = (baseExpr | iso).addParseAction(record_op(baseExpr))
    modified = ZeroOrMore(prefix_ops) + atom + ZeroOrMore(suffix_ops)
    flat << (
        modified + ZeroOrMore(ops + modified)
    ).addParseAction(make_tree).streamline()

    return flat.streamline()
