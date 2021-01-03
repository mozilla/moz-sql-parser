# encoding: utf-8
from collections import OrderedDict
from operator import itemgetter

from mo_future import Iterable, text, generator_types

from mo_parsing.core import ParserElement, _PendingSkip
from mo_parsing.engine import Engine
from mo_parsing.enhancement import Optional, SkipTo, Many
from mo_parsing.exceptions import (
    ParseException,
    ParseSyntaxException,
)
from mo_parsing.results import ParseResults
from mo_parsing.tokens import Empty
from mo_parsing.utils import (
    empty_tuple,
    is_forward,
    regex_iso,
    Log,
    append_config,
    stack_depth,
    regex_caseless,
)


class ParseExpression(ParserElement):
    """Abstract subclass of ParserElement, for combining and
    post-processing parsed tokens.
    """

    __slots__ = ["exprs"]

    def __init__(self, exprs):
        super(ParseExpression, self).__init__()

        if isinstance(exprs, generator_types):
            exprs = list(exprs)
        elif not isinstance(exprs, ParserElement) and isinstance(exprs, Iterable):
            exprs = list(exprs)
        else:
            exprs = [exprs]

        self.exprs = [engine.CURRENT.normalize(e) for e in exprs]
        for e in self.exprs:
            if is_forward(e):
                e.track(self)

    def expecting(self):
        output = OrderedDict()
        if not self.is_annotated():
            for e in self.exprs:
                expect = e.expecting()
                for k, ee in expect.items():
                    output.setdefault(k, []).extend(ee)
        else:
            for e in self.exprs:
                expect = e.expecting()
                for k, _ in expect.items():
                    output[k] = [self]
        return output

    def copy(self):
        output = ParserElement.copy(self)
        if self.engine is engine.CURRENT:
            output.exprs = self.exprs
        else:
            output.exprs = [e.copy() for e in self.exprs]
        return output

    def append(self, other):
        self.exprs.append(other)
        return self

    def leaveWhitespace(self):
        """Extends ``leaveWhitespace`` defined in base class, and also invokes ``leaveWhitespace`` on
        all contained expressions."""
        with Engine(""):
            output = self.copy()
            output.exprs = [e.leaveWhitespace() for e in self.exprs]
            return output

    def streamline(self):
        if self.streamlined:
            return self
        self.streamlined = True

        # collapse nested And's of the form And(And(And(a, b), c), d) to And(a, b, c, d)
        # but only if there are no parse actions or resultsNames on the nested And's
        # (likewise for Or's and MatchFirst's)
        if not self.is_annotated() and not self.exprs:
            return Empty(self.parser_name)

        acc = []
        same = True
        for e in self.exprs:
            f = e.streamline()
            same = same and f is e
            if f.is_annotated():
                acc.append(f)
            elif isinstance(f, self.__class__):
                same = False
                acc.extend(f.exprs)
            else:
                acc.append(f)

        if same:
            return self

        output = self.copy()
        output.exprs = acc
        output.streamlined = True
        return output

    def __call__(self, name):
        if not name:
            return self
        # for e in self.exprs:
        #     if isinstance(e, ParserElement) and e.token_name:
        #         Log.error("token name is already set in child, use Group() to clarify")

        return ParserElement.__call__(self, name)


class And(ParseExpression):
    """
    Requires all given `ParseExpression` s to be found in the given order.
    Expressions may be separated by whitespace.
    May be constructed using the ``'+'`` operator.
    May also be constructed using the ``'-'`` operator, which will
    suppress backtracking.

    """

    __slots__ = []

    class _ErrorStop(Empty):
        def __init__(self, *args, **kwargs):
            with Engine() as engine:
                engine.set_whitespace("")
                super(And._ErrorStop, self).__init__(*args, **kwargs)
                self.parser_name = "-"

    def __init__(self, exprs):
        if exprs and Ellipsis in exprs:
            tmp = []
            for i, expr in enumerate(exprs):
                if expr is Ellipsis:
                    if i < len(exprs) - 1:
                        skipto_arg = (Empty() + exprs[i + 1]).exprs[-1]
                        tmp.append(SkipTo(skipto_arg)("_skipped"))
                    else:
                        raise Exception(
                            "cannot construct And with sequence ending in ..."
                        )
                else:
                    tmp.append(expr)
            exprs[:] = tmp
        super(And, self).__init__(exprs)

    def streamline(self):
        if self.streamlined:
            return self
        if not self.exprs:
            return Empty(self.parser_name)
        if len(self.exprs) == 1 and not self.is_annotated():
            return self.exprs[0].streamline()

        # collapse any _PendingSkip's
        same = True
        exprs = self.exprs
        if any(
            isinstance(e, ParseExpression)
            and e.exprs
            and isinstance(e.exprs[-1], _PendingSkip)
            for e in exprs[:-1]
        ):
            same = False
            for i, e in enumerate(exprs[:-1]):
                if (
                    isinstance(e, ParseExpression)
                    and e.exprs
                    and isinstance(e.exprs[-1], _PendingSkip)
                ):
                    ee = e.exprs[-1] + exprs[i + 1]
                    e.exprs[-1] = ee
                    e.streamlined = False
                    exprs[i + 1] = None

        # streamline INDIVIDUAL EXPRESSIONS
        acc = []
        for e in exprs:
            if e is None:
                continue
            f = e.streamline()
            same = same and f is e
            if f.is_annotated():
                acc.append(f)
            elif isinstance(f, self.__class__):
                same = False
                acc.extend(f.exprs)
            else:
                acc.append(f)

        if same:
            self.streamlined = True
            return self

        output = self.copy()
        output.exprs = acc
        output.streamlined = True
        return output

    def expecting(self):
        if self.exprs:
            return OrderedDict((
                (k, self)
                for k, e in self.exprs[0].expecting().items()
            ))
        else:
            return {}

    def _min_length(self):
        return sum(e.min_length() for e in self.exprs)

    def parseImpl(self, string, start, doActions=True):
        # pass False as last arg to _parse for first element, since we already
        # pre-parsed the string as part of our And pre-parsing
        encountered_error_stop = False
        end = start
        acc = []
        for expr in self.exprs:
            if isinstance(expr, And._ErrorStop):
                encountered_error_stop = True
                continue
            try:
                result = expr._parse(string, end, doActions)
                end = result.end
                acc.append(result)
            except ParseException as pe:
                if encountered_error_stop:
                    raise ParseSyntaxException(pe.expr, pe.loc, pe.string)
                else:
                    raise pe

        return ParseResults(self, start, end, acc)

    def __add__(self, other):
        if other is Ellipsis:
            return _PendingSkip(self)

        return And([self, engine.CURRENT.normalize(other)]).streamline()

    def checkRecursion(self, seen=empty_tuple):
        subRecCheckList = seen + (self,)
        for e in self.exprs:
            e.checkRecursion(subRecCheckList)
            if e.min_length():
                return

    def __regex__(self):
        return "+", "".join(regex_iso(*e.__regex__(), "+") for e in self.exprs)

    def __str__(self):
        if self.parser_name:
            return self.parser_name

        return "{" + " + ".join(text(e) for e in self.exprs) + "}"


class Or(ParseExpression):
    """
    Requires that at least one `ParseExpression` is found. If
    two expressions match, the expression that matches the longest
    string will be used. May be constructed using the ``'^'``
    operator.
    """

    __slots__ = []

    def __init__(self, exprs):
        super(Or, self).__init__(exprs)

    def _min_length(self):
        return min(e.min_length() for e in self.exprs)

    def parseImpl(self, string, start, doActions=True):
        causes = []
        matches = []
        if len(self.exprs) > 20:
            print(";lomg")
        for e in self.exprs:
            try:
                end = e.tryParse(string, start)
                matches.append((end, e))
            except ParseException as err:
                causes.append(err)

        if matches:
            # re-evaluate all matches in descending order of length of match, in case attached actions
            # might change whether or how much they match of the input.
            matches.sort(key=itemgetter(0), reverse=True)

            if not doActions:
                # no further conditions or parse actions to change the selection of
                # alternative, so the first match will be the best match
                _, best_expr = matches[0]
                best_results = best_expr._parse(string, start, doActions)
                return ParseResults(
                    self, best_results.start, best_results.end, [best_results]
                )

            longest = -1, None
            for loc, expr1 in matches:
                if loc <= longest[0]:
                    # already have a longer match than this one will deliver, we are done
                    return longest

                try:
                    toks = expr1._parse(string, start, doActions)
                except ParseException as err:
                    causes.append(err)
                else:
                    if toks.end >= loc:
                        return ParseResults(self, toks.start, toks.end, [toks])
                    # didn't match as much as before
                    elif toks.end > longest[0]:
                        longest = (
                            toks.end,
                            ParseResults(self, toks.start, toks.end, [toks]),
                        )

            if longest != (-1, None):
                return longest

        raise ParseException(
            self, start, string, msg="no defined alternatives to match", cause=causes
        )

    def checkRecursion(self, seen=empty_tuple):
        seen_more = seen + (self,)
        for e in self.exprs:
            e.checkRecursion(seen_more)

    def __regex__(self):
        return (
            "|",
            "|".join(
                regex_iso(*e.__regex__(), "|")
                for e in self.exprs
                if not isinstance(e, Empty)
            ),
        )

    def __str__(self):
        if self.parser_name:
            return self.parser_name

        return "{" + " ^ ".join(text(e) for e in self.exprs) + "}"


class MatchFirst(ParseExpression):
    """Requires that at least one `ParseExpression` is found. If
    two expressions match, the first one listed is the one that will
    match. May be constructed using the ``'|'`` operator.
    """

    __slots__ = []

    def __init__(self, exprs):
        ParseExpression.__init__(self, exprs)

    def _min_length(self):
        if self.exprs:
            return min(e.min_length() for e in self.exprs)
        else:
            Log.warning("expecting streamline")
            return 0

    def parseImpl(self, string, start, doActions=True):
        causes = []

        for e in self.exprs:
            try:
                result = e._parse(string, start, doActions)
                return ParseResults(self, result.start, result.end, [result])
            except ParseException as cause:
                causes.append(cause)

        raise ParseException(self, start, string, cause=causes)

    def streamline(self):
        if self.streamlined:
            return self

        output = ParseExpression.streamline(self)

        if isinstance(output, Empty):
            return output
        if not output.is_annotated():
            if len(output.exprs) == 0:
                output = Empty()
            if len(output.exprs) == 1:
                output = output.exprs[0]

        patterns = [e.expecting() for e in output.exprs]
        # SOME NUMBER OF CONSTANT PATTERNS
        if sum(1 if p != None else 0 for p in patterns) > 5:
            print("found")
        output.streamlined = True
        output.checkRecursion()
        return output

    def checkRecursion(self, seen=empty_tuple):
        seen_more = seen + (self,)
        for e in self.exprs:
            e.checkRecursion(seen_more)

    def __or__(self, other):
        if other is Ellipsis:
            return _PendingSkip(Optional(self))

        return MatchFirst([self, engine.CURRENT.normalize(other)]).streamline()

    def __ror__(self, other):
        return engine.CURRENT.normalize(other) | self

    def __regex__(self):
        return (
            "|",
            "|".join(
                regex_iso(*e.__regex__(), "|")
                for e in self.exprs
                if not isinstance(e, Empty)
            ),
        )

    def __str__(self):
        if self.parser_name:
            return self.parser_name

        return " | ".join("{" + text(e) + "}" for e in self.exprs)


def _distinct(a, b):
    """
    ASSUME a != b
    RETURN MINIMUM length SO THAT a[:length] != b[:length]
    """
    ii = 1
    for aa, bb in zip(a, b):
        if aa != bb:
            return ii
        ii += 1
    return ii


class MatchFirstFast(MatchFirst):
    __slots__ = []

    def __init__(self, exprs):
        MatchFirst.__init__(self, exprs)

        lookup = OrderedDict()
        for e in exprs:
            for k, ee in e.expecting().items():
                lookup.setdefault(k, []).append(ee)

        merge = [(k, v) for k, v in lookup]
        if len(merge) <= 1:
            Log.error("not useful")

        # patterns must be mutually exclusive to work
        for i, (k, e) in enumerate(merge[:-1]):
            for kk, ee in merge[i + 1 :]:
                if kk.startswith(k) or k.startswith(kk):
                    e.extend(ee)
                    ee.clear()
        compact = [(k, v) for k, v in merge if v]
        if len(compact) <= 1:
            Log.error("not useful")

        # patterns can be shortened so far as they remain exclusive
        shorter = [(
            (k[:min_length], e)
            for i, (k, e) in compact[:-1]
            if e
            for min_length in [max(_distinct(k, kk) for kk, _ in compact[i + 1 :])]
        )]

        self.set_config(
            lookup={k: e for k, e in shorter},
            regex="|".join(regex_caseless(k) for k, _ in shorter),
        )

    def parseImpl(self, string, start, doActions=True):
        config = self.parser_config
        found = config.regex.match(string, start)
        index = found.end().lower()

        try:
            result = config.lookup.get(index).parseImpl(string, start, doActions)
            return ParseResults(self, result.start, result.end, [result])
        except ParseException as cause:
            raise ParseException(self, start, string, cause=cause)


class Each(ParseExpression):
    """
    Requires all given `ParseExpression` s to be found, but in
    any order. Expressions may be separated by whitespace.

    May be constructed using the ``'&'`` operator.
    """

    __slots__ = []
    Config = append_config(ParseExpression, "min_match", "max_match")

    def __init__(self, exprs):
        """
        :param exprs: The expressions to be matched
        :param mins: list of integers indincating any minimums
        """
        super(Each, self).__init__(exprs)
        self.set_config(
            min_match=[
                e.parser_config.min_match if isinstance(e, Many) else 1 for e in exprs
            ],
            max_match=[
                e.parser_config.max_match if isinstance(e, Many) else 1 for e in exprs
            ],
        )

    def streamline(self):
        if self.streamlined:
            return self
        return super(Each, self).streamline()

    def _min_length(self):
        # TODO: MAY BE TOO CONSERVATIVE, WE MAY BE ABLE TO PROVE self CAN CONSUME A CHARACTER
        return min(e.min_length() for e in self.exprs)

    def parseImpl(self, string, start, doActions=True):
        end = start
        matchOrder = []
        todo = list(zip(
            self.exprs, self.parser_config.min_match, self.parser_config.max_match
        ))
        count = [0] * len(self.exprs)

        while todo:
            for i, (c, (e, mi, ma)) in enumerate(zip(count, todo)):
                try:
                    loc = e.tryParse(string, end)
                    if loc == end:
                        continue
                    end = loc
                    c2 = count[i] = c + 1
                    if c2 >= ma:
                        del todo[i]
                        del count[i]
                    matchOrder.append(e)
                    break
                except ParseException as pe:
                    continue
            else:
                break

        for c, (e, mi, ma) in zip(count, todo):
            if c < mi:
                raise ParseException(
                    string,
                    start,
                    "Missing minimum (%i) more required elements (%s)" % (mi, e),
                )

        found = set(id(m) for m in matchOrder)
        missing = [
            e
            for e, mi in zip(self.exprs, self.parser_config.min_match)
            if id(e) not in found and mi > 0
        ]
        if missing:
            missing = ", ".join(text(e) for e in missing)
            raise ParseException(
                string, start, "Missing one or more required elements (%s)" % missing
            )

        # add any unmatched Optionals, in case they have default values defined
        matchOrder += [e for e in self.exprs if id(e) not in found]

        results = []
        end = start
        for e in matchOrder:
            result = e._parse(string, end, doActions)
            end = result.end
            results.append(result)

        return ParseResults(self, results[0].start, results[-1].end, results)

    def __str__(self):
        if self.parser_name:
            return self.parser_name

        return "{" + " & ".join(text(e) for e in self.exprs) + "}"


# export
from mo_parsing import core, engine

core.And = And
core.Or = Or
core.Each = Each
core.MatchFirst = MatchFirst

from mo_parsing import helpers

helpers.And = And
helpers.Or = Or
helpers.MatchFirst = MatchFirst
