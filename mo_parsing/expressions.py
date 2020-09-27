# encoding: utf-8
import sys
from operator import itemgetter

from mo_dots import unwrap
from mo_future import Iterable, text, generator_types
from mo_logs import Log

from mo_parsing.core import ParserElement, _PendingSkip, is_decorated
from mo_parsing.engine import Engine
from mo_parsing.enhancement import OneOrMore, Optional, SkipTo, ZeroOrMore, Many
from mo_parsing.exceptions import (
    ParseBaseException,
    ParseException,
    ParseSyntaxException,
)
from mo_parsing.results import ParseResults
from mo_parsing.tokens import Empty
from mo_parsing.utils import empty_list, empty_tuple, _MAX_INT


class ParseExpression(ParserElement):
    """Abstract subclass of ParserElement, for combining and
    post-processing parsed tokens.
    """

    def __init__(self, exprs):
        super(ParseExpression, self).__init__()

        if isinstance(exprs, generator_types):
            exprs = list(exprs)
        elif not isinstance(exprs, ParserElement) and isinstance(exprs, Iterable):
            exprs = list(exprs)
        else:
            exprs = [exprs]

        self.exprs = [engine.CURRENT.normalize(e) for e in exprs]

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
        output = self.copy()
        if self.engine.white_chars:
            Log.error("do not know how to handle")

        output.exprs = [e.leaveWhitespace() for e in self.exprs]
        return output

    def __str__(self):
        try:
            return super(ParseExpression, self).__str__()
        except Exception:
            pass

        return "%s:(%s)" % (self.__class__.__name__, text(self.exprs))

    def streamline(self):
        if self.streamlined:
            return self
        self.streamlined = True

        # collapse nested And's of the form And(And(And(a, b), c), d) to And(a, b, c, d)
        # but only if there are no parse actions or resultsNames on the nested And's
        # (likewise for Or's and MatchFirst's)
        if not self.exprs:
            return Empty(self.parser_name)

        acc = []
        for e in self.exprs:
            e = e.streamline()
            if isinstance(e, self.__class__) and not is_decorated(e):
                acc.extend(e.exprs)
            else:
                acc.append(e)

        self.exprs = acc
        return self

    def validate(self, seen=empty_list):
        tmp = seen + [self]
        for e in self.exprs:
            e.validate(tmp)
        self.checkRecursion()

    def checkRecursion(self, seen=empty_tuple):
        seen_more = seen + (self,)
        for e in self.exprs:
            e.checkRecursion(seen_more)

    def __call__(self, name):
        if not name:
            return self
        # for e in self.exprs:
        #     if isinstance(e, ParserElement) and e.token_name:
        #         Log.error("token name is already set in child, use Group() to clarify")

        return ParserElement.__call__(self, name)


class And(ParseExpression):
    """
    Requires all given :class:`ParseExpression` s to be found in the given order.
    Expressions may be separated by whitespace.
    May be constructed using the ``'+'`` operator.
    May also be constructed using the ``'-'`` operator, which will
    suppress backtracking.

    Example::

        integer = Word(nums)
        name_expr = OneOrMore(Word(alphas))

        expr = And([integer("id"), name_expr("name"), integer("age")])
        # more easily written as:
        expr = integer("id") + name_expr("name") + integer("age")
    """

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
        self.parser_config.mayReturnEmpty = all(
            e.parser_config.mayReturnEmpty for e in self.exprs
        )

    def streamline(self):
        if self.streamlined:
            return self

        # collapse any _PendingSkip's
        if any(
            isinstance(e, ParseExpression)
            and e.exprs
            and isinstance(e.exprs[-1], _PendingSkip)
            for e in self.exprs[:-1]
        ):
            for i, e in enumerate(self.exprs[:-1]):
                if (
                    isinstance(e, ParseExpression)
                    and e.exprs
                    and isinstance(e.exprs[-1], _PendingSkip)
                ):
                    ee = e.exprs[-1] + self.exprs[i + 1]
                    e.exprs[-1] = ee
                    e.streamlined = False
                    self.exprs[i + 1] = None
            self.exprs = [e for e in self.exprs if e is not None]

        output = ParseExpression.streamline(self)
        if isinstance(output, Empty):
            return output
        elif len(output.exprs) == 1 and not is_decorated(output):
            return output.exprs[0]
        output.parser_config.mayReturnEmpty = all(
            e.parser_config.mayReturnEmpty for e in self.exprs
        )
        return output

    def parseImpl(self, instring, loc, doActions=True):
        # pass False as last arg to _parse for first element, since we already
        # pre-parsed the string as part of our And pre-parsing
        encountered_error_stop = False
        acc = []
        for e in self.exprs:
            if isinstance(e, And._ErrorStop):
                encountered_error_stop = True
                continue
            try:
                loc, exprtokens = e._parse(instring, loc, doActions)
            except ParseSyntaxException as e:
                raise e
            except ParseBaseException as pe:
                if encountered_error_stop:
                    raise ParseSyntaxException(pe.pstr, pe.loc, pe.parserElement)
                else:
                    raise pe
            except IndexError as ie:
                if encountered_error_stop:
                    raise ParseSyntaxException(instring, len(instring), self)
                else:
                    raise ie

            if not isinstance(exprtokens, ParseResults):
                Log.error(
                    "expecting {{type}} to emit parseresults", type=e.__class__.__name__
                )
            acc.append(exprtokens)

        return loc, ParseResults(self, acc)

    def __add__(self, other):
        if other is Ellipsis:
            return _PendingSkip(self)

        return And([self, engine.CURRENT.normalize(other)]).streamline()

    def checkRecursion(self, seen=empty_tuple):
        subRecCheckList = seen + (self,)
        for e in self.exprs:
            e.checkRecursion(subRecCheckList)
            if not e.parser_config.mayReturnEmpty:
                break

    def __str__(self):
        if self.parser_name:
            return self.parser_name

        return "{" + " + ".join(text(e) for e in self.exprs) + "}"


class Or(ParseExpression):
    """Requires that at least one :class:`ParseExpression` is found. If
    two expressions match, the expression that matches the longest
    string will be used. May be constructed using the ``'^'``
    operator.

    Example::

        # construct Or using '^' operator

        number = Word(nums) ^ Combine(Word(nums) + '.' + Word(nums))
        print(number.searchString("123 3.1416 789"))

    prints::

        [['123'], ['3.1416'], ['789']]
    """

    def __init__(self, exprs):
        super(Or, self).__init__(exprs)
        if self.exprs:
            self.parser_config.mayReturnEmpty = any(
                e.parser_config.mayReturnEmpty for e in self.exprs
            )
        else:
            self.parser_config.mayReturnEmpty = True

    def parseImpl(self, instring, loc, doActions=True):
        maxExcLoc = -1
        maxException = None
        matches = []
        for e in self.exprs:
            try:
                loc2 = e.tryParse(instring, loc)
            except ParseException as err:
                err.__traceback__ = None
                if err.loc > maxExcLoc:
                    maxException = err
                    maxExcLoc = err.loc
            except IndexError:
                if len(instring) > maxExcLoc:
                    maxException = ParseException(instring, len(instring), self)
                    maxExcLoc = len(instring)
            else:
                # save match among all matches, to retry longest to shortest
                matches.append((loc2, e))

        if matches:
            # re-evaluate all matches in descending order of length of match, in case attached actions
            # might change whether or how much they match of the input.
            matches.sort(key=itemgetter(0), reverse=True)

            if not doActions:
                # no further conditions or parse actions to change the selection of
                # alternative, so the first match will be the best match
                _, best_expr = matches[0]
                loc, best_results = best_expr._parse(instring, loc, doActions)
                return loc, ParseResults(self, [best_results])

            longest = -1, None
            for loc1, expr1 in matches:
                if loc1 <= longest[0]:
                    # already have a longer match than this one will deliver, we are done
                    return longest

                try:
                    loc2, toks = expr1._parse(instring, loc, doActions)
                except ParseException as err:
                    err.__traceback__ = None
                    if err.loc > maxExcLoc:
                        maxException = err
                        maxExcLoc = err.loc
                else:
                    if loc2 >= loc1:
                        return loc2, ParseResults(self, [toks])
                    # didn't match as much as before
                    elif loc2 > longest[0]:
                        longest = loc2, ParseResults(self, [toks])

            if longest != (-1, None):
                return longest

        if maxException is not None:
            maxException.msg = "expecting " + text(self)
            raise maxException
        else:
            raise ParseException(
                instring, loc, "no defined alternatives to match", self
            )

    def __str__(self):
        if self.parser_name:
            return self.parser_name

        return "{" + " ^ ".join(text(e) for e in self.exprs) + "}"


class MatchFirst(ParseExpression):
    """Requires that at least one :class:`ParseExpression` is found. If
    two expressions match, the first one listed is the one that will
    match. May be constructed using the ``'|'`` operator.

    Example::

        # construct MatchFirst using '|' operator

        # watch the order of expressions to match
        number = Word(nums) | Combine(Word(nums) + '.' + Word(nums))
        print(number.searchString("123 3.1416 789")) #  Fail! -> [['123'], ['3'], ['1416'], ['789']]

        # put more selective expression first
        number = Combine(Word(nums) + '.' + Word(nums)) | Word(nums)
        print(number.searchString("123 3.1416 789")) #  Better -> [['123'], ['3.1416'], ['789']]
    """

    def __init__(self, exprs):
        super(MatchFirst, self).__init__(exprs)
        if self.exprs:
            self.parser_config.mayReturnEmpty = any(
                e.parser_config.mayReturnEmpty for e in self.exprs
            )
        else:
            self.parser_config.mayReturnEmpty = True

    def parseImpl(self, instring, loc, doActions=True):
        maxExcLoc = -1
        maxException = None
        for e in self.exprs:
            try:
                loc, ret = e._parse(instring, loc, doActions)
                return loc, ParseResults(self, [ret])
            except ParseException as err:
                if err.loc > maxExcLoc:
                    maxException = err
                    maxExcLoc = err.loc
            except IndexError:
                if len(instring) > maxExcLoc:
                    maxException = ParseException(instring, len(instring), self)
                    maxExcLoc = len(instring)

        # only got here if no expression matched, raise exception for match that made it the furthest
        else:
            if maxException is not None:
                maxException.msg = "Expecting " + text(self)
                raise maxException
            else:
                raise ParseException(instring, loc, self)

    def __or__(self, other):
        if other is Ellipsis:
            return _PendingSkip(Optional(self))

        return MatchFirst([self, engine.CURRENT.normalize(other)]).streamline()

    def __ror__(self, other):
        return engine.CURRENT.normalize(other) | self

    def __str__(self):
        if self.parser_name:
            return self.parser_name

        return " | ".join("{" + text(e) + "}" for e in self.exprs)


class Each(ParseExpression):
    """
    Requires all given :class:`ParseExpression` s to be found, but in
    any order. Expressions may be separated by whitespace.

    May be constructed using the ``'&'`` operator.
    """

    def __init__(self, exprs):
        """
        :param exprs: The expressions to be matched
        :param mins: list of integers indincating any minimums
        """
        super(Each, self).__init__(exprs)
        self.parser_config.min_match = [e.min_match if isinstance(e, Many) else 1 for e in exprs]
        self.parser_config.max_match = [e.max_match if isinstance(e, Many) else 1 for e in exprs]

        self.parser_config.mayReturnEmpty = all(
            e.parser_config.mayReturnEmpty for e in self.exprs
        )
        self.initExprGroups = True

    def streamline(self):
        if self.streamlined:
            return self

        super(Each, self).streamline()
        self.parser_config.mayReturnEmpty = all(
            e.parser_config.mayReturnEmpty for e in self.exprs
        )
        return self

    def parseImpl(self, instring, loc, doActions=True):
        end_loc = loc
        matchOrder = []
        todo = list(zip(
            self.exprs,
            self.parser_config.min_match,
            self.parser_config.max_match
        ))
        count = [0] * len(self.exprs)

        while todo:
            for i, (c, (e, mi, ma)) in enumerate(zip(count, todo)):
                try:
                    temp_loc = e.tryParse(instring, end_loc)
                    if temp_loc == end_loc:
                        continue
                    end_loc = temp_loc
                    c2 = count[i] = c + 1
                    if c2 >= ma:
                        del todo[i]
                        del count[i]
                    matchOrder.append(e)
                    break
                except ParseException:
                    continue
            else:
                break

        for c, (e, mi, ma) in zip(count, todo):
            if c < mi:
                raise ParseException(
                    instring,
                    loc,
                    "Missing minimum (%i) more required elements (%s)" % (mi, e),
                )

        found = set(id(m) for m in matchOrder)
        missing = [
            e
            for e, mi in zip(self.exprs, self.parser_config.min_matches)
            if id(e) not in found and not e.parser_config.mayReturnEmpty and mi > 0
        ]
        if missing:
            missing = ", ".join(text(e) for e in missing)
            raise ParseException(
                instring, loc, "Missing one or more required elements (%s)" % missing
            )

        # add any unmatched Optionals, in case they have default values defined
        matchOrder += [
            e
            for e in self.exprs
            if id(e) not in found and e.parser_config.mayReturnEmpty
        ]

        results = []
        for e in matchOrder:
            loc, result = e._parse(instring, loc, doActions)
            results.append(result)

        return loc, ParseResults(self, results)

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

helpers.MatchFirst = MatchFirst
helpers.And = And
