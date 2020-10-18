# encoding: utf-8
import inspect
from collections import MutableMapping

from mo_dots import is_many
from mo_future import is_text, text, PY3, NEXT, zip_longest
from mo_parsing.utils import Log

from mo_parsing import engine
from mo_parsing.utils import is_forward, forward_type

USE_ATTRIBUTE_ACCESS = False

Suppress, ParserElement, NO_PARSER, NO_RESULTS, Group, Dict, Token, Empty = [None] * 8


class ParseResults(object):
    __slots__ = [
        "type",
        "tokens",
    ]

    @property
    def name(self):
        return self.type.token_name

    def __init__(self, result_type, tokens=None):
        self.type = result_type
        self.tokens = tokens

    def _get_item_by_name(self, name):
        # return open list of (modal, value) pairs
        # modal==True means only the last value is relevant
        for tok in self.tokens:
            if isinstance(tok, ParseResults):
                if tok.name == name:
                    if isinstance(tok.type, Group):
                        yield tok
                    elif is_forward(tok.type) and isinstance(tok.tokens[0].type, Group):
                        yield tok
                    else:
                        for t in tok.tokens:
                            yield t
                    continue
                elif isinstance(tok.type, Group):
                    continue
                elif is_forward(tok.type) and isinstance(tok.tokens[0].type, Group):
                    continue
                elif tok.name:
                    continue
                for f in tok._get_item_by_name(name):
                    yield f

    def __getitem__(self, item):
        if is_forward(self.type):
            return self.tokens[0][item]

        if isinstance(item, int):
            if item < 0:
                item = len(self) + item
            for ii, v in enumerate(self):
                if item == ii:
                    return v
        elif isinstance(item, slice):
            return list(iter(self))[item]
        else:
            values = list(self._get_item_by_name(item))
            if len(values) == 0:
                return NO_RESULTS
            if len(values) == 1:
                return values[0]
            # ENCAPSULATE IN A ParseResults FOR FURTHER NAVIGATION
            return ParseResults(NO_PARSER, values)

    def __setitem__(self, k, v):
        if isinstance(k, (slice, int)):
            Log.error("not supported")

        if v is None:
            v = NO_RESULTS

        if is_forward(self):
            self.tokens[0][k] = v
            return

        for i, tok in enumerate(self.tokens):
            if isinstance(tok, ParseResults):
                if tok.name == k:
                    self.tokens[i] = v
                    v = NO_RESULTS  # ERASE ALL OTHERS
                elif isinstance(tok.type, Group):
                    continue
                elif is_forward(tok.type) and isinstance(tok.tokens[0].type, Group):
                    continue
                elif tok.name:
                    continue

                tok.__setitem__(k, NO_RESULTS)  # ERASE ALL CHILDREN

        if v is not NO_RESULTS:
            self.tokens.append(Annotation(k, [v]))

    if USE_ATTRIBUTE_ACCESS:

        def __getattribute__(self, item):
            try:
                return object.__getattribute__(self, item)
            except Exception as e:
                output = self[item]
                if not output:
                    raise e
                return output

        def __setattr__(self, key, value):
            if key in ParseResults.__slots__:
                return object.__setattr__(self, key, value)
            self[key] = value

    def __contains__(self, k):
        return any((r.name) == k for r in self.tokens)

    def length(self):
        return sum(1 for _ in self)

    def __eq__(self, other):
        if other == None:
            return not self.__bool__()
        elif is_text(other):
            try:
                return "".join(self) == other
            except Exception as e:
                return False
        elif is_many(other):
            return all(s == o for s, o in zip_longest(self, other))
        elif self.length() == 1:
            return self[0] == other
        elif not self:
            return False
        else:
            Log.error("do not know how to handle")

    def __bool__(self):
        try:
            NEXT(self.iteritems())()
            return True
        except Exception:
            pass

        try:
            NEXT(self.__iter__())()
            return True
        except Exception:
            return False

    __nonzero__ = __bool__

    def __iter__(self):
        if is_forward(self.type):
            if len(self.tokens) != 1:
                Log.error("not expected")

            output = list(self.tokens[0])
            for i in output:
                yield i
            return

        for r in self.tokens:
            if isinstance(r, Annotation):
                continue
            elif isinstance(r, ParseResults):
                if isinstance(r, Annotation):
                    return
                elif isinstance(r.type, Group):
                    yield r
                elif is_forward(r.type) and isinstance(forward_type(r), Group):
                    yield r
                elif not isinstance(r.type, Group):
                    for mm in r:
                        yield mm
            else:
                yield r

    def _del_item_by_index(self, index):
        for i, t in enumerate(self.tokens):
            if isinstance(t.type, (Group, Token)):
                if index < 1:
                    del self.tokens[i]
                    name = t.name
                    if name:
                        if not isinstance(t.type, Annotation):
                            self.tokens.append(Annotation(name, t.tokens))
                    return
                else:
                    index -= 1
                continue
            elif isinstance(t, Annotation):
                return
            elif index < len(t):
                t._del_item_by_index(index)
                return
            else:
                index -= len(t)

    def __delitem__(self, key):
        if isinstance(key, (int, slice)):
            Log.error("not allowed")
        else:
            self[key] = NO_RESULTS

    def __reversed__(self):
        return reversed(self.tokens)

    def iterkeys(self):
        for k, _ in self.iteritems():
            yield k

    def itervalues(self):
        for _, v in self.iteritems():
            yield v

    def iteritems(self):
        if is_forward(self.type):
            return self.tokens[0].iteritems()

        output = {}
        for tok in self.tokens:
            if isinstance(tok, ParseResults):
                if tok.name:
                    add(output, tok.name, [tok])
                    continue
                if isinstance(tok.type, Group):
                    continue
                if is_forward(tok.type) and isinstance(tok.tokens[0].type, Group):
                    continue
                for k, v in tok.iteritems():
                    add(output, k, v)
        for k, v in output.items():
            yield k, v

    if PY3:
        keys = iterkeys
        values = itervalues
        items = iteritems
    else:

        def keys(self):
            return list(self.iterkeys())

        def values(self):
            return list(self.itervalues())

        def items(self):
            return list(self.iteritems())

    def haskeys(self):
        """Since keys() returns an iterator, this method is helpful in bypassing
        code that looks for the existence of any defined results names."""
        return any((r.name) for r in self.tokens)

    def pop(self, index=-1, default=None):
        """
        Removes and returns item at specified index (default= ``last``).
        Supports both ``list`` and ``dict`` semantics for ``pop()``. If
        passed no argument or an integer argument, it will use ``list``
        semantics and pop tokens from the list of parsed tokens. If passed
        a non-integer argument (most likely a string), it will use ``dict``
        semantics and pop the corresponding value from any defined results
        names. A second default return value argument is supported, just as in
        ``dict.pop()``.
        """
        ret = self[index]
        del self[index]
        return ret if ret else default

    def get(self, key, defaultValue=None):
        """
        Returns named result matching the given key, or if there is no
        such name, then returns the given ``defaultValue``

        Similar to ``dict.get()``.
        """
        if key in self:
            return self[key]
        else:
            return defaultValue

    def __contains__(self, item):
        return bool(self[item])

    def __add__(self, other):
        return ParseResults(Group(self.type + other.type), self.tokens + other.tokens)

    def __radd__(self, other):
        if not other:  # happens when using sum() on parsers
            return self
        other = engine.CURRENT.normalize(other)
        return other + self

    def __repr__(self):
        try:
            return repr(self.tokens)
        except Exception as e:
            Log.warning("problem", cause=e)
            return "[]"

    def __data__(self):
        return [
            v.__data__() if isinstance(v, ParserElement) else v for v in self.tokens
        ]

    def __str__(self):
        if len(inspect.stack(0)) > 30:
            return "..."
        elif not self.tokens:
            return ""
        elif len(self.tokens) == 1:
            return text(self.tokens[0])
        else:
            return "[" + ", ".join(text(v) for v in self.tokens) + "]"

    def _asStringList(self):
        for t in self:
            if isinstance(t, ParseResults):
                for s in t._asStringList():
                    yield s
            else:
                yield t

    def asString(self, sep=""):
        return sep.join(self._asStringList())

    def asList(self):
        """
        Returns the parse results as a nested list of matching tokens, all converted to strings.

        Example::

            patt = OneOrMore(Word(alphas))
            result = patt.parseString("sldkj lsdkj sldkj")
            # even though the result prints in string-like form, it is actually a mo_parsing ParseResults
            print(type(result), result) # -> <class 'mo_parsing.ParseResults'> ['sldkj', 'lsdkj', 'sldkj']

            # Use asList() to create an actual list
            result_list = result.asList()
            print(type(result_list), result_list) # -> <class 'list'> ['sldkj', 'lsdkj', 'sldkj']
        """

        def internal(obj, depth):
            # RETURN AN OPEN LIST
            if depth > 60:
                Log.warning("deep!")

            if isinstance(obj, Annotation):
                return []
            elif isinstance(obj, ParseResults):
                output = []
                for t in obj.tokens:
                    inner = internal(t, depth + 1)
                    output.extend(inner)
                if isinstance(obj.type, Group):
                    return [output]
                else:
                    return output
            else:
                return [obj]

        output = internal(self, 0)
        # if isinstance(self.type, Group):
        #     return simpler(output)
        # else:
        return output

    def __copy__(self):
        """
        Returns a new copy of a :class:`ParseResults` object.
        """
        ret = ParseResults(self.type, list(self.tokens))
        return ret

    def getName(self):
        r"""
        Returns the results name for this token expression. Useful when several
        different expressions might match at a particular location.

        Example::

            integer = Word(nums)
            ssn_expr = Regex(r"\d\d\d-\d\d-\d\d\d\d")
            house_number_expr = Suppress('#') + Word(nums, alphanums)
            user_data = (Group(house_number_expr)("house_number")
                        | Group(ssn_expr)("ssn")
                        | Group(integer)("age"))
            user_info = OneOrMore(user_data)

            result = user_info.parseString("22 111-22-3333 #221B")
            for item in result:
                print(item.getName(), ':', item[0])

        prints::

            age : 22
            ssn : 111-22-3333
            house_number : 221B
        """
        if self.name:
            return self.name
        elif len(self.tokens) == 1:
            return self.tokens[0].name
        else:
            return None

    def __getnewargs__(self):
        old_parser = self.type
        parser_type = globals().get(old_parser.__class__.__name__, ParserElement)
        new_parser = parser_type(None)
        new_parser.token_name = old_parser.token_name
        return new_parser, self.tokens

    def __dir__(self):
        return dir(type(self))


def simpler(v):
    # convert an open list to object it represents
    if isinstance(v, list):
        if len(v) == 0:
            return None
        elif len(v) == 1:
            return v[0]
    return v


def add(obj, key, value):
    if not isinstance(value, list):
        Log.error("not allowed")
    if value and isinstance(value[0], list):
        Log.error("not expected")
    old_v = obj.get(key)
    if old_v is None:
        obj[key] = value
    else:
        old_v.extend(value)


class Annotation(ParseResults):
    # Append one of these to the parse results to
    # add key: value pair not found in the original text

    __slots__ = []

    def __init__(self, name, value):
        if not name:
            Log.error("expecting a name")
        if not isinstance(value, list):
            Log.error("expecting a list")
        ParseResults.__init__(self, Empty()(name), value)

    def __str__(self):
        return "{" + text(self.name) + ": " + text(self.tokens) + "}"

    def __repr__(self):
        return "Annotation(" + repr(self.name) + ", " + repr(self.tokens) + ")"


MutableMapping.register(ParseResults)
