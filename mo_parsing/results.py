# encoding: utf-8
from collections import MutableMapping

from mo_dots import is_many
from mo_future import is_text, text, PY3
from mo_logs import Log

from mo_parsing import engine

Suppress, ParserElement, Forward, Group, Dict, Token, Empty = [None] * 7

_get = object.__getattribute__


class ParseResults(object):
    """Structured parse results, to provide multiple means of access to
    the parsed data:

       - as a list (``len(results)``)
       - by list index (``results[0], results[1]``, etc.)
       - by attribute (``results.<token_name>`` - see :class:`ParserElement.set_token_name`)

    Example::(pars

        integer = Word(nums)
        date_str = (integer.set_token_name("year") + '/'
                        + integer.set_token_name("month") + '/'
                        + integer.set_token_name("day"))
        # equivalent form:
        # date_str = integer("year") + '/' + integer("month") + '/' + integer("day")

        # parseString returns a ParseResults object
        result = date_str.parseString("1999/12/31")

        def test(s, fn=repr):
            print("%s -> %s" % (s, fn(eval(s))))
        test("list(result)")
        test("result[0]")
        test("result['month']")
        test("result.day")
        test("'month' in result")
        test("'minutes' in result")
        test("result", str)

    prints::

        list(result) -> ['1999', '/', '12', '/', '31']
        result[0] -> '1999'
        result['month'] -> '12'
        result.day -> '31'
        'month' in result -> True
        'minutes' in result -> False
        result -> ['1999', '/', '12', '/', '31']
        - day: 31
        - month: 12
        - year: 1999
    """

    __slots__ = [
        "tokens",
        "type",
    ]

    @property
    def name(self):
        return self.type.token_name

    # Performance tuning: we construct a *lot* of these, so keep this
    # constructor as small and fast as possible
    def __init__(self, result_type, toklist=None):
        if not isinstance(result_type, ParserElement):
            Log.error("not expected")
        if isinstance(result_type, Forward):
            Log.error("not expected")
        if isinstance(toklist, ParseResults) or not isinstance(toklist, (list, tuple)):
            Log.error("no longer accepted")

        self.tokens = toklist
        self.type = result_type

    def _get_item_by_name(self, i):
        # return open list of (modal, value) pairs
        # modal==True means only the last value is relevant
        for tok in self.tokens:
            if isinstance(tok, ParseResults):
                name = tok.name
                if name == i:
                    if isinstance(tok.type, Group):
                        yield tok
                    else:
                        for t in tok.tokens:
                            yield t
                    continue
                elif isinstance(tok.type, Group):
                    continue
                elif name:
                    continue
                for f in tok._get_item_by_name(i):
                    yield f

    def __getitem__(self, i):
        if isinstance(i, int):
            if i < 0:
                i = len(self) + i
            for ii, v in enumerate(self):
                if i == ii:
                    return v
        elif isinstance(i, slice):
            return list(iter(self))[i]
        else:
            if self.name == i:
                return self

            values = list(self._get_item_by_name(i))
            if len(values) == 1:
                return values[0]
            return ParseResults(self.type, values)

    def __setitem__(self, k, v):
        if isinstance(k, (slice, int)):
            Log.error("do not know how to handle")
        else:
            for i, vv in enumerate(self.tokens):
                if isinstance(vv, ParseResults) and vv.name == k:
                    self.tokens[i] = v
                    break
            else:
                self.tokens.append(Annotation(k, [v]))

    def __contains__(self, k):
        return any((r.name) == k for r in self.tokens)

    def __len__(self):
        if isinstance(self.type, Group):
            if not self.tokens:
                return 0
            return len(self.tokens[0])
        else:
            return sum(1 for t in self)

    def __eq__(self, other):
        if other == None:
            return len(self) == 0
        elif is_text(other):
            try:
                return "".join(self) == other
            except Exception as e:
                return False
        elif is_many(other):
            return all(s == o for s, o in zip(self.other))
        elif len(self) == 1:
            return self[0] == other
        else:
            Log.error("do not know how to handle")

    def __bool__(self):
        return not not self.tokens

    __nonzero__ = __bool__

    def __iter__(self):
        if isinstance(self, Annotation):
            return
        else:
            for r in self.tokens:
                if isinstance(r, ParseResults):
                    if isinstance(r, Annotation):
                        return
                    elif isinstance(r.type, Group):
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
            if key == self.name:
                new_type = self.type.copy()
                new_type.token_name = None
                self.type = new_type
                return
            for i, t in enumerate(self.tokens):
                name = t.name
                if name == key:
                    new_type = t.type.copy()
                    new_type.token_name = None
                    t.type = new_type
                    return
                elif not isinstance(t, ParseResults):
                    pass
                elif isinstance(t.type, (Group, Token)):
                    pass
                else:
                    del t[key]

    def __reversed__(self):
        return reversed(self.tokens)

    def iterkeys(self):
        for k, _ in self.iteritems():
            yield k

    def itervalues(self):
        for _, v in self.iteritems():
            yield v

    def iteritems(self):
        output = {}
        for r in self.tokens:
            if isinstance(r, ParseResults):
                name = r.name
                if name:
                    add(output, name, [r])
                    continue
                if isinstance(r.type, Group):
                    continue
                for k, v in r.iteritems():
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

        Example::

            def remove_first(tokens):
                tokens.pop(0)
            print(OneOrMore(Word(nums)).parseString("0 123 321")) # -> ['0', '123', '321']
            print(OneOrMore(Word(nums)).addParseAction(remove_first).parseString("0 123 321")) # -> ['123', '321']

            label = Word(alphas)
            patt = label("LABEL") + OneOrMore(Word(nums))
            print(patt.parseString("AAB 123 321"))

            # Use pop() in a parse action to remove named result (note that corresponding value is not
            # removed from list form of results)
            def remove_LABEL(tokens):
                tokens.pop("LABEL")
                return tokens
            patt.addParseAction(remove_LABEL)
            print(patt.parseString("AAB 123 321"))

        prints::

            ['AAB', '123', '321']
            - LABEL: AAB

            ['AAB', '123', '321']
        """
        ret = self[index]
        del self[index]
        return ret if ret else default

    def get(self, key, defaultValue=None):
        """
        Returns named result matching the given key, or if there is no
        such name, then returns the given ``defaultValue`` or ``None`` if no
        ``defaultValue`` is specified.

        Similar to ``dict.get()``.

        Example::

            integer = Word(nums)
            date_str = integer("year") + '/' + integer("month") + '/' + integer("day")

            result = date_str.parseString("1999/12/31")
            print(result.get("year")) # -> '1999'
            print(result.get("hour", "not specified")) # -> 'not specified'
            print(result.get("hour")) # -> None
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
        if not self.tokens:
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

    def __lookup(self, sub):
        for name, value in self.tokens:
            if sub is value:
                return name
        return None

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
        if not isinstance(value, list):
            Log.error("expecting a list")
        ParseResults.__init__(self, Empty()(name), value)

    def __repr__(self):
        return "{" + (self.name) + ": ...}"


MutableMapping.register(ParseResults)
