import collections
import copy
import types

from pyparsing.exceptions import ParseFatalException, ParseException, ParseBaseException
from pyparsing.future import _MAX_INT, _ustr, basestring, RLock, _OrderedDict
from pyparsing.parse_results import ParseResults
from pyparsing.util import _trim_arity, _flatten, col, lineno


def _defaultStartDebugAction(instring, loc, expr):
    print(("Match " + _ustr(expr) + " at loc " + _ustr(loc) + "(%d,%d)" % (lineno(loc, instring), col(loc, instring))))


def _defaultSuccessDebugAction(instring, startloc, endloc, expr, toks):
    print("Matched " + _ustr(expr) + " -> " + str(toks.asList()))


def _defaultExceptionDebugAction(instring, loc, expr, exc):
    print("Exception raised:" + _ustr(exc))


class ParserElement(object):
    """Abstract base level parser element class."""
    DEFAULT_WHITE_CHARS = " \n\t\r"
    verbose_stacktrace = False

    @staticmethod
    def setDefaultWhitespaceChars( chars ):
        r"""
        Overrides the default whitespace chars

        Example::
            # default whitespace chars are space, <TAB> and newline
            OneOrMore(Word(alphas)).parseString("abc def\nghi jkl")  # -> ['abc', 'def', 'ghi', 'jkl']

            # change to just treat newline as significant
            ParserElement.setDefaultWhitespaceChars(" \t")
            OneOrMore(Word(alphas)).parseString("abc def\nghi jkl")  # -> ['abc', 'def']
        """
        ParserElement.DEFAULT_WHITE_CHARS = chars

    @staticmethod
    def inlineLiteralsUsing(cls):
        """
        Set class to be used for inclusion of string literals into a parser.

        Example::
            # default literal class used is Literal
            integer = Word(nums)
            date_str = integer("year") + '/' + integer("month") + '/' + integer("day")

            date_str.parseString("1999/12/31")  # -> ['1999', '/', '12', '/', '31']


            # change to Suppress
            ParserElement.inlineLiteralsUsing(Suppress)
            date_str = integer("year") + '/' + integer("month") + '/' + integer("day")

            date_str.parseString("1999/12/31")  # -> ['1999', '12', '31']
        """
        ParserElement._literalStringClass = cls

    def __init__( self, savelist=False ):
        self.parseAction = list()
        self.failAction = None
        #~ self.name = "<unknown>"  # don't define self.name, let subclasses try/except upcall
        self.strRepr = None
        self.resultsName = None
        self.saveAsList = savelist
        self.skipWhitespace = True
        self.whiteChars = ParserElement.DEFAULT_WHITE_CHARS
        self.copyDefaultWhiteChars = True
        self.mayReturnEmpty = False # used when checking for left-recursion
        self.keepTabs = False
        self.ignoreExprs = list()
        self.debug = False
        self.streamlined = False
        self.mayIndexError = True # used to optimize exception handling for subclasses that don't advance parse index
        self.errmsg = ""
        self.modalResults = True # used to mark results names as modal (report only last) or cumulative (list all)
        self.debugActions = ( None, None, None ) #custom debug actions
        self.re = None
        self.callPreparse = True # used to avoid redundant calls to preParse
        self.callDuringTry = False

    def copy( self ):
        """
        Make a copy of this C{ParserElement}.  Useful for defining different parse actions
        for the same parsing pattern, using copies of the original parse element.

        Example::
            integer = Word(nums).setParseAction(lambda toks: int(toks[0]))
            integerK = integer.copy().addParseAction(lambda toks: toks[0]*1024) + Suppress("K")
            integerM = integer.copy().addParseAction(lambda toks: toks[0]*1024*1024) + Suppress("M")

            print(OneOrMore(integerK | integerM | integer).parseString("5K 100 640K 256M"))
        prints::
            [5120, 100, 655360, 268435456]
        Equivalent form of C{expr.copy()} is just C{expr()}::
            integerM = integer().addParseAction(lambda toks: toks[0]*1024*1024) + Suppress("M")
        """
        cpy = copy.copy( self )
        cpy.parseAction = self.parseAction[:]
        cpy.ignoreExprs = self.ignoreExprs[:]
        if self.copyDefaultWhiteChars:
            cpy.whiteChars = ParserElement.DEFAULT_WHITE_CHARS
        return cpy

    def setName( self, name ):
        """
        Define name for this expression, makes debugging and exception messages clearer.

        Example::
            Word(nums).parseString("ABC")  # -> Exception: Expected W:(0123...) (at char 0), (line:1, col:1)
            Word(nums).setName("integer").parseString("ABC")  # -> Exception: Expected integer (at char 0), (line:1, col:1)
        """
        self.name = name
        self.errmsg = "Expected " + self.name
        if hasattr(self,"exception"):
            self.exception.msg = self.errmsg
        return self

    def setResultsName( self, name, listAllMatches=False ):
        """
        Define name for referencing matching tokens as a nested attribute
        of the returned parse results.
        NOTE: this returns a *copy* of the original C{ParserElement} object;
        this is so that the client can define a basic element, such as an
        integer, and reference it in multiple places with different names.

        You can also set results names using the abbreviated syntax,
        C{expr("name")} in place of C{expr.setResultsName("name")} -
        see L{I{__call__}<__call__>}.

        Example::
            date_str = (integer.setResultsName("year") + '/'
                        + integer.setResultsName("month") + '/'
                        + integer.setResultsName("day"))

            # equivalent form:
            date_str = integer("year") + '/' + integer("month") + '/' + integer("day")
        """
        newself = self.copy()
        if name.endswith("*"):
            name = name[:-1]
            listAllMatches=True
        newself.resultsName = name
        newself.modalResults = not listAllMatches
        return newself

    def setBreak(self,breakFlag = True):
        """Method to invoke the Python pdb debugger when this element is
           about to be parsed. Set C{breakFlag} to True to enable, False to
           disable.
        """
        if breakFlag:
            _parseMethod = self._parse
            def breaker(instring, loc, doActions=True, callPreParse=True):
                import pdb
                pdb.set_trace()
                return _parseMethod( instring, loc, doActions, callPreParse )
            breaker._originalParseMethod = _parseMethod
            self._parse = breaker
        else:
            if hasattr(self._parse,"_originalParseMethod"):
                self._parse = self._parse._originalParseMethod
        return self

    def setParseAction( self, *fns, **kwargs ):
        """
        Define one or more actions to perform when successfully matching parse element definition.
        Parse action fn is a callable method with 0-3 arguments, called as C{fn(s,loc,toks)},
        C{fn(loc,toks)}, C{fn(toks)}, or just C{fn()}, where:
         - s   = the original string being parsed (see note below)
         - loc = the location of the matching substring
         - toks = a list of the matched tokens, packaged as a C{L{ParseResults}} object
        If the functions in fns modify the tokens, they can return them as the return
        value from fn, and the modified list of tokens will replace the original.
        Otherwise, fn does not need to return any value.

        Optional keyword arguments:
         - callDuringTry = (default=C{False}) indicate if parse action should be run during lookaheads and alternate testing

        Note: the default parsing behavior is to expand tabs in the input string
        before starting the parsing process.  See L{I{parseString}<parseString>} for more information
        on parsing strings containing C{<TAB>}s, and suggested methods to maintain a
        consistent view of the parsed string, the parse location, and line and column
        positions within the parsed string.

        Example::
            integer = Word(nums)
            date_str = integer + '/' + integer + '/' + integer

            date_str.parseString("1999/12/31")  # -> ['1999', '/', '12', '/', '31']

            # use parse action to convert to ints at parse time
            integer = Word(nums).setParseAction(lambda toks: int(toks[0]))
            date_str = integer + '/' + integer + '/' + integer

            # note that integer fields are now ints, not strings
            date_str.parseString("1999/12/31")  # -> [1999, '/', 12, '/', 31]
        """
        self.parseAction = list(map(_trim_arity, list(fns)))
        self.callDuringTry = kwargs.get("callDuringTry", False)
        return self

    def addParseAction( self, *fns, **kwargs ):
        """
        Add one or more parse actions to expression's list of parse actions. See L{I{setParseAction}<setParseAction>}.

        See examples in L{I{copy}<copy>}.
        """
        self.parseAction += list(map(_trim_arity, list(fns)))
        self.callDuringTry = self.callDuringTry or kwargs.get("callDuringTry", False)
        return self

    def addCondition(self, *fns, **kwargs):
        """Add a boolean predicate function to expression's list of parse actions. See
        L{I{setParseAction}<setParseAction>} for function call signatures. Unlike C{setParseAction},
        functions passed to C{addCondition} need to return boolean success/fail of the condition.

        Optional keyword arguments:
         - message = define a custom message to be used in the raised exception
         - fatal   = if True, will raise ParseFatalException to stop parsing immediately; otherwise will raise ParseException

        Example::
            integer = Word(nums).setParseAction(lambda toks: int(toks[0]))
            year_int = integer.copy()
            year_int.addCondition(lambda toks: toks[0] >= 2000, message="Only support years 2000 and later")
            date_str = year_int + '/' + integer + '/' + integer

            result = date_str.parseString("1999/12/31")  # -> Exception: Only support years 2000 and later (at char 0), (line:1, col:1)
        """
        msg = kwargs.get("message", "failed user-defined condition")
        exc_type = ParseFatalException if kwargs.get("fatal", False) else ParseException
        for fn in fns:
            def pa(s,l,t):
                if not bool(_trim_arity(fn)(s,l,t)):
                    raise exc_type(s,l,msg)
            self.parseAction.append(pa)
        self.callDuringTry = self.callDuringTry or kwargs.get("callDuringTry", False)
        return self

    def setFailAction( self, fn ):
        """Define action to perform if parsing fails at this expression.
           Fail acton fn is a callable function that takes the arguments
           C{fn(s,loc,expr,err)} where:
            - s = string being parsed
            - loc = location where expression match was attempted and failed
            - expr = the parse expression that failed
            - err = the exception thrown
           The function returns no value.  It may throw C{L{ParseFatalException}}
           if it is desired to stop parsing immediately."""
        self.failAction = fn
        return self

    def _skipIgnorables( self, instring, loc ):
        exprsFound = True
        while exprsFound:
            exprsFound = False
            for e in self.ignoreExprs:
                try:
                    while 1:
                        loc,dummy = e._parse( instring, loc )
                        exprsFound = True
                except ParseException:
                    pass
        return loc

    def preParse( self, instring, loc ):
        if self.ignoreExprs:
            loc = self._skipIgnorables( instring, loc )

        if self.skipWhitespace:
            wt = self.whiteChars
            instrlen = len(instring)
            while loc < instrlen and instring[loc] in wt:
                loc += 1

        return loc

    def parseImpl( self, instring, loc, doActions=True ):
        return loc, []

    def postParse( self, instring, loc, tokenlist ):
        return tokenlist

    #~ @profile
    def _parseNoCache( self, instring, loc, doActions=True, callPreParse=True ):
        debugging = ( self.debug ) #and doActions )

        if debugging or self.failAction:
            #~ print ("Match",self,"at loc",loc,"(%d,%d)" % ( lineno(loc,instring), col(loc,instring) ))
            if (self.debugActions[0] ):
                self.debugActions[0]( instring, loc, self )
            if callPreParse and self.callPreparse:
                preloc = self.preParse( instring, loc )
            else:
                preloc = loc
            tokensStart = preloc
            try:
                try:
                    loc,tokens = self.parseImpl( instring, preloc, doActions )
                except IndexError:
                    raise ParseException( instring, len(instring), self.errmsg, self )
            except ParseBaseException as err:
                #~ print ("Exception raised:", err)
                if self.debugActions[2]:
                    self.debugActions[2]( instring, tokensStart, self, err )
                if self.failAction:
                    self.failAction( instring, tokensStart, self, err )
                raise
        else:
            if callPreParse and self.callPreparse:
                preloc = self.preParse( instring, loc )
            else:
                preloc = loc
            tokensStart = preloc
            if self.mayIndexError or preloc >= len(instring):
                try:
                    loc,tokens = self.parseImpl( instring, preloc, doActions )
                except IndexError:
                    raise ParseException( instring, len(instring), self.errmsg, self )
            else:
                loc,tokens = self.parseImpl( instring, preloc, doActions )

        tokens = self.postParse( instring, loc, tokens )

        retTokens = ParseResults( tokens, self, self.resultsName, asList=self.saveAsList, modal=self.modalResults )
        if self.parseAction and (doActions or self.callDuringTry):
            if debugging:
                try:
                    for fn in self.parseAction:
                        try:
                            tokens = fn( instring, tokensStart, retTokens )
                        except IndexError as parse_action_exc:
                            exc = ParseException("exception raised in parse action")
                            exc.__cause__ = parse_action_exc
                            raise exc

                        if tokens is not None and tokens is not retTokens:
                            retTokens = ParseResults( tokens, 
                                                      self,
                                                      self.resultsName,
                                                      asList=self.saveAsList and isinstance(tokens,(ParseResults,list)),
                                                      modal=self.modalResults )
                except ParseBaseException as err:
                    #~ print "Exception raised in user parse action:", err
                    if (self.debugActions[2] ):
                        self.debugActions[2]( instring, tokensStart, self, err )
                    raise
            else:
                for fn in self.parseAction:
                    try:
                        tokens = fn( instring, tokensStart, retTokens )
                    except IndexError as parse_action_exc:
                        exc = ParseException("exception raised in parse action")
                        exc.__cause__ = parse_action_exc
                        raise exc

                    if tokens is not None and tokens is not retTokens:
                        retTokens = ParseResults( tokens, 
                                                  self,
                                                  self.resultsName,
                                                  asList=self.saveAsList and isinstance(tokens,(ParseResults,list)),
                                                  modal=self.modalResults )
        if debugging:
            #~ print ("Matched",self,"->",retTokens.asList())
            if (self.debugActions[1] ):
                self.debugActions[1]( instring, tokensStart, loc, self, retTokens )

        return loc, retTokens

    def tryParse( self, instring, loc ):
        try:
            return self._parse( instring, loc, doActions=False )[0]
        except ParseFatalException:
            raise ParseException( instring, loc, self.errmsg, self)

    def canParseNext(self, instring, loc):
        try:
            self.tryParse(instring, loc)
        except (ParseException, IndexError):
            return False
        else:
            return True

    class _UnboundedCache(object):
        def __init__(self):
            cache = {}
            self.not_in_cache = not_in_cache = object()

            def get(self, key):
                return cache.get(key, not_in_cache)

            def set(self, key, value):
                cache[key] = value

            def clear(self):
                cache.clear()

            def cache_len(self):
                return len(cache)

            self.get = types.MethodType(get, self)
            self.set = types.MethodType(set, self)
            self.clear = types.MethodType(clear, self)
            self.__len__ = types.MethodType(cache_len, self)

    if _OrderedDict is not None:
        class _FifoCache(object):
            def __init__(self, size):
                self.not_in_cache = not_in_cache = object()

                cache = _OrderedDict()

                def get(self, key):
                    return cache.get(key, not_in_cache)

                def set(self, key, value):
                    cache[key] = value
                    while len(cache) > size:
                        try:
                            cache.popitem(False)
                        except KeyError:
                            pass

                def clear(self):
                    cache.clear()

                def cache_len(self):
                    return len(cache)

                self.get = types.MethodType(get, self)
                self.set = types.MethodType(set, self)
                self.clear = types.MethodType(clear, self)
                self.__len__ = types.MethodType(cache_len, self)

    else:
        class _FifoCache(object):
            def __init__(self, size):
                self.not_in_cache = not_in_cache = object()

                cache = {}
                key_fifo = collections.deque([], size)

                def get(self, key):
                    return cache.get(key, not_in_cache)

                def set(self, key, value):
                    cache[key] = value
                    while len(key_fifo) > size:
                        cache.pop(key_fifo.popleft(), None)
                    key_fifo.append(key)

                def clear(self):
                    cache.clear()
                    key_fifo.clear()

                def cache_len(self):
                    return len(cache)

                self.get = types.MethodType(get, self)
                self.set = types.MethodType(set, self)
                self.clear = types.MethodType(clear, self)
                self.__len__ = types.MethodType(cache_len, self)

    # argument cache for optimizing repeated calls when backtracking through recursive expressions
    packrat_cache = {} # this is set later by enabledPackrat(); this is here so that resetCache() doesn't fail
    packrat_cache_lock = RLock()
    packrat_cache_stats = [0, 0]

    # this method gets repeatedly called during backtracking with the same arguments -
    # we can cache these arguments and save ourselves the trouble of re-parsing the contained expression
    def _parseCache( self, instring, loc, doActions=True, callPreParse=True ):
        HIT, MISS = 0, 1
        lookup = (self, instring, loc, callPreParse, doActions)
        with ParserElement.packrat_cache_lock:
            cache = ParserElement.packrat_cache
            value = cache.get(lookup)
            if value is cache.not_in_cache:
                ParserElement.packrat_cache_stats[MISS] += 1
                try:
                    value = self._parseNoCache(instring, loc, doActions, callPreParse)
                except ParseBaseException as pe:
                    # cache a copy of the exception, without the traceback
                    cache.set(lookup, pe.__class__(*pe.args))
                    raise
                else:
                    cache.set(lookup, (value[0], value[1].copy()))
                    return value
            else:
                ParserElement.packrat_cache_stats[HIT] += 1
                if isinstance(value, Exception):
                    raise value
                return (value[0], value[1].copy())

    _parse = _parseNoCache

    @staticmethod
    def resetCache():
        ParserElement.packrat_cache.clear()
        ParserElement.packrat_cache_stats[:] = [0] * len(ParserElement.packrat_cache_stats)

    _packratEnabled = False
    @staticmethod
    def enablePackrat(cache_size_limit=128):
        """Enables "packrat" parsing, which adds memoizing to the parsing logic.
           Repeated parse attempts at the same string location (which happens
           often in many complex grammars) can immediately return a cached value,
           instead of re-executing parsing/validating code.  Memoizing is done of
           both valid results and parsing exceptions.

           Parameters:
            - cache_size_limit - (default=C{128}) - if an integer value is provided
              will limit the size of the packrat cache; if None is passed, then
              the cache size will be unbounded; if 0 is passed, the cache will
              be effectively disabled.

           This speedup may break existing programs that use parse actions that
           have side-effects.  For this reason, packrat parsing is disabled when
           you first import pyparsing.  To activate the packrat feature, your
           program must call the class method C{ParserElement.enablePackrat()}.  If
           your program uses C{psyco} to "compile as you go", you must call
           C{enablePackrat} before calling C{psyco.full()}.  If you do not do this,
           Python will crash.  For best results, call C{enablePackrat()} immediately
           after importing pyparsing.

           Example::
               import pyparsing
               pyparsing.ParserElement.enablePackrat()
        """
        if not ParserElement._packratEnabled:
            ParserElement._packratEnabled = True
            if cache_size_limit is None:
                ParserElement.packrat_cache = ParserElement._UnboundedCache()
            else:
                ParserElement.packrat_cache = ParserElement._FifoCache(cache_size_limit)
            ParserElement._parse = ParserElement._parseCache

    def scanString( self, instring, maxMatches=_MAX_INT, overlap=False ):
        """
        Scan the input string for expression matches.  Each match will return the
        matching tokens, start location, and end location.  May be called with optional
        C{maxMatches} argument, to clip scanning after 'n' matches are found.  If
        C{overlap} is specified, then overlapping matches will be reported.

        Note that the start and end locations are reported relative to the string
        being parsed.  See L{I{parseString}<parseString>} for more information on parsing
        strings with embedded tabs.

        Example::
            source = "sldjf123lsdjjkf345sldkjf879lkjsfd987"
            print(source)
            for tokens,start,end in Word(alphas).scanString(source):
                print(' '*start + '^'*(end-start))
                print(' '*start + tokens[0])

        prints::

            sldjf123lsdjjkf345sldkjf879lkjsfd987
            ^^^^^
            sldjf
                    ^^^^^^^
                    lsdjjkf
                              ^^^^^^
                              sldkjf
                                       ^^^^^^
                                       lkjsfd
        """
        if not self.streamlined:
            self.streamline()
        for e in self.ignoreExprs:
            e.streamline()

        if not self.keepTabs:
            instring = _ustr(instring).expandtabs()
        instrlen = len(instring)
        loc = 0
        preparseFn = self.preParse
        parseFn = self._parse
        ParserElement.resetCache()
        matches = 0
        try:
            while loc <= instrlen and matches < maxMatches:
                try:
                    preloc = preparseFn( instring, loc )
                    nextLoc,tokens = parseFn( instring, preloc, callPreParse=False )
                except ParseException:
                    loc = preloc+1
                else:
                    if nextLoc > loc:
                        matches += 1
                        yield tokens, preloc, nextLoc
                        if overlap:
                            nextloc = preparseFn( instring, loc )
                            if nextloc > loc:
                                loc = nextLoc
                            else:
                                loc += 1
                        else:
                            loc = nextLoc
                    else:
                        loc = preloc+1
        except ParseBaseException as exc:
            if ParserElement.verbose_stacktrace:
                raise
            else:
                # catch and re-raise exception from here, clears out pyparsing internal stack trace
                raise exc

    def transformString( self, instring ):
        """
        Extension to C{L{scanString}}, to modify matching text with modified tokens that may
        be returned from a parse action.  To use C{transformString}, define a grammar and
        attach a parse action to it that modifies the returned token list.
        Invoking C{transformString()} on a target string will then scan for matches,
        and replace the matched text patterns according to the logic in the parse
        action.  C{transformString()} returns the resulting transformed string.

        Example::
            wd = Word(alphas)
            wd.setParseAction(lambda toks: toks[0].title())

            print(wd.transformString("now is the winter of our discontent made glorious summer by this sun of york."))
        Prints::
            Now Is The Winter Of Our Discontent Made Glorious Summer By This Sun Of York.
        """
        out = []
        lastE = 0
        # force preservation of <TAB>s, to minimize unwanted transformation of string, and to
        # keep string locs straight between transformString and scanString
        self.keepTabs = True
        try:
            for t,s,e in self.scanString( instring ):
                out.append( instring[lastE:s] )
                if t:
                    if isinstance(t,ParseResults):
                        out += t.asList()
                    elif isinstance(t,list):
                        out += t
                    else:
                        out.append(t)
                lastE = e
            out.append(instring[lastE:])
            out = [o for o in out if o]
            return "".join(map(_ustr,_flatten(out)))
        except ParseBaseException as exc:
            if ParserElement.verbose_stacktrace:
                raise
            else:
                # catch and re-raise exception from here, clears out pyparsing internal stack trace
                raise exc

    def searchString( self, instring, maxMatches=_MAX_INT ):
        """
        Another extension to C{L{scanString}}, simplifying the access to the tokens found
        to match the given parse expression.  May be called with optional
        C{maxMatches} argument, to clip searching after 'n' matches are found.

        Example::
            # a capitalized word starts with an uppercase letter, followed by zero or more lowercase letters
            cap_word = Word(alphas.upper(), alphas.lower())

            print(cap_word.searchString("More than Iron, more than Lead, more than Gold I need Electricity"))

            # the sum() builtin can be used to merge results into a single ParseResults object
            print(sum(cap_word.searchString("More than Iron, more than Lead, more than Gold I need Electricity")))
        prints::
            [['More'], ['Iron'], ['Lead'], ['Gold'], ['I'], ['Electricity']]
            ['More', 'Iron', 'Lead', 'Gold', 'I', 'Electricity']
        """
        try:
            return ParseResults([ t for t,s,e in self.scanString( instring, maxMatches ) ], self)
        except ParseBaseException as exc:
            if ParserElement.verbose_stacktrace:
                raise
            else:
                # catch and re-raise exception from here, clears out pyparsing internal stack trace
                raise exc

    def split(self, instring, maxsplit=_MAX_INT, includeSeparators=False):
        """
        Generator method to split a string using the given expression as a separator.
        May be called with optional C{maxsplit} argument, to limit the number of splits;
        and the optional C{includeSeparators} argument (default=C{False}), if the separating
        matching text should be included in the split results.

        Example::
            punc = oneOf(list(".,;:/-!?"))
            print(list(punc.split("This, this?, this sentence, is badly punctuated!")))
        prints::
            ['This', ' this', '', ' this sentence', ' is badly punctuated', '']
        """
        splits = 0
        last = 0
        for t,s,e in self.scanString(instring, maxMatches=maxsplit):
            yield instring[last:s]
            if includeSeparators:
                yield t[0]
            last = e
        yield instring[last:]

    def leaveWhitespace( self ):
        """
        Disables the skipping of whitespace before matching the characters in the
        C{ParserElement}'s defined pattern.  This is normally only used internally by
        the pyparsing module, but may be needed in some whitespace-sensitive grammars.
        """
        self.skipWhitespace = False
        return self

    def setWhitespaceChars( self, chars ):
        """
        Overrides the default whitespace chars
        """
        self.skipWhitespace = True
        self.whiteChars = chars
        self.copyDefaultWhiteChars = False
        return self

    def parseWithTabs( self ):
        """
        Overrides default behavior to expand C{<TAB>}s to spaces before parsing the input string.
        Must be called before C{parseString} when the input grammar contains elements that
        match C{<TAB>} characters.
        """
        self.keepTabs = True
        return self

    def setDebugActions( self, startAction, successAction, exceptionAction ):
        """
        Enable display of debugging messages while doing pattern matching.
        """
        self.debugActions = (startAction or _defaultStartDebugAction,
                             successAction or _defaultSuccessDebugAction,
                             exceptionAction or _defaultExceptionDebugAction)
        self.debug = True
        return self

    def setDebug( self, flag=True ):
        """
        Enable display of debugging messages while doing pattern matching.
        Set C{flag} to True to enable, False to disable.

        Example::
            wd = Word(alphas).setName("alphaword")
            integer = Word(nums).setName("numword")
            term = wd | integer

            # turn on debugging for wd
            wd.setDebug()

            OneOrMore(term).parseString("abc 123 xyz 890")

        prints::
            Match alphaword at loc 0(1,1)
            Matched alphaword -> ['abc']
            Match alphaword at loc 3(1,4)
            Exception raised:Expected alphaword (at char 4), (line:1, col:5)
            Match alphaword at loc 7(1,8)
            Matched alphaword -> ['xyz']
            Match alphaword at loc 11(1,12)
            Exception raised:Expected alphaword (at char 12), (line:1, col:13)
            Match alphaword at loc 15(1,16)
            Exception raised:Expected alphaword (at char 15), (line:1, col:16)

        The output shown is that produced by the default debug actions - custom debug actions can be
        specified using L{setDebugActions}. Prior to attempting
        to match the C{wd} expression, the debugging message C{"Match <exprname> at loc <n>(<line>,<col>)"}
        is shown. Then if the parse succeeds, a C{"Matched"} message is shown, or an C{"Exception raised"}
        message is shown. Also note the use of L{setName} to assign a human-readable name to the expression,
        which makes debugging and exception messages easier to understand - for instance, the default
        name created for the C{Word} expression without calling C{setName} is C{"W:(ABCD...)"}.
        """
        if flag:
            self.setDebugActions( _defaultStartDebugAction, _defaultSuccessDebugAction, _defaultExceptionDebugAction )
        else:
            self.debug = False
        return self

    def __str__( self ):
        return self.name

    def __repr__( self ):
        return _ustr(self)

    def streamline( self ):
        self.streamlined = True
        self.strRepr = None
        return self

    def checkRecursion( self, parseElementList ):
        pass

    def validate( self, validateTrace=[] ):
        """
        Check defined expressions for valid structure, check for infinite recursive definitions.
        """
        self.checkRecursion( [] )

    def parseFile( self, file_or_filename, parseAll=False ):
        """
        Execute the parse expression on the given file or filename.
        If a filename is specified (instead of a file object),
        the entire file is opened, read, and closed before parsing.
        """
        try:
            file_contents = file_or_filename.read()
        except AttributeError:
            with open(file_or_filename, "r") as f:
                file_contents = f.read()
        try:
            return self.parseString(file_contents, parseAll)
        except ParseBaseException as exc:
            if ParserElement.verbose_stacktrace:
                raise
            else:
                # catch and re-raise exception from here, clears out pyparsing internal stack trace
                raise exc

    def __eq__(self,other):
        if isinstance(other, ParserElement):
            return self is other or vars(self) == vars(other)
        elif isinstance(other, basestring):
            return self.matches(other)
        else:
            return super(ParserElement,self)==other

    def __ne__(self,other):
        return not (self == other)

    def __hash__(self):
        return hash(id(self))

    def __req__(self,other):
        return self == other

    def __rne__(self,other):
        return not (self == other)

    def matches(self, testString, parseAll=True):
        """
        Method for quick testing of a parser against a test string. Good for simple
        inline microtests of sub expressions while building up larger parser.

        Parameters:
         - testString - to test against this expression for a match
         - parseAll - (default=C{True}) - flag to pass to C{L{parseString}} when running tests

        Example::
            expr = Word(nums)
            assert expr.matches("100")
        """
        try:
            self.parseString(_ustr(testString), parseAll=parseAll)
            return True
        except ParseBaseException:
            return False

