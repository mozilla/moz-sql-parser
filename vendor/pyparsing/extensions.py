import warnings

from pyparsing import StringEnd, And, ZeroOrMore, OneOrMore, Optional, Or, Each, NotAny, Suppress, MatchFirst, Literal, line, ParserElement, Empty
from pyparsing.exceptions import ParseFatalException, ParseBaseException
from pyparsing.future import basestring
from pyparsing.util import col


class _ExtendedParserElement(ParserElement):

    def parseString( self, instring, parseAll=False ):
        """
        Execute the parse expression with the given string.
        This is the main interface to the client code, once the complete
        expression has been built.

        If you want the grammar to require that the entire input string be
        successfully parsed, then set C{parseAll} to True (equivalent to ending
        the grammar with C{L{StringEnd()}}).

        Note: C{parseString} implicitly calls C{expandtabs()} on the input string,
        in order to report proper column numbers in parse actions.
        If the input string contains tabs and
        the grammar uses parse actions that use the C{loc} argument to index into the
        string being parsed, you can ensure you have a consistent view of the input
        string by:
         - calling C{parseWithTabs} on your grammar before calling C{parseString}
           (see L{I{parseWithTabs}<parseWithTabs>})
         - define your parse action using the full C{(s,loc,toks)} signature, and
           reference the input string using the parse action's C{s} argument
         - explictly expand the tabs in your input string before calling
           C{parseString}

        Example::
            Word('a').parseString('aaaaabaaa')  # -> ['aaaaa']
            Word('a').parseString('aaaaabaaa', parseAll=True)  # -> Exception: Expected end of text
        """
        ParserElement.resetCache()
        if not self.streamlined:
            self.streamline()
            #~ self.saveAsList = True
        for e in self.ignoreExprs:
            e.streamline()
        if not self.keepTabs:
            instring = instring.expandtabs()
        try:
            loc, tokens = self._parse( instring, 0 )
            if parseAll:
                loc = self.preParse( instring, loc )
                se = Empty() + StringEnd()
                se._parse( instring, loc )
        except ParseBaseException as exc:
            if ParserElement.verbose_stacktrace:
                raise
            else:
                # catch and re-raise exception from here, clears out pyparsing internal stack trace
                raise exc
        else:
            return tokens



    def __add__(self, other ):
        """
        Implementation of + operator - returns C{L{And}}. Adding strings to a ParserElement
        converts them to L{Literal}s by default.

        Example::
            greet = Word(alphas) + "," + Word(alphas) + "!"
            hello = "Hello, World!"
            print (hello, "->", greet.parseString(hello))
        Prints::
            Hello, World! -> ['Hello', ',', 'World', '!']
        """
        if isinstance( other, basestring ):
            other = ParserElement._literalStringClass( other )
        if not isinstance( other, ParserElement ):
            warnings.warn("Cannot combine element of type %s with ParserElement" % type(other),
                    SyntaxWarning, stacklevel=2)
            return None
        return And( [ self, other ] )

    def __radd__(self, other ):
        """
        Implementation of + operator when left operand is not a C{L{ParserElement}}
        """
        if isinstance( other, basestring ):
            other = ParserElement._literalStringClass( other )
        if not isinstance( other, ParserElement ):
            warnings.warn("Cannot combine element of type %s with ParserElement" % type(other),
                    SyntaxWarning, stacklevel=2)
            return None
        return other + self

    def __sub__(self, other):
        """
        Implementation of - operator, returns C{L{And}} with error stop
        """
        if isinstance( other, basestring ):
            other = ParserElement._literalStringClass( other )
        if not isinstance( other, ParserElement ):
            warnings.warn("Cannot combine element of type %s with ParserElement" % type(other),
                    SyntaxWarning, stacklevel=2)
            return None
        return self + And._ErrorStop() + other

    def __rsub__(self, other ):
        """
        Implementation of - operator when left operand is not a C{L{ParserElement}}
        """
        if isinstance( other, basestring ):
            other = ParserElement._literalStringClass( other )
        if not isinstance( other, ParserElement ):
            warnings.warn("Cannot combine element of type %s with ParserElement" % type(other),
                    SyntaxWarning, stacklevel=2)
            return None
        return other - self

    def __mul__(self,other):
        """
        Implementation of * operator, allows use of C{expr * 3} in place of
        C{expr + expr + expr}.  Expressions may also me multiplied by a 2-integer
        tuple, similar to C{{min,max}} multipliers in regular expressions.  Tuples
        may also include C{None} as in:
         - C{expr*(n,None)} or C{expr*(n,)} is equivalent
              to C{expr*n + L{ZeroOrMore}(expr)}
              (read as "at least n instances of C{expr}")
         - C{expr*(None,n)} is equivalent to C{expr*(0,n)}
              (read as "0 to n instances of C{expr}")
         - C{expr*(None,None)} is equivalent to C{L{ZeroOrMore}(expr)}
         - C{expr*(1,None)} is equivalent to C{L{OneOrMore}(expr)}

        Note that C{expr*(None,n)} does not raise an exception if
        more than n exprs exist in the input stream; that is,
        C{expr*(None,n)} does not enforce a maximum number of expr
        occurrences.  If this behavior is desired, then write
        C{expr*(None,n) + ~expr}
        """
        if isinstance(other,int):
            minElements, optElements = other,0
        elif isinstance(other,tuple):
            other = (other + (None, None))[:2]
            if other[0] is None:
                other = (0, other[1])
            if isinstance(other[0],int) and other[1] is None:
                if other[0] == 0:
                    return ZeroOrMore(self)
                if other[0] == 1:
                    return OneOrMore(self)
                else:
                    return self*other[0] + ZeroOrMore(self)
            elif isinstance(other[0],int) and isinstance(other[1],int):
                minElements, optElements = other
                optElements -= minElements
            else:
                raise TypeError("cannot multiply 'ParserElement' and ('%s','%s') objects", type(other[0]),type(other[1]))
        else:
            raise TypeError("cannot multiply 'ParserElement' and '%s' objects", type(other))

        if minElements < 0:
            raise ValueError("cannot multiply ParserElement by negative value")
        if optElements < 0:
            raise ValueError("second tuple value must be greater or equal to first tuple value")
        if minElements == optElements == 0:
            raise ValueError("cannot multiply ParserElement by 0 or (0,0)")

        if (optElements):
            def makeOptionalList(n):
                if n>1:
                    return Optional(self + makeOptionalList(n-1))
                else:
                    return Optional(self)
            if minElements:
                if minElements == 1:
                    ret = self + makeOptionalList(optElements)
                else:
                    ret = And([self]*minElements) + makeOptionalList(optElements)
            else:
                ret = makeOptionalList(optElements)
        else:
            if minElements == 1:
                ret = self
            else:
                ret = And([self]*minElements)
        return ret

    def __rmul__(self, other):
        return self.__mul__(other)

    def __or__(self, other ):
        """
        Implementation of | operator - returns C{L{MatchFirst}}
        """
        if isinstance( other, basestring ):
            other = ParserElement._literalStringClass( other )
        if not isinstance( other, ParserElement ):
            warnings.warn("Cannot combine element of type %s with ParserElement" % type(other),
                    SyntaxWarning, stacklevel=2)
            return None
        return MatchFirst( [ self, other ] )

    def __ror__(self, other ):
        """
        Implementation of | operator when left operand is not a C{L{ParserElement}}
        """
        if isinstance( other, basestring ):
            other = ParserElement._literalStringClass( other )
        if not isinstance( other, ParserElement ):
            warnings.warn("Cannot combine element of type %s with ParserElement" % type(other),
                    SyntaxWarning, stacklevel=2)
            return None
        return other | self

    def __xor__(self, other ):
        """
        Implementation of ^ operator - returns C{L{Or}}
        """
        if isinstance( other, basestring ):
            other = ParserElement._literalStringClass( other )
        if not isinstance( other, ParserElement ):
            warnings.warn("Cannot combine element of type %s with ParserElement" % type(other),
                    SyntaxWarning, stacklevel=2)
            return None
        return Or( [ self, other ] )

    def __rxor__(self, other ):
        """
        Implementation of ^ operator when left operand is not a C{L{ParserElement}}
        """
        if isinstance( other, basestring ):
            other = ParserElement._literalStringClass( other )
        if not isinstance( other, ParserElement ):
            warnings.warn("Cannot combine element of type %s with ParserElement" % type(other),
                    SyntaxWarning, stacklevel=2)
            return None
        return other ^ self

    def __and__(self, other ):
        """
        Implementation of & operator - returns C{L{Each}}
        """
        if isinstance( other, basestring ):
            other = ParserElement._literalStringClass( other )
        if not isinstance( other, ParserElement ):
            warnings.warn("Cannot combine element of type %s with ParserElement" % type(other),
                    SyntaxWarning, stacklevel=2)
            return None
        return Each( [ self, other ] )

    def __rand__(self, other ):
        """
        Implementation of & operator when left operand is not a C{L{ParserElement}}
        """
        if isinstance( other, basestring ):
            other = ParserElement._literalStringClass( other )
        if not isinstance( other, ParserElement ):
            warnings.warn("Cannot combine element of type %s with ParserElement" % type(other),
                    SyntaxWarning, stacklevel=2)
            return None
        return other & self

    def __invert__( self ):
        """
        Implementation of ~ operator - returns C{L{NotAny}}
        """
        return NotAny( self )

    def __call__(self, name=None):
        """
        Shortcut for C{L{setResultsName}}, with C{listAllMatches=False}.

        If C{name} is given with a trailing C{'*'} character, then C{listAllMatches} will be
        passed as C{True}.

        If C{name} is omitted, same as calling C{L{copy}}.

        Example::
            # these are equivalent
            userdata = Word(alphas).setResultsName("name") + Word(nums+"-").setResultsName("socsecno")
            userdata = Word(alphas)("name") + Word(nums+"-")("socsecno")
        """
        if name is not None:
            return self.setResultsName(name)
        else:
            return self.copy()

    def suppress( self ):
        """
        Suppresses the output of this C{ParserElement}; useful to keep punctuation from
        cluttering up returned output.
        """
        return Suppress( self )

    def ignore( self, other ):
        """
        Define expression to be ignored (e.g., comments) while doing pattern
        matching; may be called repeatedly, to define multiple comment or other
        ignorable patterns.

        Example::
            patt = OneOrMore(Word(alphas))
            patt.parseString('ablaj /* comment */ lskjd') # -> ['ablaj']

            patt.ignore(cStyleComment)
            patt.parseString('ablaj /* comment */ lskjd') # -> ['ablaj', 'lskjd']
        """
        if isinstance(other, basestring):
            other = Suppress(other)

        if isinstance( other, Suppress ):
            if other not in self.ignoreExprs:
                self.ignoreExprs.append(other)
        else:
            self.ignoreExprs.append( Suppress( other.copy() ) )
        return self

    def runTests(self, tests, parseAll=True, comment='#', fullDump=True, printResults=True, failureTests=False):
        """
        Execute the parse expression on a series of test strings, showing each
        test, the parsed results or where the parse failed. Quick and easy way to
        run a parse expression against a list of sample strings.

        Parameters:
         - tests - a list of separate test strings, or a multiline string of test strings
         - parseAll - (default=C{True}) - flag to pass to C{L{parseString}} when running tests
         - comment - (default=C{'#'}) - expression for indicating embedded comments in the test
              string; pass None to disable comment filtering
         - fullDump - (default=C{True}) - dump results as list followed by results names in nested outline;
              if False, only dump nested list
         - printResults - (default=C{True}) prints test output to stdout
         - failureTests - (default=C{False}) indicates if these tests are expected to fail parsing

        Returns: a (success, results) tuple, where success indicates that all tests succeeded
        (or failed if C{failureTests} is True), and the results contain a list of lines of each
        test's output

        Example::
            number_expr = pyparsing_common.number.copy()

            result = number_expr.runTests('''
                # unsigned integer
                100
                # negative integer
                -100
                # float with scientific notation
                6.02e23
                # integer with scientific notation
                1e-12
                ''')
            print("Success" if result[0] else "Failed!")

            result = number_expr.runTests('''
                # stray character
                100Z
                # missing leading digit before '.'
                -.100
                # too many '.'
                3.14.159
                ''', failureTests=True)
            print("Success" if result[0] else "Failed!")
        prints::
            # unsigned integer
            100
            [100]

            # negative integer
            -100
            [-100]

            # float with scientific notation
            6.02e23
            [6.02e+23]

            # integer with scientific notation
            1e-12
            [1e-12]

            Success

            # stray character
            100Z
               ^
            FAIL: Expected end of text (at char 3), (line:1, col:4)

            # missing leading digit before '.'
            -.100
            ^
            FAIL: Expected {real number with scientific notation | real number | signed integer} (at char 0), (line:1, col:1)

            # too many '.'
            3.14.159
                ^
            FAIL: Expected end of text (at char 4), (line:1, col:5)

            Success

        Each test string must be on a single line. If you want to test a string that spans multiple
        lines, create a test like this::

            expr.runTest(r"this is a test\\n of strings that spans \\n 3 lines")

        (Note that this is a raw string literal, you must include the leading 'r'.)
        """
        if isinstance(tests, basestring):
            tests = list(map(str.strip, tests.rstrip().splitlines()))
        if isinstance(comment, basestring):
            comment = Literal(comment)
        allResults = []
        comments = []
        success = True
        for t in tests:
            if comment is not None and comment.matches(t, False) or comments and not t:
                comments.append(t)
                continue
            if not t:
                continue
            out = ['\n'.join(comments), t]
            comments = []
            try:
                t = t.replace(r'\n','\n')
                result = self.parseString(t, parseAll=parseAll)
                out.append(result.dump(full=fullDump))
                success = success and not failureTests
            except ParseBaseException as pe:
                fatal = "(FATAL)" if isinstance(pe, ParseFatalException) else ""
                if '\n' in t:
                    out.append(line(pe.loc, t))
                    out.append(' '*(col(pe.loc,t)-1) + '^' + fatal)
                else:
                    out.append(' '*pe.loc + '^' + fatal)
                out.append("FAIL: " + str(pe))
                success = success and failureTests
                result = pe
            except Exception as exc:
                out.append("FAIL-EXCEPTION: " + str(exc))
                success = success and failureTests
                result = exc

            if printResults:
                if fullDump:
                    out.append('')
                print('\n'.join(out))

            allResults.append((t, result))

        return success, allResults


# We made ExtendedParserElement, but we do not actually **use** it
# Instead, we copy over these methods to the ParserElement
for v in set(dir(_ExtendedParserElement)) - set(dir(ParserElement)):
    method = getattr(_ExtendedParserElement, v)
    setattr(ParserElement, v, method)
