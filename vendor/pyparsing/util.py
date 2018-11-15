import sys
import traceback

from pyparsing.future import system_version, singleArgBuiltins


def col (loc,strg):
    """Returns current column within a string, counting newlines as line separators.
   The first column is number 1.

   Note: the default parsing behavior is to expand tabs in the input string
   before starting the parsing process.  See L{I{ParserElement.parseString}<ParserElement.parseString>} for more information
   on parsing strings containing C{<TAB>}s, and suggested methods to maintain a
   consistent view of the parsed string, the parse location, and line and column
   positions within the parsed string.
   """
    s = strg
    return 1 if 0<loc<len(s) and s[loc-1] == '\n' else loc - s.rfind("\n", 0, loc)

def lineno(loc,strg):
    """Returns current line number within a string, counting newlines as line separators.
   The first line is number 1.

   Note: the default parsing behavior is to expand tabs in the input string
   before starting the parsing process.  See L{I{ParserElement.parseString}<ParserElement.parseString>} for more information
   on parsing strings containing C{<TAB>}s, and suggested methods to maintain a
   consistent view of the parsed string, the parse location, and line and column
   positions within the parsed string.
   """
    return strg.count("\n",0,loc) + 1

def line( loc, strg ):
    """Returns the line of text containing loc within a string, counting newlines as line separators.
       """
    lastCR = strg.rfind("\n", 0, loc)
    nextCR = strg.find("\n", loc)
    if nextCR >= 0:
        return strg[lastCR+1:nextCR]
    else:
        return strg[lastCR+1:]


# Only works on Python 3.x - nonlocal is toxic to Python 2 installs
# ~ 'decorator to trim function calls to match the arity of the target'
# ~ def _trim_arity(func, maxargs=3):
#     ~ if func in singleArgBuiltins:
#         ~ return lambda s,l,t: func(t)
#     ~ limit = 0
#     ~ foundArity = False
#     ~ def wrapper(*args):
#         ~ nonlocal limit,foundArity
#         ~ while 1:
#             ~ try:
#                 ~ ret = func(*args[limit:])
#                 ~ foundArity = True
#                 ~ return ret
#             ~ except TypeError:
#                 ~ if limit == maxargs or foundArity:
#                     ~ raise
#                 ~ limit += 1
#                 ~ continue
#     ~ return wrapper


def _xml_escape(data):
    """Escape &, <, >, ", ', etc. in a string of data."""

    # ampersand must be replaced first
    from_symbols = '&><"\''
    to_symbols = ('&'+s+';' for s in "amp gt lt quot apos".split())
    for from_,to_ in zip(from_symbols, to_symbols):
        data = data.replace(from_, to_)
    return data

# this version is Python 2.x-3.x cross-compatible
'decorator to trim function calls to match the arity of the target'
def _trim_arity(func, maxargs=2):
    if func in singleArgBuiltins:
        return lambda s,l,t: func(t)
    limit = [0]
    foundArity = [False]

    # traceback return data structure changed in Py3.5 - normalize back to plain tuples
    if system_version[:2] >= (3,5):
        def extract_stack(limit=0):
            # special handling for Python 3.5.0 - extra deep call stack by 1
            offset = -3 if system_version == (3,5,0) else -2
            frame_summary = traceback.extract_stack(limit=-offset+limit-1)[offset]
            return [frame_summary[:2]]
        def extract_tb(tb, limit=0):
            frames = traceback.extract_tb(tb, limit=limit)
            frame_summary = frames[-1]
            return [frame_summary[:2]]
    else:
        extract_stack = traceback.extract_stack
        extract_tb = traceback.extract_tb

    # synthesize what would be returned by traceback.extract_stack at the call to
    # user's parse action 'func', so that we don't incur call penalty at parse time

    LINE_DIFF = 6
    # IF ANY CODE CHANGES, EVEN JUST COMMENTS OR BLANK LINES, BETWEEN THE NEXT LINE AND
    # THE CALL TO FUNC INSIDE WRAPPER, LINE_DIFF MUST BE MODIFIED!!!!
    this_line = extract_stack(limit=2)[-1]
    pa_call_line_synth = (this_line[0], this_line[1]+LINE_DIFF)

    def wrapper(*args):
        while 1:
            try:
                ret = func(*args[limit[0]:])
                foundArity[0] = True
                return ret
            except TypeError:
                # re-raise TypeErrors if they did not come from our arity testing
                if foundArity[0]:
                    raise
                else:
                    try:
                        tb = sys.exc_info()[-1]
                        if not extract_tb(tb, limit=2)[-1][:2] == pa_call_line_synth:
                            raise
                    finally:
                        del tb

                if limit[0] <= maxargs:
                    limit[0] += 1
                    continue
                raise

    # copy func name to wrapper for sensible debug output
    func_name = "<parse action>"
    try:
        func_name = getattr(func, '__name__',
                            getattr(func, '__class__').__name__)
    except Exception:
        func_name = str(func)
    wrapper.__name__ = func_name

    return wrapper

def _flatten(L):
    ret = []
    for i in L:
        if isinstance(i,list):
            ret.extend(_flatten(i))
        else:
            ret.append(i)
    return ret

