DEBUG = False

all_exceptions = {}


def record_exception(expr, loc, string, exc):
    # if DEBUG:
    #     print ("Exception raised:" + _ustr(exc))
    es = all_exceptions.setdefault(loc, [])
    es.append(exc)


def nothing(*args):
    pass


def attempt(expr, loc, string):
    print(
        "Attempt ["
        + string[loc : loc + 10]
        + "...] (char "
        + str(loc)
        + " with "
        + str(expr)
    )


def match(expr, start, loc, string, tokens):
    print(
        "Matched [" + string[start:loc] + "] (char " + str(start) + " with " + str(expr)
    )


if DEBUG:
    # debug = (attempt, match, record_exception)
    debug = (None, None, record_exception)
else:
    debug = (nothing, nothing, record_exception)
