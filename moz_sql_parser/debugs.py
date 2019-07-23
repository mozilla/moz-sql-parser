DEBUG = False

all_exceptions = {}


def record_exception(instring, loc, expr, exc):
    # if DEBUG:
    #     print ("Exception raised:" + _ustr(exc))
    es = all_exceptions.setdefault(loc, [])
    es.append(exc)


def nothing(*args):
    pass


if DEBUG:
    debug = (None, None, None)
else:
    debug = (nothing, nothing, record_exception)
