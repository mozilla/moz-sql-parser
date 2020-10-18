import types
from collections import OrderedDict
from threading import RLock, Lock

from mo_dots import Data


class FiFoCache(object):
    def __init__(self, size):
        cache = OrderedDict()

        def get(self, key):
            return cache.get(key)

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


class UnboundedCache(object):
    def __init__(self):
        cache = {}

        def get(self, key):
            return cache.get(key)

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


packrat_cache = UnboundedCache()
packrat_cache_stats = Data()


def resetCache():
    packrat_cache.clear()
    packrat_cache_stats.hit = 0
    packrat_cache_stats.miss = 0


def enablePackrat(cache_size_limit=128):
    """Enables "packrat" parsing, which adds memoizing to the parsing logic.
    Repeated parse attempts at the same string location (which happens
    often in many complex grammars) can immediately return a cached value,
    instead of re-executing parsing/validating code.  Memoizing is done of
    both valid results and parsing exceptions.

    Parameters:

    - cache_size_limit - (default= ``128``) - if an integer value is provided
      will limit the size of the packrat cache; if None is passed, then
      the cache size will be unbounded; if 0 is passed, the cache will
      be effectively disabled.

    This speedup may break existing programs that use parse actions that
    have side-effects.  For this reason, packrat parsing is disabled when
    you first import mo_parsing.  To activate the packrat feature, your
    program must call the class method :class:`enablePackrat`.
    For best results, call ``enablePackrat()`` immediately after
    importing mo_parsing.

    Example::

        import mo_parsing
        mo_parsing.enablePackrat()
    """
    global packrat_cache
    if cache_size_limit is None:
        packrat_cache = UnboundedCache()
    else:
        packrat_cache = FiFoCache(cache_size_limit)
