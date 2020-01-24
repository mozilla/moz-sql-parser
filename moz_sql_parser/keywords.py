from pyparsing import Keyword, MatchFirst

from moz_sql_parser.debugs import debug

AND = None
AS = None
ASC = None
BETWEEN = None
CASE = None
COLLATE_NOCASE = None
CROSS_JOIN = None
DESC = None
END = None
ELSE = None
FROM = None
FULL_JOIN = None
FULL_OUTER_JOIN = None
GROUP_BY = None
HAVING = None
IN = None
INNER_JOIN = None
IS = None
IS_NOT = None
JOIN = None
LEFT_JOIN = None
LEFT_OUTER_JOIN = None
LIKE = None
LIMIT = None
NOT_BETWEEN = None
NOT_IN = None
NOT_LIKE = None
OFFSET = None
ON = None
OR = None
ORDER_BY = None
RESERVED = None
RIGHT_JOIN = None
RIGHT_OUTER_JOIN = None
SELECT = None
THEN = None
UNION = None
UNION_ALL = None
USING = None
WHEN = None
WHERE = None

locs = locals()
reserved = []
for name, v in list(locs.items()):
    if v is None:
        n = name.lower().replace("_", " ")
        value = locs[name] = Keyword(n, caseless=True).setName(n).setDebugActions(*debug)
        reserved.append(value)
RESERVED = MatchFirst(reserved)

join_keywords = {
    "join",
    "full join",
    "cross join",
    "inner join",
    "left join",
    "right join",
    "full outer join",
    "right outer join",
    "left outer join",
}

unary_ops = {
    "-": "neg",
    "~": "binary_not"
}

binary_ops = {
    "||": "concat",
    "*": "mul",
    "/": "div",
    "%": "mod",
    "+": "add",
    "-": "sub",
    "&": "binary_and",
    "|": "binary_or",
    "<": "lt",
    "<=": "lte",
    ">": "gt",
    ">=": "gte",
    "=": "eq",
    "==": "eq",
    "!=": "neq",
    "<>": "neq",
    "not in": "nin",
    "is not": "neq",
    "is": "eq",
    "not like": "nlike",
    "not between": "not_between",
    "or":"or",
    "and":"and"
}

precedence = {
    "concat": 1,
    "mul": 2,
    "div": 2,
    "mod": 2,
    "add": 3,
    "sub": 3,
    "binary_and": 4,
    "binary_or": 4,
    "gte": 5,
    "lte": 5,
    "lt": 5,
    "gt": 6,
    "eq": 7,
    "neq": 7,
    "between": 8,
    "not_between": 8,
    "in": 8,
    "nin": 8,
    "is": 8,
    "like": 8,
    "nlike": 8,
    "and": 10,
    "or": 11
}
