from mo_dots import Null

from mo_parsing import Keyword, Literal, ParserElement, Or, Group

# SQL CONSTANTS
NULL = Keyword("null", caseless=True).addParseAction(lambda: [Null])
TRUE = Keyword("true", caseless=True).addParseAction(lambda: [True])
FALSE = Keyword("false", caseless=True).addParseAction(lambda: [False])
NOCASE = Keyword("nocase", caseless=True)
ASC = Keyword("asc", caseless=True)
DESC = Keyword("desc", caseless=True)


# SIMPLE KEYWORDS
AS = Keyword("as", caseless=True).suppress()
ALL = Keyword("all", caseless=True)
BY = Keyword("by", caseless=True).suppress()
CAST = Keyword("cast", caseless=True)
CROSS = Keyword("cross", caseless=True)
DATE = Keyword("date", caseless=True)
DISTINCT = Keyword("distinct", caseless=True)
FROM = Keyword("from", caseless=True).suppress()
FULL = Keyword("full", caseless=True)
GROUP = Keyword("group", caseless=True).suppress()
HAVING = Keyword("having", caseless=True).suppress()
INNER = Keyword("inner", caseless=True)
TIMESTAMP = Keyword("timestamp", caseless=True)
INTERVAL = Keyword("interval", caseless=True)
JOIN = Keyword("join", caseless=True)
LEFT = Keyword("left", caseless=True)
LIKE = Keyword("like", caseless=True)
LIMIT = Keyword("limit", caseless=True).suppress()
OFFSET = Keyword("offset", caseless=True).suppress()
ON = Keyword("on", caseless=True).suppress()
ORDER = Keyword("order", caseless=True).suppress()
OUTER = Keyword("outer", caseless=True)
OVER = Keyword("over", caseless=True).suppress()
PARTITION = Keyword("partition", caseless=True).suppress()
RIGHT = Keyword("right", caseless=True)
RLIKE = Keyword("rlike", caseless=True)
SELECT = Keyword("select", caseless=True).suppress()
THEN = Keyword("then", caseless=True).suppress()
UNION = Keyword("union", caseless=True)
USING = Keyword("using", caseless=True).suppress()
WHEN = Keyword("when", caseless=True).suppress()
WHERE = Keyword("where", caseless=True).suppress()
WITH = Keyword("with", caseless=True).suppress()
WITHIN = Keyword("within", caseless=True).suppress()

# SIMPLE OPERATORS
CASTING = Literal("::").set_parser_name("concat")
CONCAT = Literal("||").set_parser_name("concat")
MUL = Literal("*").set_parser_name("mul")
DIV = Literal("/").set_parser_name("div")
MOD = Literal("%").set_parser_name("mod")
NEG = Literal("-").set_parser_name("neg")
ADD = Literal("+").set_parser_name("add")
SUB = Literal("-").set_parser_name("sub")
BINARY_NOT = Literal("~").set_parser_name("binary_not")
BINARY_AND = Literal("&").set_parser_name("binary_and")
BINARY_OR = Literal("|").set_parser_name("binary_or")
GTE = Literal(">=").set_parser_name("gte")
LTE = Literal("<=").set_parser_name("lte")
LT = Literal("<").set_parser_name("lt")
GT = Literal(">").set_parser_name("gt")
EQ = (Literal("==") | Literal("=")).set_parser_name("eq")
NEQ = (Literal("!=") | Literal("<>")).set_parser_name("neq")

AND = Keyword("and", caseless=True)
BETWEEN = Keyword("between", caseless=True)
CASE = Keyword("case", caseless=True).suppress()
COLLATE = Keyword("collate", caseless=True)
END = Keyword("end", caseless=True)
ELSE = Keyword("else", caseless=True).suppress()
IN = Keyword("in", caseless=True)
IS = Keyword("is", caseless=True)
NOT = Keyword("not", caseless=True)
OR = Keyword("or", caseless=True)

# COMPOUND KEYWORDS
CROSS_JOIN = Group(CROSS + JOIN).set_parser_name("cross join")
FULL_JOIN = Group(FULL + JOIN).set_parser_name("full join")
FULL_OUTER_JOIN = Group(FULL + OUTER + JOIN).set_parser_name("full outer join")
GROUP_BY = Group(GROUP + BY).set_parser_name("group by")
INNER_JOIN = Group(INNER + JOIN).set_parser_name("inner join")
LEFT_JOIN = Group(LEFT + JOIN).set_parser_name("left join")
LEFT_OUTER_JOIN = Group(LEFT + OUTER + JOIN).set_parser_name("left outer join")
ORDER_BY = Group(ORDER + BY).set_parser_name("order by")
PARTITION_BY = Group(PARTITION + BY).set_parser_name("partition by")
RIGHT_JOIN = Group(RIGHT + JOIN).set_parser_name("right join")
RIGHT_OUTER_JOIN = Group(RIGHT + OUTER + JOIN).set_parser_name("right outer join")
SELECT_DISTINCT = Group(SELECT + DISTINCT).set_parser_name("select distinct")
UNION_ALL = Group(UNION + ALL).set_parser_name("union_all")
WITHIN_GROUP = Group(WITHIN + GROUP).set_parser_name("within_group")

# COMPOUND OPERATORS
NOT_BETWEEN = Group(NOT + BETWEEN).set_parser_name("not_between")
NOT_LIKE = Group(NOT + LIKE).set_parser_name("not_like")
NOT_RLIKE = Group(NOT + RLIKE).set_parser_name("not_rlike")
NOT_IN = Group(NOT + IN).set_parser_name("nin")
IS_NOT = Group(IS + NOT).set_parser_name("is_not")

RESERVED = Or([
    v
    for k, v in locals().items()
    if isinstance(v, ParserElement) and not isinstance(v, Literal)
])

LB = Literal("(").suppress()
RB = Literal(")").suppress()

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

unary_ops = (NEG, NOT, BINARY_NOT)

binary_ops = {
    "::": "cast",
    "COLLATE": "collate",
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
    "not like": "not_like",
    "not rlike": "not_rlike",
    "or": "or",
    "and": "and",
}

precedence = {
    # https://www.sqlite.org/lang_expr.html
    "cast": 0,
    "collate": 0,
    "concat": 1,
    "mul": 2,
    "div": 2,
    "mod": 2,
    "neg": 3,
    "add": 3,
    "sub": 3,
    "binary_not": 4,
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
    "not_like": 8,
    "rlike": 8,
    "not_rlike": 8,
    "and": 10,
    "or": 11,
}


KNOWN_OPS = [
    CASTING,
    COLLATE,
    CONCAT,
    MUL | DIV | MOD,
    NEG,
    ADD | SUB,
    BINARY_NOT,
    BINARY_AND,
    BINARY_OR,
    GTE | LTE | LT | GT,
    EQ | NEQ,
    (BETWEEN, AND),
    (NOT_BETWEEN, AND),
    IN,
    NOT_IN,
    IS_NOT,
    IS,
    LIKE,
    NOT_LIKE,
    RLIKE,
    NOT_RLIKE,
    NOT,
    AND,
    OR,
]

times = ["now", "today", "tomorrow", "eod", "epoch"]

durations = {
    "milliseconds": "millisecond",
    "millisecond": "millisecond",
    "seconds": "second",
    "second": "second",
    "s": "second",
    "minutes": "minute",
    "minute": "minute",
    "m": "minute",
    "hours": "hour",
    "hour": "hour",
    "h": "hour",
    "days": "day",
    "day": "day",
    "d": "day",
    "weeks": "week",
    "week": "week",
    "w": "week",
    "months": "month",
    "month": "month",
    "years": "year",
    "year": "year",
}
