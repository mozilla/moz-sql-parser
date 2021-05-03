from moz_sql_parser.utils import *

# SQL CONSTANTS
NULL = Keyword("null", caseless=True).addParseAction(lambda: "null")
TRUE = Keyword("true", caseless=True).addParseAction(lambda: True)
FALSE = Keyword("false", caseless=True).addParseAction(lambda: False)
NOCASE = Keyword("nocase", caseless=True)
ASC = Keyword("asc", caseless=True)
DESC = Keyword("desc", caseless=True)

# SIMPLE KEYWORDS
AS = Keyword("as", caseless=True).suppress()
ALL = Keyword("all", caseless=True)
BY = Keyword("by", caseless=True).suppress()
CAST = Keyword("cast", caseless=True)
CROSS = Keyword("cross", caseless=True)
DISTINCT = Keyword("distinct", caseless=True)
FROM = Keyword("from", caseless=True).suppress()
FULL = Keyword("full", caseless=True)
GROUP = Keyword("group", caseless=True).suppress()
HAVING = Keyword("having", caseless=True).suppress()
INNER = Keyword("inner", caseless=True)
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
# PERCENT = Keyword("percent", caseless=True).suppress()
RIGHT = Keyword("right", caseless=True)
RLIKE = Keyword("rlike", caseless=True)
SELECT = Keyword("select", caseless=True).suppress()
THEN = Keyword("then", caseless=True).suppress()
TOP = Keyword("top", caseless=True).suppress()
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

_SIMILAR = Keyword("similar", caseless=True)
_TO = Keyword("to", caseless=True)
SIMILAR_TO = Group(_SIMILAR + _TO).set_parser_name("is_not")
NOT_SIMILAR_TO = Group(_SIMILAR + _TO).set_parser_name("is_not")

RESERVED = MatchFirst([
    ALL,
    AND,
    AS,
    ASC,
    BETWEEN,
    BY,
    CASE,
    CAST,
    COLLATE,
    CROSS_JOIN,
    CROSS,
    DESC,
    DISTINCT,
    ELSE,
    END,
    FALSE,
    FROM,
    FULL_JOIN,
    FULL_OUTER_JOIN,
    FULL,
    GROUP_BY,
    GROUP,
    HAVING,
    IN,
    INNER_JOIN,
    INNER,
    INTERVAL,
    IS_NOT,
    IS,
    JOIN,
    LEFT_JOIN,
    LEFT_OUTER_JOIN,
    LEFT,
    LIKE,
    LIMIT,
    NOCASE,
    NOT_BETWEEN,
    NOT_IN,
    NOT_LIKE,
    NOT_RLIKE,
    NOT,
    NULL,
    OFFSET,
    ON,
    OR,
    ORDER_BY,
    ORDER,
    OUTER,
    OVER,
    PARTITION_BY,
    PARTITION,
    # PERCENT,
    RIGHT_JOIN,
    RIGHT_OUTER_JOIN,
    RIGHT,
    RLIKE,
    SELECT_DISTINCT,
    SELECT,
    THEN,
    TRUE,
    UNION_ALL,
    UNION,
    USING,
    WHEN,
    WHERE,
    WITH,
    WITHIN_GROUP,
    WITHIN,
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
    "similar_to": 8,
    "not_similar_to": 8,
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
    SIMILAR_TO,
    NOT_SIMILAR_TO,
    NOT,
    AND,
    OR,
]

times = ["now", "today", "tomorrow", "eod"]

durations = {
    "microseconds": "microsecond",
    "microsecond": "microsecond",
    "microsecs": "microsecond",
    "microsec": "microsecond",
    "useconds": "microsecond",
    "usecond": "microsecond",
    "usecs": "microsecond",
    "usec": "microsecond",
    "us": "microsecond",
    "milliseconds": "millisecond",
    "millisecond": "millisecond",
    "millisecon": "millisecond",
    "mseconds": "millisecond",
    "msecond": "millisecond",
    "millisecs": "millisecond",
    "millisec": "millisecond",
    "msecs": "millisecond",
    "msec": "millisecond",
    "ms": "millisecond",
    "seconds": "second",
    "second": "second",
    "secs": "second",
    "sec": "second",
    "s": "second",
    "minutes": "minute",
    "minute": "minute",
    "mins": "minute",
    "min": "minute",
    "m": "minute",
    "hours": "hour",
    "hour": "hour",
    "hrs": "hour",
    "hr": "hour",
    "h": "hour",
    "days": "day",
    "day": "day",
    "d": "day",
    "dayofweek": "dow",
    "dow": "dow",
    "weekday": "dow",
    "weeks": "week",
    "week": "week",
    "w": "week",
    "months": "month",
    "mons": "month",
    "mon": "month",
    "quarters": "quarter",
    "quarter": "quarter",
    "years": "year",
    "year": "year",
    "decades": "decade",
    "decade": "decade",
    "decs": "decade",
    "dec": "decade",
    "centuries": "century",
    "century": "century",
    "cents": "century",
    "cent": "century",
    "c": "century",
    "millennia": "millennium",
    "millennium": "millennium",
    "mils": "millennium",
    "mil": "millennium",
    "epoch": "epoch",
}

_size = Optional(LB + intNum("params") + RB)
_sizes = Optional(LB + intNum("params") + "," + intNum("params") + RB)

# KNOWN TYPES
ARRAY = Group(Keyword("array", caseless=True)("op")).addParseAction(to_json_call)
BIGINT = Group(Keyword("bigint", caseless=True)("op")).addParseAction(to_json_call)
BOOL = Group(Keyword("bool", caseless=True)("op")).addParseAction(to_json_call)
BOOLEAN = Group(Keyword("boolean", caseless=True)("op")).addParseAction(to_json_call)
DOUBLE = Group(Keyword("double", caseless=True)("op")).addParseAction(to_json_call)
FLOAT64 = Group(Keyword("float64", caseless=True)("op")).addParseAction(to_json_call)
FLOAT = Group(Keyword("float", caseless=True)("op")).addParseAction(to_json_call)
GEOMETRY = Group(Keyword("geometry", caseless=True)("op")).addParseAction(to_json_call)
INTEGER = Group(Keyword("integer", caseless=True)("op")).addParseAction(to_json_call)
INT = Group(Keyword("int", caseless=True)("op")).addParseAction(to_json_call)
INT32 = Group(Keyword("int32", caseless=True)("op")).addParseAction(to_json_call)
INT64 = Group(Keyword("int64", caseless=True)("op")).addParseAction(to_json_call)
REAL = Group(Keyword("real", caseless=True)("op")).addParseAction(to_json_call)
TEXT = Group(Keyword("text", caseless=True)("op")).addParseAction(to_json_call)
SMALLINT = Group(Keyword("smallint", caseless=True)("op")).addParseAction(to_json_call)
STRING = Group(Keyword("string", caseless=True)("op")).addParseAction(to_json_call)
STRUCT = Group(Keyword("struct", caseless=True)("op")).addParseAction(to_json_call)

BLOB = (Keyword("blob", caseless=True)("op") + _size).addParseAction(to_json_call)
BYTES = (Keyword("bytes", caseless=True)("op") + _size).addParseAction(to_json_call)
CHAR = (Keyword("char", caseless=True)("op") + _size).addParseAction(to_json_call)
VARCHAR = (Keyword("varchar", caseless=True)("op") + _size).addParseAction(to_json_call)

DECIMAL = (
    Keyword("decimal", caseless=True)("op") + _sizes
).addParseAction(to_json_call)
DOUBLE_PRECISION = (
    Keyword("double", caseless=True) + Keyword("precision", caseless=True)("op")
).addParseAction(lambda: {"double_precision": {}})
NUMERIC = (
    Keyword("numeric", caseless=True)("op") + _sizes
).addParseAction(to_json_call)


DATE = Keyword("date", caseless=True)
DATETIME = Keyword("datetime", caseless=True)
TIME = Keyword("time", caseless=True)
TIMESTAMP = Keyword("timestamp", caseless=True)
TIMESTAMPTZ = Keyword("timestamptz", caseless=True)
TIMETZ = Keyword("timetz", caseless=True)

time_functions = DATE | DATETIME | TIME | TIMESTAMP | TIMESTAMPTZ | TIMETZ

# KNOWNN TIME TYPES
_format = Optional(Regex(r'\"(\"\"|[^"])*\"')("params").addParseAction(unquote))

DATE_TYPE = (DATE("op") + _format).addParseAction(to_json_call)
DATETIME_TYPE = (DATETIME("op") + _format).addParseAction(to_json_call)
TIME_TYPE = (TIME("op") + _format).addParseAction(to_json_call)
TIMESTAMP_TYPE = (TIMESTAMP("op") + _format).addParseAction(to_json_call)
TIMESTAMPTZ_TYPE = (TIMESTAMPTZ("op") + _format).addParseAction(to_json_call)
TIMETZ_TYPE = (TIMETZ("op") + _format).addParseAction(to_json_call)

known_types = MatchFirst([
    ARRAY,
    BIGINT,
    BOOL,
    BOOLEAN,
    BLOB,
    BYTES,
    CHAR,
    DATE_TYPE,
    DATETIME_TYPE,
    DECIMAL,
    DOUBLE_PRECISION,
    DOUBLE,
    FLOAT64,
    FLOAT,
    GEOMETRY,
    INTEGER,
    INT,
    INT32,
    INT64,
    NUMERIC,
    REAL,
    TEXT,
    SMALLINT,
    STRING,
    STRUCT,
    TIME_TYPE,
    TIMESTAMP_TYPE,
    TIMESTAMPTZ_TYPE,
    TIMETZ_TYPE,
    VARCHAR,
])
