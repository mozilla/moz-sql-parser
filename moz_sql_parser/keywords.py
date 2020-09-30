from mo_parsing import Keyword, MatchFirst

sql_clauses = [
    "AS",
    "FROM",
    "GROUP_BY",
    "HAVING",
    "JOIN",
    "LEFT_JOIN",
    "LEFT_OUTER_JOIN",
    "LIMIT",
    "OFFSET",
    "ORDER_BY",
    "RIGHT_JOIN",
    "RIGHT_OUTER_JOIN",
    "SELECT",
    "USING",
    "WITH",
    "WHERE",
]

sql_operators = [
    "AND",
    "ASC",
    "BETWEEN",
    "CASE",
    "COLLATE_NOCASE",
    "CROSS_JOIN",
    "DESC",
    "END",
    "ELSE",
    "FULL_JOIN",
    "FULL_OUTER_JOIN",
    "IN",
    "INNER_JOIN",
    "IS",
    "IS_NOT",
    "LIKE",
    "NOT_BETWEEN",
    "NOT_IN",
    "NOT_LIKE",
    "ON",
    "OR",
    "RESERVED",
    "THEN",
    "UNION",
    "UNION_ALL",
    "WHEN",
]

reserved_keywords = []
for name in sql_clauses:
    n = name.lower().replace("_", " ")
    value = locals()[name] = (
        # WE suppress() THE CLAUSES
        Keyword(n, caseless=True).suppress().set_parser_name(n)
    )
    reserved_keywords.append(value)
for name in sql_operators:
    n = name.lower().replace("_", " ")
    value = locals()[name] = (
        # DO NTO suppress() OPERATORS, SO WE CAN POST-PROCESSES THEM EASIER
        Keyword(n, caseless=True).set_parser_name(n)
    )
    reserved_keywords.append(value)
RESERVED = MatchFirst(reserved_keywords)

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

unary_ops = {"-": "neg", "~": "binary_not"}

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
    "or": "or",
    "and": "and",
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
    "or": 11,
}

durations = [
    "milliseconds",
    "seconds",
    "minutes",
    "hours",
    "days",
    "weeks",
    "months",
    "years",
]
