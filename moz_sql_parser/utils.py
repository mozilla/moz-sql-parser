# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Contact: Kyle Lahnakoski (kyle@lahnakoski.com)
#

from __future__ import absolute_import, division, unicode_literals

import ast

from mo_dots import is_data, is_null
from mo_future import text, number_types, binary_type
from mo_logs import Log

from mo_parsing import *
from mo_parsing import debug
from mo_parsing.utils import is_number, listwrap, alphanums

IDENT_CHAR = alphanums + "@_$"


def scrub(result):
    if result == None:
        return None
    elif isinstance(result, text):
        if result == "null":
            return None
        return result
    elif isinstance(result, binary_type):
        return result.decode("utf8")
    elif isinstance(result, number_types):
        return result
    elif isinstance(result, dict) and not result:
        return result
    elif isinstance(result, list):
        output = [rr for r in result for rr in [scrub(r)]]

        if not output:
            return None
        elif len(output) == 1:
            return output[0]
        else:
            return scrub_literal(output)
    else:
        # ATTEMPT A DICT INTERPRETATION
        kv_pairs = list(result.items())
        output = {k: vv for k, v in kv_pairs for vv in [scrub(v)] if not is_null(vv)}
        if isinstance(result, dict):
            return output
        elif output:
            if debug.DEBUGGING:
                # CHECK THAT NO ITEMS WERE MISSED
                def look(r):
                    for token in r.tokens:
                        if isinstance(token, ParseResults):
                            if token.name:
                                continue
                            elif token.length() == 0:
                                continue
                            elif isinstance(token.type, Group):
                                Log.error(
                                    "This token is lost during scrub: {{token}}",
                                    token=token,
                                )
                            else:
                                look(token)
                        else:
                            Log.error(
                                "This token is lost during scrub: {{token}}",
                                token=token,
                            )

                look(result)

            return output
        temp = list(result)
        return scrub(temp)


def scrub_literal(candidate):
    # IF ALL MEMBERS OF A LIST ARE LITERALS, THEN MAKE THE LIST LITERAL
    if all(isinstance(r, number_types) for r in candidate):
        pass
    elif all(
        isinstance(r, number_types) or (is_data(r) and "literal" in r.keys())
        for r in candidate
    ):
        candidate = {"literal": [r["literal"] if is_data(r) else r for r in candidate]}
    return candidate


def _chunk(values, size):
    acc = []
    for v in values:
        acc.append(v)
        if len(acc) == size:
            yield acc
            acc = []
    if acc:
        yield acc


def to_json_operator(tokens):
    # ARRANGE INTO {op: params} FORMAT
    length = len(tokens.tokens)
    if length == 2:
        # UNARY OPERATOR
        op = tokens.tokens[0].type.parser_name
        if op == "neg" and is_number(tokens[1]):
            return -tokens[1]
        return {op: tokens[1]}
    elif length == 5:
        # TRINARY OPERATOR
        return {tokens.tokens[1].type.parser_name: [tokens[0], tokens[2], tokens[4]]}

    op = tokens[1]
    if not isinstance(op, text):
        op = op.type.parser_name
    op = binary_ops.get(op, op)
    if op == "eq":
        if tokens[2] == "null":
            return {"missing": tokens[0]}
        elif tokens[0] == "null":
            return {"missing": tokens[2]}
    elif op == "neq":
        if tokens[2] == "null":
            return {"exists": tokens[0]}
        elif tokens[0] == "null":
            return {"exists": tokens[2]}
    elif op == "is":
        if tokens[2] == "null":
            return {"missing": tokens[0]}
        else:
            return {"exists": tokens[0]}
    elif op == "is_not":
        if tokens[2] == "null":
            return {"exists": tokens[0]}
        else:
            return {"missing": tokens[0]}

    operands = [tokens[0], tokens[2]]
    binary_op = {op: operands}

    if op in {"add", "mul", "and", "or"}:
        # ASSOCIATIVE OPERATORS
        acc = []
        for operand in operands:
            if isinstance(operand, ParseResults):
                operand = operand[0]
                if isinstance(operand, list):
                    acc.append(operand)
                    continue
                prefix = operand.get(op)
                if prefix:
                    acc.extend(prefix)
                    continue
                else:
                    acc.append(operand)
            else:
                acc.append(operand)
        binary_op = {op: acc}
    return binary_op


def to_tuple_call(tokens):
    # IS THIS ONE VALUE IN (), OR MANY?
    if tokens.length() == 1:
        return [scrub(tokens)]
    return [scrub_literal(scrub(tokens))]


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
    "similar to": "similar_to",
    "not like": "not_like",
    "not rlike": "not_rlike",
    "not simlilar to": "not_similar_to",
    "or": "or",
    "and": "and",
}


def to_json_call(tokens):
    # ARRANGE INTO {op: params} FORMAT
    op = tokens["op"].lower()
    op = binary_ops.get(op, op)

    params = scrub(tokens["params"])
    if not params:
        params = {}
    if scrub(tokens["ignore_nulls"]):
        ignore_nulls = True
    else:
        ignore_nulls = None

    return ParseResults(
        tokens.type,
        tokens.start,
        tokens.end,
        [{op: params, "ignore_nulls": ignore_nulls}],
    )


def to_interval_call(tokens):
    # ARRANGE INTO {interval: [amount, type]} FORMAT
    params = scrub(tokens["params"])
    if not params:
        params = {}
    if len(params) == 2:
        return ParseResults(
            tokens.type, tokens.start, tokens.end, [{"interval": params}]
        )

    return ParseResults(
        tokens.type,
        tokens.start,
        tokens.end,
        [{"add": [{"interval": p} for p in _chunk(params, size=2)]}],
    )


def to_case_call(tokens):
    cases = list(tokens["case"])
    elze = tokens["else"]
    if elze != None:
        cases.append(elze)
    return {"case": cases}


def to_switch_call(tokens):
    # CONVERT TO CLASSIC CASE STATEMENT
    value = tokens["value"]
    cases = list(tokens["case"])
    for c in cases:
        c["when"] = {"eq": [value, c["when"]]}
    elze = tokens["else"]
    if elze:
        cases.append(elze)
    return {"case": cases}


def to_when_call(tokens):
    tok = tokens
    return {"when": tok["when"], "then": tok["then"]}


def to_join_call(tokens):
    op = " ".join(listwrap(scrub(tokens["op"])))
    if tokens["join"]["name"]:
        output = {op: {
            "name": tokens["join"]["name"],
            "value": tokens["join"]["value"],
        }}
    else:
        output = {op: tokens["join"]}

    output["on"] = tokens["on"]
    output["using"] = tokens["using"]
    return output


def to_expression_call(tokens):
    over = tokens["over"]
    within = tokens["within"]
    if over or within:
        return

    expr = ParseResults(
        tokens.type, tokens.start, tokens.end, listwrap(tokens["value"])
    )
    return expr


def to_alias(tokens):
    cols = scrub(tokens["col"])
    name = scrub(tokens[0])
    if cols:
        return {name: cols}
    return name


def to_top_clause(tokens):
    value = scrub(tokens["value"])
    if not value:
        return None
    elif tokens["ties"]:
        output = {}
        output["ties"] = True
        if tokens["percent"]:
            output["percent"] = value
        else:
            output["value"] = value
        return output
    elif tokens["percent"]:
        return {"percent": value}
    else:
        return value


def to_select_call(tokens):
    value = tokens["value"]
    if value.value() == "*":
        return ["*"]

    if value["over"] or value["within"]:
        output = ParseResults(tokens.type, tokens.start, tokens.end, value.tokens)
        output["name"] = tokens["name"]
        return output
    else:
        return


def to_union_call(tokens):
    unions = tokens["union"]
    if unions.type.parser_name == "unordered sql":
        output = scrub(unions)  # REMOVE THE Group()
    else:
        unions = list(unions)
        sources = scrub([unions[i] for i in range(0, len(unions), 2)])
        operators = [
            "_".join(listwrap(scrub(unions[i]))) for i in range(1, len(unions), 2)
        ]
        acc = sources[-1]
        last_union = None
        for op, so in reversed(list(zip(operators, sources))):
            if op == last_union:
                acc[op] = [so] + acc[op]
            else:
                acc = {op: [so, acc]}
            last_union = op

        if not tokens["orderby"] and not tokens["offset"] and not tokens["limit"]:
            return acc
        else:
            output = {"from": acc}

    output["orderby"] = tokens["orderby"]
    output["offset"] = tokens["offset"]
    output["limit"] = tokens["limit"]
    return output


def to_statement(tokens):
    output = scrub(tokens["query"])
    output["with"] = scrub(tokens["with"])
    return output


def unquote(tokens):
    val = tokens[0]
    if val.startswith("'") and val.endswith("'"):
        val = "'" + val[1:-1].replace("''", "\\'") + "'"
    elif val.startswith('"') and val.endswith('"'):
        val = '"' + val[1:-1].replace('""', '\\"') + '"'
    elif val.startswith("`") and val.endswith("`"):
        val = '"' + val[1:-1].replace("``", "`").replace('"', '\\"') + '"'
    elif val.startswith("[") and val.endswith("]"):
        val = '"' + val[1:-1].replace("]]", "]").replace('"', '\\"') + '"'
    elif val.startswith("+"):
        val = val[1:]
    un = ast.literal_eval(val).replace(".", "\\.")
    return un


def to_string(tokens):
    val = tokens[0]
    val = "'" + val[1:-1].replace("''", "\\'") + "'"
    return {"literal": ast.literal_eval(val)}


# NUMBERS
realNum = (
    Regex(r"[+-]?(\d+\.\d*|\.\d+)([eE][+-]?\d+)?")
    .set_parser_name("float")
    .addParseAction(lambda t: float(t[0]))
)
intNum = (
    Regex(r"[+-]?\d+([eE]\+?\d+)?")
    .set_parser_name("int")
    .addParseAction(lambda t: int(t[0]))
)

# STRINGS
sqlString = Regex(r"\'(\'\'|[^'])*\'").addParseAction(to_string)

expr = Forward()
