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

from mo_dots import is_data
from mo_future import text, number_types, binary_type

from mo_parsing import *
from mo_parsing.utils import is_number, listwrap, alphanums


IDENT_CHAR = alphanums + "@_$"


def scrub(result):
    if result == None:
        return None
    elif isinstance(result, text):
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
        output = {k: vv for k, v in kv_pairs for vv in [scrub(v)] if vv != None}
        if output:
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
        if len(acc)==size:
            yield acc
            acc=[]
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
        if tokens[2] == None:
            return {"missing": tokens[0]}
        elif tokens[0] == "null":
            return {"missing": tokens[2]}
    elif op == "neq":
        if tokens[2] == None:
            return {"exists": tokens[0]}
        elif tokens[0] == "null":
            return {"exists": tokens[2]}
    elif op == "is":
        if tokens[2] == None:
            return {"missing": tokens[0]}
        else:
            return {"exists": tokens[0]}
    elif op == "is_not":
        if tokens[2] == None:
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
    "not like": "not_like",
    "not rlike": "not_rlike",
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
    # elif isinstance(params, list) and len(params) == 1:
    #     params = params[0]
    # elif isinstance(params, ParseResults) and params.length() == 1:
    #     params = params[0]

    return ParseResults(tokens.type, tokens.start, tokens.end, [{op: params}])


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
    if elze:
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


def to_alias(tokens):
    cols = scrub(tokens["col"])
    name = scrub(tokens[0])
    if cols:
        return {name: cols}
    return name


def to_select_call(tokens):
    if tokens["value"][0][0] == "*":
        return ["*"]


def to_union_call(tokens):
    unions = list(tokens["union"])
    if len(unions) == 1:
        output = scrub(unions[0].tokens)  # REMOVE THE Group()
    else:
        sources = scrub([unions[i] for i in range(0, len(unions), 2)])
        operators = [
            "_".join(listwrap(scrub(unions[i]))) for i in range(1, len(unions), 2)
        ]
        op = operators[0]
        if any(o != op for o in operators):
            raise Exception("Expecting no mixing of UNION with UNION ALL")

        if not tokens["orderby"] and not tokens["offset"] and not tokens["limit"]:
            return {op: sources}
        else:
            output = {"from": {op: sources}}

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
    un = ast.literal_eval(val)
    return un


def to_string(tokens):
    val = tokens[0]
    val = "'" + val[1:-1].replace("''", "\\'") + "'"
    return {"literal": ast.literal_eval(val)}


# NUMBERS
realNum = Regex(r"[+-]?(\d+\.\d*|\.\d+)([eE][+-]?\d+)?").set_parser_name("float").addParseAction(lambda t: float(t[0]))
intNum = Regex(r"[+-]?\d+([eE]\+?\d+)?").set_parser_name("int").addParseAction(lambda t: int(t[0]))

# STRINGS
sqlString = Regex(r"\'(\'\'|[^'])*\'").addParseAction(to_string)

