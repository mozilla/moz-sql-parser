# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Author: Erwin de Haan (http://github.com/EraYaN)
#

from mo_future import text_type, number_types, binary_type, items

from pyparsing import ParseResults

import typed_ast.ast3 as ast

#from horast import dump


DEBUG = False


def debug(*args):
    if DEBUG:
        print("[DEBUG] " + " ".join(map(str, args)))
    else:
        pass


def render(item):
    if isinstance(item, str):
        return item.replace("\"", "\\\"")
    elif isinstance(item, ASTNode):
        return item
    elif isinstance(item, ast.AST):
        return dump(item)
    else:
        return item


class ASTNode(ast.AST):
    def __init__(self):
        super().__init__()
        self.set_fields()

    def set_fields(self):
        self._fields = tuple(self.__dict__.keys())

    @classmethod
    def from_tokens(cls, tokens):
        debug("{}: {}".format(cls(), tokens))
        return cls()

    def __str__(self):
        return self.__class__.__name__ + '(' + (self.dict_as_parameters(self.__dict__) if self.__dict__ else '') + ')'

    def dict_as_parameters(self, dict):
        dict = {k: v for k, v in dict.items() if v is not None and k != '_fields'}

        if len(dict) == 1:
            v = dict[next(iter(dict))]
            return "{1}{0}{1}".format(render(v), '"' if isinstance(v, str) else "")
        else:
            return ','.join(
                ["{0}={2}{1}{2}".format(k, render(v), '"' if isinstance(v, str) else "") for (k, v) in dict.items()])

    __repr__ = __str__

    def __key(self):
        return self.__str__()

    def __eq__(self, other):
        if not isinstance(other, ASTNode):
            # Don't recognise "other", so let *it* decide if we're equal
            return False
        return self.__dict__ == other.__dict__

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash(self.__key())

    @staticmethod
    def unquote(val=None, tokens=[None]):
        if not isinstance(val, str):
            return val

        if not val:
            val = tokens[0]
        if val.startswith("'") and val.endswith("'"):
            val = "'" + val[1:-1].replace("''", "\\'") + "'"
            # val = val.replace(".", "\\.")
        elif val.startswith('"') and val.endswith('"'):
            val = '"' + val[1:-1].replace('""', '\\"') + '"'
            # val = val.replace(".", "\\.")
        elif val.startswith('`') and val.endswith('`'):
            val = "'" + val[1:-1].replace("``", "`") + "'"
        elif val.startswith("+"):
            val = val[1:]

        try:
            return ast.literal_eval(val)
        except:
            return val

    @staticmethod
    def scrub(result):
        if isinstance(result, text_type):
            return result
        elif isinstance(result, binary_type):
            return result.decode('utf8')
        elif isinstance(result, number_types):
            return result
        elif isinstance(result, ASTNode):
            return result
        elif not result:
            return {}
        elif isinstance(result, (list, ParseResults)):
            if not result:
                return None
            elif len(result) == 1:
                return ASTNode.scrub(result[0])
            else:
                output = [
                    rr
                    for r in result
                    for rr in [ASTNode.scrub(r)]
                    if rr != None
                ]
                # IF ALL MEMBERS OF A LIST ARE LITERALS, THEN MAKE THE LIST LITERAL
                # if all(isinstance(r, Mapping) and "literal" in r.keys() for r in output):
                #    output = {"literal": [r['literal'] for r in output]}
                return output
        elif not items(result):
            return {}
        else:
            return {
                k: vv
                for k, v in result.items()
                for vv in [ASTNode.scrub(v)]
                if vv != None
            }


class Query(ASTNode):
    def __init__(self, q=None):
        self.q = q
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):
        tok = ASTNode.scrub(tokens)
        debug("Query", tok)

        q = tok

        return cls(q)


class Select(ASTNode):
    def __init__(self, columns=None):
        self.columns = columns
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):
        tok = ASTNode.scrub(tokens)
        debug("Select", tok)

        if isinstance(tok, list):
            columns = tok
        else:
            columns = [tok]

        return cls(columns)


class From(ASTNode):
    def __init__(self, sources=None):
        self.sources = sources
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):

        tok = ASTNode.scrub(tokens)
        debug("From", tok)
        if isinstance(tok, list):
            sources = tok
        else:
            sources = [tok]

        return cls(sources)


class Where(ASTNode):
    def __init__(self, condition=None):
        self.condition = condition
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):

        tok = ASTNode.scrub(tokens)
        debug("Where", tok)
        if isinstance(tok, ASTNode):
            condition = tok
        else:
            raise ValueError("Where is malformed.")

        return cls(condition)


class GroupBy(ASTNode):
    def __init__(self, groupings=None):
        self.groupings = groupings
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):

        tok = ASTNode.scrub(tokens)
        debug("GroupBy", tok)
        if isinstance(tok, list):
            groupings = tok
        else:
            groupings = [tok]

        return cls(groupings)


class OrderBy(ASTNode):
    def __init__(self, orders=None):
        self.orders = orders
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):
        tok = ASTNode.scrub(tokens)
        debug("OrderBy", tok)
        if isinstance(tok, list):
            orders = tok
        else:
            orders = [tok]

        return cls(orders)


class OrderByColumn(ASTNode):
    def __init__(self, ref=None, dir=None):
        self.ref = ref
        self.dir = dir
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):
        tok = ASTNode.scrub(tokens)
        debug("OrderByColumn", tok)
        if isinstance(tok, list):
            if len(tok) == 2:
                ref = tok[0]
                dir = tok[1]
            else:
                raise ValueError("OrderByColumn is malformed.")
        elif isinstance(tok, ColumnReference):
            ref = tok
            dir = None
        else:
            raise ValueError("OrderByColumn is malformed.")

        return cls(ref, dir)


class OrderDirection(ASTNode): pass


class OrderAscending(OrderDirection): pass


class OrderDescending(OrderDirection): pass


class Having(ASTNode):
    def __init__(self, condition=None):
        self.condition = condition
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):

        tok = ASTNode.scrub(tokens)
        debug("Having", tok)
        if isinstance(tok, ASTNode):
            condition = tok
        else:
            raise ValueError("Having is malformed.")

        return cls(condition)


class Limit(ASTNode):
    def __init__(self, count=None):
        self.count = count
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):

        tok = ASTNode.scrub(tokens)
        debug("Limit", tok)
        if isinstance(tok, IntValue):
            count = tok
        else:
            raise ValueError("Limit is malformed.")

        return cls(count)


class Offset(ASTNode):
    def __init__(self, count=None):
        self.count = count
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):
        tok = ASTNode.scrub(tokens)
        debug("Offset", tok)
        if isinstance(tok, IntValue):
            count = tok
        else:
            raise ValueError("Offset is malformed.")

        return cls(count)


class Union(ASTNode):
    def __init__(self, subqueries=None, types=None):
        self.subqueries = subqueries
        self.types = types  # UNION DISTINCT override any UNION ALL to the left of them.
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):
        tok = ASTNode.scrub(tokens)
        debug("Union", tok)
        # Check for odd number
        if len(tok) % 2 == 1:
            types = tok[1::2]  # Odd items
            subqueries = tok[::2]  # Even items
        else:
            raise ValueError("Union clause has missing items.")

        return cls(types, subqueries)


class UnionType(ASTNode): pass


class UnionAll(UnionType): pass


class UnionDistinct(UnionType): pass


class Join(ASTNode):
    def __init__(self, type=None, source=None, constrainttype=None, constraints=None):
        self.type = type
        self.source = source
        self.constrainttype = constrainttype
        self.constraints = constraints
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):
        tok = ASTNode.scrub(tokens)
        debug("Join", tok)
        if len(tok) == 4:
            if isinstance(tok[0], JoinType) and isinstance(tok[2], JoinConstraintType):
                type = tok[0]
                source = tok[1]
                constrainttype = tok[2]
                constraints = tok[3]
            else:
                raise ValueError("Join clause was malformed.")
        else:
            raise ValueError("Join clause has missing items.")

        return cls(type, source, constrainttype, constraints)


class JoinType(ASTNode): pass


class JoinOuter(JoinType): pass


class JoinInner(JoinType): pass


class JoinCross(JoinType): pass


class JoinLeft(JoinType): pass


class JoinRight(JoinType): pass


class JoinFull(JoinType): pass


class JoinFullOuter(JoinType): pass


class JoinLeftOuter(JoinType): pass


class JoinRightOuter(JoinType): pass


class JoinConstraintType(ASTNode): pass


class JoinConstraintTypeOn(JoinConstraintType): pass


class JoinConstraintTypeUsing(JoinConstraintType): pass


class Case(ASTNode):
    def __init__(self, when=None, elsevalue=None):
        self.when = when
        self.elsevalue = elsevalue
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):
        tok = ASTNode.scrub(tokens)
        debug("Case", tok)

        if len(tok) == 5:
            if isinstance(tok[0], CaseKeyword) and isinstance(tok[2], ElseKeyword) and isinstance(tok[4], EndKeyword):
                when = tok[1]
                elsevalue = tok[3]
            else:
                raise ValueError("Case clause was malformed.")
        elif len(tok) == 3:
            if isinstance(tok[0], CaseKeyword) and isinstance(tok[2], EndKeyword):
                when = tok[1]
                elsevalue = None
            else:
                raise ValueError("Case clause was malformed.")
        else:
            raise ValueError("Case clause has missing items.")

        return cls(when, elsevalue)


class When(ASTNode):
    def __init__(self, condition=None, value=None):
        self.condition = condition
        self.value = value
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):
        tok = ASTNode.scrub(tokens)
        debug("When", tok)

        if len(tok) == 4:
            if isinstance(tok[0], WhenKeyword) and isinstance(tok[2], ThenKeyword):
                condition = tok[1]
                value = tok[3]
            else:
                raise ValueError("When clause was malformed.")
        else:
            raise ValueError("When clause has missing items.")

        return cls(condition, value)


class SelectColumn(ASTNode):
    def __init__(self, value=None, alias=None):
        self.value = value
        self.alias = alias
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):
        tok = ASTNode.scrub(tokens)
        debug("Select Column", tok)
        if isinstance(tok, ASTNode):
            value = tok
            alias = None
        elif isinstance(tok[1], Alias) and isinstance(tok[0], ASTNode):
            alias = tok[1]
            value = tok[0]
        elif isinstance(tok[-1], Alias) and isinstance(tok[-2], AsKeyword):
            alias = tok[-1]
            value = tok[-3]
        else:
            raise ValueError("Select Column was malformed")

        return cls(value, alias)


class FromSource(ASTNode):
    def __init__(self, value=None, alias=None):
        self.value = value
        self.alias = alias
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):

        tok = ASTNode.scrub(tokens)
        debug("From Source", tok)
        if isinstance(tok, ASTNode):
            value = tok
            alias = None
        elif isinstance(tok[1], Alias) and isinstance(tok[0], ASTNode):
            alias = tok[1]
            value = tok[0]
        elif isinstance(tok[-1], Alias) and isinstance(tok[-2], AsKeyword):
            alias = tok[-1]
            value = tok[:-2]
        else:
            raise ValueError("From Source was malformed")
        return cls(value, alias)


class Wildcard(ASTNode):
    def __init__(self):
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):
        return cls()

class Reference(ASTNode):
    def __init__(self, id=None, is_value_reference=None, is_valid_reference=None):
        self.id = id
        self.is_value_reference = is_value_reference
        self.is_valid_reference = is_valid_reference
        super().__init__() #ASTNode


class ColumnReference(Reference):
    def __init__(self, id=None, table=None, is_value_reference=None, is_valid_reference=None):
        self.table = table
        super().__init__(id=id, is_valid_reference=is_valid_reference, is_value_reference=is_value_reference) #ASTNode

    @classmethod
    def from_tokens(cls, tokens):

        ids = ASTNode.scrub(tokens)
        debug("Column", ids)
        if isinstance(ids, str):
            id = ASTNode.unquote(ids, tokens)
            table = None
        elif isinstance(ids, ASTNode):
            id = ids
            table = None
        elif len(ids) == 2 or len(ids) == 3:
            id = ASTNode.unquote(ids[-1], tokens)
            table = TableReference.from_tokens(ids[:-1])

        return cls(id, table)


class IntermediaryReference(Reference): pass

class Alias(ASTNode):
    def __init__(self, id=None):
        self.id = id
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):
        ids = ASTNode.scrub(tokens)
        debug("Alias", ids)
        if isinstance(ids, str):
            id = ASTNode.unquote(ids, tokens)
        elif isinstance(ids, list):
            id = ASTNode.unquote(".".join(ids), tokens)
        else:
            raise ValueError("Alias is malformed.")

        return cls(id)


class TableReference(ASTNode):
    def __init__(self, id=None, database=None):
        self.id = id
        self.database = database
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):
        ids = ASTNode.scrub(tokens)

        debug("Table", ids)
        if isinstance(ids, str):
            id = ASTNode.unquote(ids, tokens)
            database = None
        elif len(ids) == 2:
            id = ASTNode.unquote(ids[-1], tokens)
            database = DatabaseReference.from_tokens(ids[:-1])

        return cls(id, database)


class DatabaseReference(ASTNode):
    def __init__(self, id=None):
        self.id = id
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):
        ids = ASTNode.scrub(tokens)

        debug("Database", ids)
        if isinstance(ids, str):
            id = ASTNode.unquote(ids, tokens)

        return cls(id)


class Between(ASTNode):
    def __init__(self, value=None, start=None, end=None):
        self.value = value
        self.start = start
        self.end = end
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):

        tok = ASTNode.scrub(tokens)
        if len(tok) != 5:
            raise ValueError("Between clause has missing parts.")
        if isinstance(tok[1], BetweenKeyword) and isinstance(tok[3], AndOperator):
            value = tok[0]
            start = tok[2]
            end = tok[4]
        else:
            raise ValueError("Between clause did not contain between or and keyword")

        return cls(value, start, end)


class Not(ASTNode):
    def __init__(self, expr=None):
        self.expr = expr
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):
        debug("Not", tokens)
        rhs = tokens[1][0]

        return cls(rhs)


class Distinct(ASTNode):
    def __init__(self, value=None):
        self.value = value
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):
        tok = ASTNode.scrub(tokens)
        debug("Distict", tok)

        return cls(tok)


class FunctionCall(ASTNode):
    def __init__(self, func=None, params=None):
        self.func = func
        self.params = params
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):

        tok = ASTNode.scrub(tokens)
        debug("FunctionCall", tok)
        func = tok[0]
        if len(tok) == 2:
            if isinstance(tok[1], list):
                params = tok[1]
            else:
                params = [tok[1]]
        else:
            params = None

        return cls(func, params)


class UnOp(ASTNode):
    def __init__(self, op=None, rhs=None):
        self.op = op
        self.rhs = rhs
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):

        tok = ASTNode.scrub(tokens)
        debug("UnOp", tok)
        if len(tok) == 2:
            op = tok[0]
            rhs = tok[1]
        else:
            raise ValueError("UnOp should have and can only have one parameter.")

        return cls(op, rhs)


class BinOp(ASTNode):
    def __init__(self, op=None, lhs=None, rhs=None):
        self.op = op
        self.lhs = lhs
        self.rhs = rhs
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):

        tok = ASTNode.scrub(tokens)
        debug("BinOp", tok)
        if len(tok) == 3:
            op = tok[1]
            lhs = tok[0]
            rhs = tok[2]
        elif len(tok) > 3:
            op = tok[1]
            lhs = tok[0]
            rhs = BinOp.from_tokens(tok[2:])
        else:
            raise ValueError("BinOp can only have two parameters.")

        return cls(op, lhs, rhs)


class CompOp(ASTNode):
    def __init__(self, op=None, lhs=None, rhs=None):
        self.op = op
        self.lhs = lhs
        self.rhs = rhs
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):

        params = ASTNode.scrub(tokens)
        debug("CompOp", params)
        if len(params) == 3:
            op = params[1]
            lhs = params[0]
            rhs = params[2]
        else:
            raise ValueError("CompOp can only have two parameters.")

        return cls(op, lhs, rhs)


class Value(ASTNode):
    def __init__(self, value=None):
        self.value = value
        super().__init__() #ASTNode

    @classmethod
    def from_tokens(cls, tokens):
        value = ASTNode.scrub(tokens)
        return cls(value)


class NullValue(Value):
    @classmethod
    def from_tokens(cls, tokens):
        return cls()


class BoolValue(Value):
    @classmethod
    def from_tokens(cls, tokens):
        return cls()


class TrueValue(BoolValue):
    def __bool__(self):
        return True
    __nonzero__ = __bool__


class FalseValue(BoolValue):
    def __bool__(self):
        return False
    __nonzero__ = __bool__


class StringValue(Value):
    def __init__(self, value=None):
        super().__init__(str(value)) #Value

    @classmethod
    def from_tokens(cls, tokens):
        value = ASTNode.unquote(ASTNode.scrub(tokens), tokens)
        return cls(value)


class IntValue(Value):
    def __init__(self, value=None):
        if value is not None:
            super().__init__(int(value)) #Value
        else:
            super().__init__()

    @classmethod
    def from_tokens(cls, tokens):
        value = ASTNode.unquote(ASTNode.scrub(tokens))
        return cls(value)


class DoubleValue(Value):
    def __init__(self, value=None):
        if value is not None:
            super().__init__(float(value))  # Value
        else:
            super().__init__()

    @classmethod
    def from_tokens(cls, tokens):
        value = ASTNode.unquote(ASTNode.scrub(tokens))
        return cls(value)


class CompareOperator(ASTNode): pass


class BinaryOperator(ASTNode): pass


class UnaryOperator(ASTNode): pass


class UAddOperator(UnaryOperator): pass


class USubOperator(UnaryOperator): pass


class BitNotOperator(UnaryOperator): pass


class ConcatOperator(BinaryOperator): pass


class AddOperator(BinaryOperator): pass


class SubOperator(BinaryOperator): pass


class MulOperator(BinaryOperator): pass


class DivOperator(BinaryOperator): pass


class LShiftOperator(BinaryOperator): pass


class RShiftOperator(BinaryOperator): pass


class BitAndOperator(BinaryOperator): pass


class BitOrOperator(BinaryOperator): pass


class BitXorOperator(BinaryOperator): pass


class NEqOperator(CompareOperator): pass


class GtOperator(CompareOperator): pass


class LtOperator(CompareOperator): pass


class GtEOperator(CompareOperator): pass


class LtEOperator(CompareOperator): pass


class EqOperator(CompareOperator): pass


class InOperator(CompareOperator): pass


class NotInOperator(CompareOperator): pass


class IsOperator(CompareOperator): pass


class LikeOperator(CompareOperator): pass


class NotLikeOperator(CompareOperator): pass


class BoolOperator(BinaryOperator): pass


class OrOperator(BoolOperator): pass


class AndOperator(BoolOperator): pass


class Keyword(ASTNode): pass


class AndKeyword(Keyword): pass


class AsKeyword(Keyword): pass


class AscKeyword(Keyword): pass


class BetweenKeyword(Keyword): pass


class CaseKeyword(Keyword): pass


class CollateNoCaseKeyword(Keyword): pass


class CrossJoinKeyword(Keyword): pass


class DescKeyword(Keyword): pass


class ElseKeyword(Keyword): pass


class EndKeyword(Keyword): pass


class FromKeyword(Keyword): pass


class FullJoinKeyword(Keyword): pass


class FullOuterJoinKeyword(Keyword): pass


class GroupByKeyword(Keyword): pass


class HavingKeyword(Keyword): pass


class InKeyword(Keyword): pass


class NotInKeyword(Keyword): pass


class InnerJoinKeyword(Keyword): pass


class IsKeyword(Keyword): pass


class JoinKeyword(Keyword): pass


class LeftJoinKeyword(Keyword): pass


class LeftOuterJoinKeyword(Keyword): pass


class LimitKeyword(Keyword): pass


class OffsetKeyword(Keyword): pass


class LikeKeyword(Keyword): pass


class NotLikeKeyword(Keyword): pass


class OnKeyword(Keyword): pass


class UsingKeyword(Keyword): pass


class OrKeyword(Keyword): pass


class OrderByKeyword(Keyword): pass


class RightJoinKeyword(Keyword): pass


class RightOuterJoinKeyword(Keyword): pass


class SelectKeyword(Keyword): pass


class ThenKeyword(Keyword): pass


class UnionKeyword(Keyword): pass


class UnionAllKeyword(Keyword): pass


class WhenKeyword(Keyword): pass


class WhereKeyword(Keyword): pass


class WithKeyword(Keyword): pass
