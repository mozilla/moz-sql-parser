import operator

from mo_future import xrange
from pyparsing import Combine, Optional, opAssoc, ZeroOrMore, Group


def reduce(op, patterns):
    patterns = [p for p in patterns if p != None]
    if len(patterns) == 0:
        return None

    acc = patterns[0]
    for p in patterns[1:]:
        acc = op(acc, p)
    return acc


def infix_notation(operand, operators):

    # EXTEND operand TO INCLUDE UNARY OPERATORS
    prefixes = [Optional(op) for op, num_terms, assoc, parseAction in operators if num_terms == 1 and assoc == opAssoc.RIGHT]
    suffixes = [Optional(op) for op, num_terms, assoc, parseAction in operators if num_terms == 1 and assoc == opAssoc.LEFT]
    compound = reduce(operator.add, [reduce(operator.or_, prefixes), operand, reduce(operator.or_, suffixes)])

    binaries = [op for op, num_terms, assoc, parseAction in operators if num_terms == 2]
    # TRINARY OPERATORS ARE SPLIT INTO TWO BINARY OPERATORS
    binaries.extend(op for ops, num_terms, assoc, parseAction in operators if num_terms ==3 for op in ops)
    all_ops = reduce(operator.or_, binaries)


    def aggregate(match_string, tokensStart, op_list):
        if len(op_list[0]) == 1:
            return op_list

        sequence = op_list[0]
        # CONSTRUCT THE TREE
        op_index = 0
        while op_index<len(operators):
            op, num_terms, assoc, parseAction = operators[op_index]
            if num_terms==1:
                # FIND IN operand
                raise NotImplemented
            elif num_terms==2:
                # PAIR UP
                for i in xrange(1, len(sequence), 2):
                    if sequence[i]==op:
                        pass

            elif num_terms==3:
                # FIND TRIPLE
                pass
            op_index +=1


    # SIMPLE ALTERNATING operand, operator, operand, ... FORMAT
    full_expression = Group(compound + ZeroOrMore((all_ops("operator") + compound("operand"))("compound"))).addParseAction(aggregate)

    return full_expression




