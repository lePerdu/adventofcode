import abc
import typing

from dataclasses import dataclass

from part1 import Id, Op, Monkey, Monkeys, OpMonkey, SimpleMonkey, read_input


class EvalError(Exception):
    pass


class Expr(abc.ABC):
    def reduce(self) -> 'Expr':
        return self


@dataclass(frozen=True)
class Number(Expr):
    value: float

    def __str__(self) -> str:
        return str(self.value)


class Node(Expr):
    op: Op
    a: Expr
    b: Expr

    def __new__(cls, op: Op, a: Expr, b: Expr) -> Expr:
        if isinstance(a, Number) and isinstance(b, Number) and op is not Op.EQU:
            n = op.apply(a.value, b.value)
            return Number(n)
        else:
            return super(Node, cls).__new__(cls)

    def __init__(self, op: Op, a: Expr, b: Expr):
        self.op = op
        self.a = a
        self.b = b

    def reduce(self) -> 'Expr':
        a = self.a.reduce()
        b = self.b.reduce()
        if isinstance(a, Number) and isinstance(b, Number) and self.op is not Op.EQU:
            n = self.op.apply(a.value, b.value)
            return Number(n)
        if compare_expr(a, b) > 0:
            if self.op.commutative:
                return Node(self.op, b, a)
            else:
                return Node(
                    self.op.inverse,
                    Node(self.op, Number(self.op.identity), b).reduce(),
                    a,
                )
        else:
            return Node(self.op, a, b)

    def __str__(self) -> str:
        def with_parens(n: Expr) -> str:
            if isinstance(n, Node):
                return f'({n})'
            else:
                return str(n)
        return f'{with_parens(self.a)} {self.op.value} {with_parens(self.b)}'


class Unknown(Expr):
    def __str__(self) -> str:
        return 'X'


def compare_op(op1: Op, op2: Op) -> float:
    if op1.value < op2.value:
        return -1
    elif op2.value > op2.value:
        return 1
    else:
        return 0


def compare_expr(a: Expr, b: Expr) -> float:
    match a, b:
        case _, Unknown(): return -1
        case Unknown(), _: return 1
        case Number(an), Number(bn): return an - bn
        case Number(), _: return -1
        case _, Number(): return 1
        case Node(op_a, a_a, b_a), Node(op_b, a_b, b_b):
            if (res := compare_op(op_a, op_b)) != 0:
                return res
            elif (res := compare_expr(a_a, a_b)) != 0:
                return res
            elif (res := compare_expr(b_a, b_b)) != 0:
                return res
            else:
                return 0
    raise Exception('Missed case')


ROOT = 'root'
HUMAN = 'humn'


def build_expr(monkeys: Monkeys, node: Id, human_node: typing.Optional[str] = HUMAN) -> Expr:
    if node == human_node:
        return Unknown()

    match monkeys[node]:
        case SimpleMonkey(n): return Number(n)
        case OpMonkey(op, (a, b)):
            if node == ROOT:
                op = Op.EQU
            return Node(
                op,
                build_expr(monkeys, a, human_node=human_node),
                build_expr(monkeys, b, human_node=human_node),
            )


def find_unknown(expr: Expr) -> float:
    assert isinstance(expr, Node) and expr.op is Op.EQU, \
        'Must start with an `=` expression'

    def solve_equality(lhs: Expr, rhs: Expr) -> float:
        assert isinstance(lhs, Number), 'LHS must be a number'

        match rhs:
            case Node():
                match rhs.op:
                    case Op.ADD | Op.MUL:
                        new_lhs = Node(rhs.op.inverse, lhs, rhs.a)
                    case Op.SUB | Op.DIV:
                        new_lhs = Node(rhs.op, rhs.a, lhs)
                    case Op.EQU:
                        raise ValueError(f'Unexpected EQU')
                return solve_equality(new_lhs, rhs.b)
            case Unknown():
                return lhs.value
            case _:
                raise ValueError(f'Expected Node or Unknown, found {rhs}')

    return solve_equality(expr.a, expr.b)


def main():
    monkeys = read_input()
    root_expr = build_expr(monkeys, 'root')
    print('Original', root_expr)
    root_expr = root_expr.reduce()
    print('Reduced', root_expr)
    ans = find_unknown(root_expr)
    print(ans)

    monkeys[HUMAN] = SimpleMonkey(ans)
    root_expr = build_expr(monkeys, 'root', human_node=None)
    print('With value', root_expr)
    root_expr = root_expr.reduce()
    print('Evaluated', root_expr)


if __name__ == '__main__':
    main()
