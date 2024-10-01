import enum
import math
import re
import sys
import typing

from dataclasses import dataclass

Id = str


class Op(enum.Enum):
    ADD = '+'
    SUB = '-'
    MUL = '*'
    DIV = '/'
    EQU = '='

    @classmethod
    def from_char(cls, char: str) -> 'Op':
        for op in Op:
            if op.value == char:
                return op
        raise ValueError(f'Invalid operation: `{char}`')

    def apply(self, a: float, b: float) -> float:
        match self:
            case Op.ADD: return a + b
            case Op.SUB: return a - b
            case Op.MUL: return a * b
            case Op.DIV: return a / b
            case Op.EQU: return a == b

    @property
    def identity(self) -> float:
        match self:
            case Op.ADD | Op.SUB: return 0
            case Op.MUL | Op.DIV: return 1
            case Op.EQU: raise ValueError('EQU has no identity')

    @property
    def commutative(self) -> bool:
        return self is Op.ADD or self is Op.MUL or self is Op.EQU

    @property
    def inverse(self) -> 'Op':
        match self:
            case Op.ADD: return Op.SUB
            case Op.SUB: return Op.ADD
            case Op.MUL: return Op.DIV
            case Op.DIV: return Op.MUL
            case Op.EQU: return Op.EQU


T = typing.TypeVar('T')


class Lazy(typing.Generic[T]):
    __value: T

    def __init__(self, callable: typing.Callable[[], T]):
        self.callable = callable
        self.called = False

    def get(self) -> T:
        if not self.called:
            self.__value = self.callable()
            self.called = True
        return self.__value


@dataclass
class SimpleMonkey:
    number: float


@dataclass
class OpMonkey:
    operation: Op
    dependencies: tuple[Id, Id]


Monkey = SimpleMonkey | OpMonkey

Monkeys = dict[Id, Monkey]

EXPR_PATTERN = re.compile(r'(\w+) ([-+*/]) (\w+)')


def read_input() -> Monkeys:
    result: Monkeys = {}

    for line in sys.stdin:
        name, expr = line.split(': ')
        monkey: typing.Optional[Monkey] = None
        try:
            number = float(expr)
            monkey = SimpleMonkey(number)
        except ValueError:
            pass

        if m := EXPR_PATTERN.match(expr):
            a, op, b = m.groups()
            operation = Op.from_char(op)
            monkey = OpMonkey(operation, (a, b))

        if monkey is None:
            raise Exception(f'Could not parse line: {line}')
        result[name] = monkey

    return result


def eval_monkey(monkeys: Monkeys, name: Id) -> float:
    computed: dict[Id, float] = {}

    def do_eval(name: Id) -> float:
        if (result := computed.get(name)) is not None:
            return result

        match monkeys[name]:
            case SimpleMonkey(n): result = n
            case OpMonkey(op, (a, b)): result = op.apply(do_eval(a), do_eval(b))

        computed[name] = result
        return result

    return do_eval(name)


def main():
    monkeys = read_input()

    print(monkeys)
    print(eval_monkey(monkeys, 'root'))


if __name__ == '__main__':
    main()
