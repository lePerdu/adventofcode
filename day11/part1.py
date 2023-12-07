import abc
import math
import typing

from dataclasses import dataclass

# Part 1
CALM_DOWN_ENABLED = True
ROUND_COUNT = 20

# Part 2
# CALM_DOWN_ENABLED = False
# ROUND_COUNT = 10_000

WorryLevel = int

MonkeyId = int


@dataclass(frozen=True)
class Operation(abc.ABC):
    operand: typing.Optional[int]

    def apply(self, current: WorryLevel) -> WorryLevel:
        operand = self.operand if self.operand is not None else current
        return self._do_apply(current, operand)

    @abc.abstractmethod
    def _do_apply(self, current: WorryLevel, operand: WorryLevel) -> WorryLevel:
        ...


class AddOp(Operation):
    def _do_apply(self, current: WorryLevel, operand: WorryLevel) -> WorryLevel:
        res = current + operand
        print(f'    Worry level increases by {operand} to {res}.')
        return res


class MulOp(Operation):
    def _do_apply(self, current: WorryLevel, operand: WorryLevel) -> WorryLevel:
        res = current * operand
        print(f'    Worry level is multiplied by {operand} to {res}.')
        return res


@dataclass(frozen=True)
class Test:
    div_by: int

    def check(self, value: WorryLevel) -> bool:
        res = value % self.div_by == 0
        print(
            '    Current worry level is'
            f'{"" if res else " not"} divisible by {self.div_by}.')
        return res


@dataclass(frozen=True)
class Actions:
    if_true: MonkeyId
    if_false: MonkeyId


@dataclass(frozen=True)
class Throw:
    item: WorryLevel
    to_monkey: MonkeyId


@dataclass
class Monkey:
    items: list[WorryLevel]
    operation: Operation
    test: Test
    actions: Actions

    inspect_count = 0

    def add_item(self, item: WorryLevel):
        self.items.append(item)

    def take_turn(self, calm_down: bool = True) -> typing.Iterator[Throw]:
        for item in self.items:
            print(f'  Monkey inspects an item with a worry level of {item}.')
            self.inspect_count += 1
            item = self.operation.apply(item)
            if calm_down:
                item = apply_calm_down(item)

            if self.test.check(item):
                dest = self.actions.if_true
            else:
                dest = self.actions.if_false

            print(
                f'    Item with worry level {item} is thrown to monkey {dest}.'
            )
            yield Throw(item, dest)

        self.items.clear()

    def reduce_worry_levels(self, modulus: int):
        for i in range(len(self.items)):
            self.items[i] %= modulus


def apply_calm_down(item: WorryLevel) -> WorryLevel:
    if CALM_DOWN_ENABLED:
        res = item // 3
        print(
            '    Monkey gets bored with item. '
            f'Worry level is divided by 3 to {res}.'
        )
        return res
    else:
        return item


def read_line(expected: str) -> str:
    raw = input()
    if raw.startswith(expected):
        return raw.replace(expected, '')
    else:
        raise ValueError(f'Unexpected input: {raw:?}')


def read_starting_items() -> list[WorryLevel]:
    return [int(i) for i in read_line('  Starting items: ').split(',')]


def read_operation() -> Operation:
    operator, operand = read_line('  Operation: new = old ').split(maxsplit=1)

    try:
        operand = int(operand)
    except:
        operand = None

    match operator:
        case '+': return AddOp(operand)
        case '*': return MulOp(operand)
        case _: raise ValueError(f'Invalid operator: {operator:?}')


def read_test() -> Test:
    div_by = int(read_line('  Test: divisible by '))
    return Test(div_by)


def read_actions() -> Actions:
    return Actions(
        if_true=int(read_line('    If true: throw to monkey ')),
        if_false=int(read_line('    If false: throw to monkey ')),
    )


def read_monkey() -> typing.Optional[Monkey]:
    try:
        read_line('Monkey ')
    except EOFError:
        return None

    m = Monkey(
        read_starting_items(),
        read_operation(),
        read_test(),
        read_actions(),
    )

    # Optional line at the end
    try:
        read_line('')
    except EOFError:
        pass

    return m


def read_input() -> typing.Iterator[Monkey]:
    while (m := read_monkey()) is not None:
        yield m


def do_round(monkeys: list[Monkey]):
    for index, m in enumerate(monkeys):
        print(f'Monkey {index}:')
        # TODO Buffer throws to avoid infinite loop in case of throw to self
        for t in m.take_turn():
            monkeys[t.to_monkey].add_item(t.item)


def main():
    monkeys = list(read_input())

    modulus = math.lcm(*(m.test.div_by for m in monkeys))

    for round_number in range(1, ROUND_COUNT+1):
        do_round(monkeys)
        for m in monkeys:
            m.reduce_worry_levels(modulus)

        print()
        print(f'After round {round_number}:')
        for i, m in enumerate(monkeys):
            print(f'Monkey {i}: {m.items}. Inspected {m.inspect_count} items.')

    ordered = sorted(m.inspect_count for m in monkeys)
    monkey_business = ordered[-1] * ordered[-2]
    print('Monkey business: ', monkey_business)


if __name__ == '__main__':
    main()
