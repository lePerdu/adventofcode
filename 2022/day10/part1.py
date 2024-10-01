import itertools
import sys
import typing

from dataclasses import dataclass


@dataclass
class AddInstr():
    value: int

    def __str__(self) -> str:
        return f'addx {self.value}'


class NoopInstr():
    def __new__(cls) -> 'NoopInstr':
        if not hasattr(cls, 'instance'):
            cls.instance = super(NoopInstr, cls).__new__(cls)
        return cls.instance

    def __str__(self) -> str:

        return 'noop'


Instr = AddInstr | NoopInstr


@dataclass
class State:
    cycle: int
    register: int

    @property
    def signal_strength(self) -> int:
        return self.cycle * self.register


class Machine:
    state = State(0, 1)

    def _clock_cycle(self) -> State:
        self.state.cycle += 1
        return State(self.state.cycle, self.state.register)

    def exec(self, instr: Instr) -> typing.Iterator[State]:
        match instr:
            case AddInstr(v):
                yield self._clock_cycle()
                yield self._clock_cycle()
                self.state.register += v
            case NoopInstr():
                yield self._clock_cycle()


def read_input() -> typing.Iterator[Instr]:
    for line in sys.stdin:
        command, *args = line.split(maxsplit=1)
        match command:
            case 'addx': yield AddInstr(int(args[0]))
            case 'noop': yield NoopInstr()
            case _: raise ValueError(f'Invalid instruction: {line.strip()}')


def is_important(cycle: int) -> bool:
    return (cycle + 20) % 40 == 0


def main():
    instrs = read_input()
    emulator = Machine()

    total = 0
    for instr in instrs:
        print(instr)
        for state in emulator.exec(instr):
            print(f'    {state}')
            if is_important(state.cycle):
                print('== Signal strength', state.signal_strength)
                total += state.signal_strength

    print(total)


if __name__ == '__main__':
    main()
