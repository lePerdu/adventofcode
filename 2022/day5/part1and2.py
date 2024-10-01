import re
import sys
import typing

from dataclasses import dataclass


@dataclass
class Move:
    count: int
    src: int
    dst: int

    def __repr__(self) -> str:
        return f'move {self.count} from {self.src} to {self.dst}'


Box = str


class Crates:
    stacks: list[list[Box]]

    def __init__(self, stacks: list[list[Box]]):
        self.stacks = stacks

    def move_one_at_a_time(self, m: Move):
        for _ in range(m.count):
            self.stacks[m.dst-1].append(
                self.stacks[m.src-1].pop()
            )

    def move_same_order(self, m: Move):
        chunk = self.stacks[m.src-1][-m.count:]
        del self.stacks[m.src-1][-m.count:]
        self.stacks[m.dst-1].extend(chunk)

    def get_tops(self) -> typing.Iterator[Box]:
        for s in self.stacks:
            try:
                yield s[-1]
            except:
                yield ' '

    def __repr__(self) -> str:
        res = ''
        for s in self.stacks:
            res += f'{s}\n'
        return res


def read_crates() -> Crates:
    boxes_lines: list[str] = []
    for line in sys.stdin:
        if line.strip() == '':
            break
        boxes_lines.append(line)

    # Remove numbering
    stack_count = len(boxes_lines.pop().split())
    stacks = [[] for _ in range(stack_count)]

    start_index = 1
    skip_index = 4
    end_index = start_index + skip_index*(stack_count - 1) + 1

    while len(boxes_lines) > 0:
        line = boxes_lines.pop()
        for stack_index, box_index in enumerate(range(start_index, end_index, skip_index)):
            box = line[box_index]
            if not box.isspace():
                stacks[stack_index].append(box)

    return Crates(stacks)


def read_moves() -> typing.Iterator[Move]:
    for line in sys.stdin:
        # move COUNT from SOURCE to DESTINATION
        _, c, _, s, _, d, *_ = line.split()
        yield Move(int(c), int(s), int(d))


def read_input() -> tuple[Crates, list[Move]]:
    crates = read_crates()
    moves = read_moves()
    return crates, list(moves)


def main():
    crates, moves = read_input()
    print(crates)
    print(moves)
    print()

    for m in moves:
        # crates.move_one_at_a_time(m)  # Part1
        crates.move_same_order(m)  # Part2
    print(crates)

    print(''.join(crates.get_tops()))


if __name__ == '__main__':
    main()
