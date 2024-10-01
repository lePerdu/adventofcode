import itertools
import sys
import typing

Elf = list[int]


def read_input(f: typing.TextIO) -> typing.Generator[Elf, None, None]:
    elf = []
    for line in f:
        if line == '' or line.isspace():
            yield elf
            elf = []
        else:
            elf.append(int(line))

    if len(elf) > 0:
        yield elf


def main():
    """Find total calories of the elf carrying the most"""

    print(max(sum(elf) for elf in read_input(sys.stdin)))


if __name__ == '__main__':
    main()
