import itertools
import sys
import typing

ID = int

Range = tuple[ID, ID]


def read_range(s: str) -> Range:
    return tuple(int(v) for v in s.split('-', maxsplit=1))


def read_input() -> typing.Iterator[tuple[Range, Range]]:
    for line in sys.stdin:
        yield tuple(read_range(r) for r in line.split(',', maxsplit=1))


def contains(a: Range, b: Range) -> bool:
    return a[0] <= b[0] and b[1] <= a[1]


def one_contains(a: Range, b: Range) -> bool:
    return contains(a, b) or contains(b, a)


def main():
    count = 0
    for ra, rb in read_input():
        if one_contains(ra, rb):
            count += 1
    print(count)


if __name__ == '__main__':
    main()
