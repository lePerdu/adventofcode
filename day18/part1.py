import typing
import sys

Coord = tuple[int, int, int]


def read_input() -> typing.Iterator[Coord]:
    for line in sys.stdin:
        yield tuple(int(c) for c in line.split(','))


def adjacent(a: Coord, b: Coord) -> bool:
    return abs(a[0] - b[0]) + abs(a[1] - b[1]) + abs(a[2] - b[2]) == 1


def main():
    collected = []
    adjacents = 0
    for coord in read_input():
        for seen in collected:
            if adjacent(coord, seen):
                adjacents += 1
        collected.append(coord)

    print(6*len(collected) - 2*adjacents)


if __name__ == '__main__':
    main()
