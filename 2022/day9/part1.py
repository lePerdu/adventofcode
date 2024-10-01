import sys
import typing

from dataclasses import dataclass

Direction = typing.Literal['up', 'down', 'left', 'right']


@dataclass(frozen=True)
class Vec:
    x: int = 0
    y: int = 0

    def __abs__(self) -> int:
        return max(abs(self.x), abs(self.y))

    def norm(self) -> 'Vec':
        def norm_val(v: int) -> int:
            # kind of like an int version of copysign, but keep 0
            return 0 if v == 0 else v // abs(v)

        return Vec(norm_val(self.x), norm_val(self.y))


@dataclass(frozen=True)
class Coord:
    x: int
    y: int

    def __add__(self, other: Vec | Direction) -> 'Coord':
        match other:
            case Vec(dx, dy): return Coord(self.x+dx, self.y+dy)
            case 'up': return Coord(self.x, self.y+1)
            case 'down': return Coord(self.x, self.y-1)
            case 'left': return Coord(self.x-1, self.y)
            case 'right': return Coord(self.x+1, self.y)

    def __sub__(self, other: 'Coord') -> Vec:
        return Vec(self.x - other.x, self.y - other.y)


def get_tail_move(head: Coord, tail: Coord) -> Vec:
    diff = head - tail
    if abs(diff) > 1:
        return diff.norm()
    else:
        return Vec()


@dataclass(frozen=True)
class Step:
    direction: Direction
    distance: int


def parse_direction(dir_str: str) -> Direction:
    match dir_str:
        case 'U': return 'up'
        case 'D': return 'down'
        case 'L': return 'left'
        case 'R': return 'right'
        case _: raise ValueError(f'Invalid direction: `{dir_str}`')


def read_input() -> typing.Iterator[Step]:
    for line in sys.stdin:
        direction, distance = line.split(maxsplit=1)
        yield Step(parse_direction(direction), int(distance))


def main():
    steps = read_input()

    head = Coord(0, 0)
    tail = head
    visited: set[Coord] = {tail}

    def print_state():
        xs = [c.x for c in visited]
        ys = [c.y for c in visited]
        min_x = min(*xs, 0)
        min_y = min(*ys, 0)
        max_x = max(xs)
        max_y = max(ys)

        for y in range(max_y, min_y-1, -1):
            for x in range(min_x, max_x+1):
                coord = Coord(x, y)
                char = '.'  # Default
                if coord == head:
                    char = 'H'
                elif coord == tail:
                    char = 'T'
                elif coord == Coord(0, 0):
                    char = 's'
                elif coord in visited:
                    char = '#'

                print(char, end='')

            print()
        print()

    print_state()
    for step in steps:
        for _ in range(step.distance):
            head += step.direction
            tail += get_tail_move(head, tail)
            # print_state()
            visited.add(tail)

    head = None
    tail = None

    print('Final result:')
    print_state()
    print(len(visited))


if __name__ == '__main__':
    main()
