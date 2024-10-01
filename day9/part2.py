import itertools
import typing

from part1 import Coord, Vec, get_tail_move, read_input

ROPE_LENGTH = 10


def compute_grid_size(
    coords: typing.Iterator[Coord], base_size: Vec = Vec(5, 4)
) -> tuple[Vec, Vec]:
    min_x, min_y, max_x, max_y = 0, 0, base_size.x, base_size.y
    for c in coords:
        min_x = min(min_x, c.x)
        min_y = min(min_y, c.y)
        max_x = max(max_x, c.x)
        max_y = max(max_y, c.y)

    return Vec(min_x, min_y), Vec(max_x, max_y)


def main():
    steps = read_input()

    rope = [Coord(0, 0)] * ROPE_LENGTH
    visited: set[Coord] = {rope[-1]}

    def print_state():
        min_range, max_range = compute_grid_size(
            itertools.chain(rope, visited)
        )

        for y in range(max_range.y, min_range.y-1, -1):
            for x in range(min_range.x, max_range.x+1):
                coord = Coord(x, y)
                char = None
                try:
                    rope_index = rope.index(coord)
                    char = 'H' if rope_index == 0 else str(rope_index)
                except:
                    pass

                if char is not None:
                    pass
                elif coord == Coord(0, 0):
                    char = 's'
                elif coord in visited:
                    char = '#'
                else:
                    char = '.'

                print(char, end='')

            print()
        print()

    print('== Initial State ==')
    print_state()

    for step in steps:
        print(f'== {step.direction} {step.distance} ==')
        print()
        for _ in range(step.distance):
            # Move head first
            rope[0] += step.direction

            # Move rest of the rope 1-by-1
            for rope_index in range(1, len(rope)):
                rope[rope_index] += get_tail_move(
                    rope[rope_index-1], rope[rope_index]
                )

            # print_state()
            visited.add(rope[-1])

    rope = []
    print('== Final State ==')
    print()
    print_state()

    print('== Result ==')
    print(len(visited))


if __name__ == '__main__':
    main()
