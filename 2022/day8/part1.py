import itertools
import sys
import typing

from dataclasses import dataclass

Direction = typing.Literal['up', 'right', 'down', 'left']

ALL_DIRECTIONS = ('up', 'right', 'down', 'left')


@dataclass
class Coord:
    x: int
    y: int

    def add_dir(self, direction: Direction) -> 'Coord':
        match direction:
            case 'up':
                return Coord(self.x, self.y-1)
            case 'right':
                return Coord(self.x+1, self.y)
            case 'down':
                return Coord(self.x, self.y+1)
            case 'left':
                return Coord(self.x-1, self.y)


class Grid:
    def __init__(self, trees: list[list[int]]):
        # Verify grid is uniform
        first_len = len(trees[0])
        for i in range(1, len(trees)):
            if first_len != len(trees[i]):
                raise ValueError('Tree grid not of uniform length')
        self.trees = trees

    @property
    def width(self) -> int:
        return len(self.trees[0])

    @property
    def height(self) -> int:
        return len(self.trees)

    def at_edge(self, coord: Coord) -> bool:
        return (
            coord.y <= 0 or coord.y >= len(self.trees)-1
            or
            coord.x <= 0 or coord.x >= len(self.trees[coord.y])-1
        )

    def is_oob(self, coord: Coord) -> bool:
        return (
            coord.y < 0 or coord.y >= len(self.trees)
            or
            coord.x < 0 or coord.x >= len(self.trees[coord.y])
        )

    def all_coords(self) -> typing.Iterator[Coord]:
        return (
            Coord(x, y)
            for x, y in itertools.product(
                range(self.width), range(self.height)
            )
        )

    def __getitem__(self, c: Coord) -> int:
        return self.trees[c.y][c.x]


def read_input() -> Grid:
    return Grid([
        [int(c) for c in line.strip()]
        for line in sys.stdin
    ])


def get_coords_in_dir(
    coord: Coord, direction: Direction, grid: Grid
) -> typing.Iterator[Coord]:
    while True:
        coord = coord.add_dir(direction)
        if grid.is_oob(coord):
            break
        yield coord


def is_coord_visible(coord: Coord, grid: Grid) -> bool:
    if grid.at_edge(coord):
        print(f'{coord} at edge')
        return True

    coord_height = grid[coord]
    for direction in ALL_DIRECTIONS:
        # Just need to find 1 direction which is clear
        clear_in_dir = True
        for check_coord in get_coords_in_dir(coord, direction, grid):
            if grid[check_coord] >= coord_height:
                clear_in_dir = False
                break

        if clear_in_dir:
            print(f'{coord} visible from {direction}')
            return True

    return False


def main():
    grid = read_input()

    count = 0
    for c in grid.all_coords():
        if is_coord_visible(c, grid):
            count += 1

    print(count)


if __name__ == '__main__':
    main()
