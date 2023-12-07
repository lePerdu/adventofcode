import enum
import itertools
import sys
import typing

import numpy as np


class Point(typing.NamedTuple):
    y: int
    x: int


Path = list[Point]

DROP_LOCATION = Point(y=0, x=500)


class Space(enum.IntEnum):
    EMPTY = 0
    ROCK = 1
    SAND = 2

    def to_char(self) -> str:
        match self:
            case Space.EMPTY: return '.'
            case Space.ROCK: return '#'
            case Space.SAND: return 'o'


class Grid:
    def __init__(self, min_bound: Point, max_bound: Point):
        self.min_bound = min_bound
        self.max_bound = max_bound
        self.grid = np.zeros(
            (max_bound.y - min_bound.y + 1, max_bound.x - min_bound.x + 1),
            dtype=np.ubyte,
        )

    def in_bounds(self, p: Point) -> bool:
        return (
            self.min_bound.x <= p.x <= self.max_bound.x and
            self.min_bound.y <= p.y <= self.max_bound.y
        )

    def __getitem__(self, p: Point) -> Space:
        return Space(self.grid[p.y-self.min_bound.y, p.x-self.min_bound.x])

    def __setitem__(self, p: Point, value: Space):
        self.grid[p.y-self.min_bound.y, p.x-self.min_bound.x] = value

    def print(self):
        for row in range(self.min_bound.y, self.max_bound.y+1):
            for col in range(self.min_bound.x, self.max_bound.x+1):
                coord = Point(y=row, x=col)
                if coord == DROP_LOCATION:
                    print('+', end='')
                else:
                    print(Space(self[coord]).to_char(), end='')
            print()


def read_point(s: str) -> Point:
    x, y = s.split(',', maxsplit=1)
    return Point(y=int(y), x=int(x))


def read_input() -> typing.Iterator[list[Point]]:
    for line in sys.stdin:
        yield [read_point(s) for s in line.split(' -> ')]


def calculate_bounds(paths: list[Path]) -> tuple[Point, Point]:
    def get_coords(index: int) -> typing.Iterator[int]:
        return itertools.chain(
            (point[index] for path in paths for point in path),
            (DROP_LOCATION[index],),
        )
    min_x = min(get_coords(1))
    max_x = max(get_coords(1))
    min_y = min(get_coords(0))
    max_y = max(get_coords(0))

    return (
        Point(y=min_y, x=min_x),
        Point(y=max_y, x=max_x),
    )


def get_points_in_line(start: Point, end: Point) -> typing.Iterable[Point]:
    if start.y == end.y:
        direction = 1 if end.x >= start.x else -1
        for x in range(start.x, end.x+direction, direction):
            yield Point(y=start.y, x=x)
    elif start.x == end.x:
        direction = 1 if end.y >= start.y else -1
        for y in range(start.y, end.y+direction, direction):
            yield Point(y=y, x=start.x)
    else:
        raise ValueError('Lines must be horizontal or vertical')


def get_points_in_path(path: Path) -> typing.Iterable[Point]:
    for i in range(len(path) - 1):
        yield from get_points_in_line(path[i], path[i+1])


def draw_paths(grid: Grid, paths: list[Path]):
    for path in paths:
        for point in get_points_in_path(path):
            grid[point] = Space.ROCK


def process_input() -> Grid:
    paths = list(read_input())
    min_bound, max_bound = calculate_bounds(paths)
    grid = Grid(min_bound, max_bound)
    draw_paths(grid, paths)
    return grid


def drop_sand(grid: Grid) -> bool:
    sand_pos = Point(*DROP_LOCATION)
    if grid[sand_pos] != Space.EMPTY:
        return False

    while True:
        below = Point(sand_pos.y+1, sand_pos.x)
        left = Point(sand_pos.y+1, sand_pos.x-1)
        right = Point(sand_pos.y+1, sand_pos.x+1)
        moved = False
        for check_pos in (below, left, right):
            if not grid.in_bounds(check_pos):
                return False
            if grid[check_pos] == Space.EMPTY:
                sand_pos = check_pos
                moved = True
                break
        if not moved:
            break

    grid[sand_pos] = Space.SAND
    return True


def main():
    grid = process_input()
    grid.print()
    print()
    sand_count = 0

    while True:
        if drop_sand(grid):
            sand_count += 1
        else:
            break
        # grid.print()
        # print()

    grid.print()
    print()
    print(sand_count)


if __name__ == '__main__':
    main()
