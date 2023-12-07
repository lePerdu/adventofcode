import sys
import typing

from collections import deque
from dataclasses import dataclass

import numpy as np
import numpy.typing as npt


Position = tuple[int, int]


@dataclass
class HeightMap:
    grid: npt.NDArray[np.int8]
    start: Position
    end: Position

    def in_bounds(self, pos: Position) -> bool:
        return (
            0 <= pos[0] < self.grid.shape[0] and
            0 <= pos[1] < self.grid.shape[1]
        )

    def get_moves_from(self, pos: Position) -> typing.Iterator[Position]:
        potential = (
            (pos[0]-1, pos[1]),
            (pos[0], pos[1]-1),
            (pos[0]+1, pos[1]),
            (pos[0], pos[1]+1),
        )

        return (
            p for p in potential
            if self.in_bounds(p) and can_move_to(self[pos], self[p])
        )

    def __getitem__(self, pos: Position) -> int:
        return self.grid[pos]


def can_move_to(src_height: int, dst_height: int) -> bool:
    return dst_height - src_height <= 1


def parse_elevation(char: str) -> int:
    if char == 'S':
        return 0
    if char == 'E':
        return 25
    if 'a' <= char <= 'z':
        return ord(char) - ord('a')
    else:
        raise Exception(f'Invalid elevation: {char}')


def read_input() -> HeightMap:
    start_pos: typing.Optional[tuple[int, int]] = None
    end_pos: typing.Optional[tuple[int, int]] = None
    grid: list[list[int]] = []
    for line in sys.stdin:
        row: list[int] = []
        for char in line.strip():
            pos = (len(grid), len(row))
            if char == 'S':
                start_pos = pos
            elif char == 'E':
                end_pos = pos
            row.append(parse_elevation(char))
        grid.append(row)

    if start_pos is None or end_pos is None:
        raise Exception('Start/end not found')

    return HeightMap(np.array(grid, dtype=np.int_), start_pos, end_pos)


def find_path(
    height_map: HeightMap, start: typing.Optional[Position] = None
) -> list[Position]:
    if start is None:
        start = height_map.start
    distance = np.full_like(height_map.grid, -1, dtype=np.int_)
    predecessor = np.full(
        (height_map.grid.shape[0], height_map.grid.shape[1], 2),
        -1,
        dtype=np.int_,
    )

    def has_visited(pos: Position) -> bool:
        return distance[pos] != -1

    def walk_backward() -> list[Position]:
        path = deque((height_map.end,))
        while tuple(path[0]) != start:
            path.appendleft(tuple(predecessor[path[0]]))
        return list(path)

    distance[start] = 0

    # Max size of queue is number of vertices
    move_queue = deque((start,), maxlen=height_map.grid.size)
    while len(move_queue) > 0:
        pos = move_queue.popleft()
        for new_pos in height_map.get_moves_from(pos):
            if has_visited(new_pos):
                continue

            distance[new_pos] = distance[pos] + 1
            predecessor[new_pos] = pos

            if new_pos == height_map.end:
                return walk_backward()

            move_queue.append(new_pos)

    raise Exception('No path found')


def get_direction(src: Position, dst: Position) -> str:
    dy, dx = dst[0]-src[0], dst[1]-src[1]
    match dy, dx:
        case 0, -1: return '<'
        case 0,  1: return '>'
        case -1, 0: return '^'
        case  1, 0: return 'v'
        case _: raise ValueError(f'Invalid move: {src} -> {dst}')


def print_path(height_map: HeightMap, path: list[Position]):
    print_grid = np.full_like(height_map.grid, ord('.'), dtype=np.ubyte)
    print_grid[path[-1]] = ord('E')

    for i in range(0, len(path)-1):
        print_grid[path[i]] = ord(get_direction(path[i], path[i+1]))

    for row in print_grid:
        print(''.join(chr(c) for c in row))


def main():
    height_map = read_input()
    path = find_path(height_map)
    print_path(height_map, path)
    print(len(path) - 1)


if __name__ == '__main__':
    main()
