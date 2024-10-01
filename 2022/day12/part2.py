import typing

import numpy as np

from part1 import HeightMap, Position, find_path, print_path, read_input


def find_all_paths(height_map: HeightMap) -> typing.Iterable[list[Position]]:
    potential_starts = np.argwhere(height_map.grid == 0)

    for start in potential_starts:
        try:
            yield find_path(height_map, tuple(start))
        except:
            pass


def main():
    height_map = read_input()
    min_path = min(find_all_paths(height_map), key=lambda p: len(p))
    print_path(height_map, min_path)
    print(len(min_path) - 1)


if __name__ == '__main__':
    main()
