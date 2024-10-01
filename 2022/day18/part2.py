import collections
import itertools
import typing

import numpy as np
import numpy.typing as npt

from part1 import Coord, read_input

directions = np.array([
    [1, 0, 0],
    [-1, 0, 0],
    [0, 1, 0],
    [0, -1, 0],
    [0, 0, 1],
    [0, 0, -1],
], dtype=np.int8)


def get_adjacents(coord: Coord, shape: Coord) -> typing.Iterator[Coord]:
    for d in directions:
        c = coord + d
        if all(c >= 0) and all(c < shape):
            yield tuple(c)


def find_exterior_spaces(space: npt.NDArray[np.bool_]) -> npt.NDArray[np.bool_]:
    exterior = np.zeros_like(space, dtype=np.bool_)

    queue = collections.deque[Coord]()

    # Mark boundaries as exterior
    exterior[[0, space.shape[0]-1], :, :] = True
    exterior[:, [0, space.shape[1]-1], :] = True
    exterior[:, :, [0, space.shape[2]-1]] = True

    ext_coords = np.argwhere(exterior)
    for c in ext_coords:
        queue.append(tuple(c))

    while len(queue) > 0:
        coord = queue.popleft()
        exterior[coord] = True
        for next_coord in get_adjacents(coord, space.shape):
            if exterior[next_coord] or space[next_coord]:
                continue
            queue.append(next_coord)

    return exterior


def count_adj_exterior(
    coord: Coord,
    exterior: npt.NDArray[np.bool_],
) -> int:
    count = 0
    for adj in get_adjacents(coord, exterior.shape):
        if exterior[adj]:
            count += 1
    return count


def main():
    coords = np.array(list(read_input()), dtype=np.int8)

    space_shape = np.max(coords, axis=0) + 2
    space = np.zeros(space_shape, dtype=np.bool_)
    for c in coords:
        space[tuple(c)] = True

    exterior = find_exterior_spaces(space)
    total = sum(
        count_adj_exterior(c, exterior) for c in coords
    )

    print(total)


if __name__ == '__main__':
    main()
