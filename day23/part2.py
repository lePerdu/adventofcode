import itertools
import typing

import numpy as np

from part1 import (
    Coord, Direction, Grid, Move,
    get_proposed_moves, read_input, print_grid
)


def near_boundary(coord: Coord, grid: Grid) -> bool:
    return (
        coord.row <= 1 or coord.row >= grid.shape[0]-2 or
        coord.col <= 1 or coord.col >= grid.shape[1]-2
    )


def apply_moves_and_expand(grid: Grid, moves: typing.Iterable[Move]) -> Grid:
    # Apply the computed moves
    expand = False
    for src, dst in moves:
        grid[dst] = True
        grid[src] = False

        if near_boundary(dst, grid):
            expand = True

    if expand:
        # Double the grid size
        exp_row = grid.shape[0] // 2
        exp_col = grid.shape[1] // 2
        return np.pad(grid, ((exp_row, exp_row), (exp_col, exp_col)))
    else:
        return grid


def run_rounds_until_done(
    grid: Grid,
    init_direction_order: typing.Iterable[Direction],
) -> tuple[int, Grid]:
    direction_order = tuple(init_direction_order)
    for round_number in itertools.count(1):
        moves = get_proposed_moves(grid, direction_order)
        if len(moves) == 0:
            return round_number, grid
        grid = apply_moves_and_expand(grid, moves)
        print('Round', round_number)
        print('Grid size', grid.shape)
        # print_grid(grid)
        # print()
        direction_order = (*direction_order[1:], direction_order[0])

    raise Exception(f'Ended infinite loop')


def trim_grid(grid: Grid) -> Grid:
    """Trim grid to minimum bounding box of elves."""
    rows, cols = np.nonzero(grid)
    return grid[
        min(rows):max(rows)+1,
        min(cols):max(cols)+1,
    ].copy()  # Copy since indexing returns a view


ROUND_COUNT = 10


def main():
    grid = read_input()
    grid = np.pad(grid, 1)
    final_round, grid = run_rounds_until_done(grid, Direction)

    trimmed = trim_grid(grid)
    print_grid(trimmed)

    print('Final round', final_round)


if __name__ == '__main__':
    main()
