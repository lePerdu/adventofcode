import enum
import sys
import typing

import numpy as np


class Direction(enum.Enum):
    N = 'N'
    S = 'S'
    W = 'W'
    E = 'E'


class Coord(typing.NamedTuple):
    row: int
    col: int

    @staticmethod
    def from_direction(d: Direction) -> 'Coord':
        match d:
            case Direction.N: return Coord(-1, 0)
            case Direction.S: return Coord(1, 0)
            case Direction.W: return Coord(0, -1)
            case Direction.E: return Coord(0, 1)

    def __add__(self, other: typing.Union['Coord', Direction]) -> 'Coord':
        if isinstance(other, Direction):
            other = Coord.from_direction(other)
        return Coord(self.row + other.row, self.col + other.col)

    def __mul__(self, scale: int) -> 'Coord':
        return Coord(self.row * scale, self.col * scale)


Grid = np.ndarray[Coord, np.dtype[np.bool_]]


def read_cell(char: str) -> bool:
    if char == '.':
        return False
    elif char == '#':
        return True
    else:
        raise ValueError(f'Invalid character: {char}')


def read_input() -> Grid:
    return np.array([
        [read_cell(c) for c in line.strip()]
        for line in sys.stdin.readlines()
    ], dtype=np.bool_)


def propose_move(
    grid: Grid, elf: Coord, direction_order: typing.Iterable[Direction]
) -> typing.Optional[Coord]:
    """Get an elf's proposed move.
    Assumes elf is surrounded by cells in all directions.
    """
    assert grid[elf], f'No elf at position {elf}'
    surrounding = grid[elf.row-1:elf.row+2, elf.col-1:elf.col+2]

    # Do nothing if no elves nearby
    if np.sum(surrounding) == 1:
        return None

    for d in direction_order:
        match d:
            case Direction.N:
                check_spaces = surrounding[0, :]
            case Direction.S:
                check_spaces = surrounding[2, :]
            case Direction.W:
                check_spaces = surrounding[:, 0]
            case Direction.E:
                check_spaces = surrounding[:, 2]

        # Pick first empty direction found
        if np.all(check_spaces == False):
            return elf + d

    return None


class Move(typing.NamedTuple):
    src: Coord
    dst: Coord


def get_proposed_moves(
    grid: Grid, direction_order: typing.Iterable[Direction]
) -> list[Move]:
    # TODO Ensure grid is big enough? (covered in propose_move for now)

    # destination: source | blocked
    all_proposed_moves: dict[Coord, typing.Optional[Coord]] = {}
    for row, col in np.argwhere(grid):

        elf = Coord(row, col)
        proposed = propose_move(grid, elf, direction_order)
        if proposed is None:
            pass
        elif proposed in all_proposed_moves:
            # If more than 1 elf wanted to move here, block the space off
            all_proposed_moves[proposed] = None
        else:
            all_proposed_moves[proposed] = elf

    return [
        Move(src, dst)
        for dst, src in all_proposed_moves.items()
        if src is not None
    ]


def apply_moves(grid: Grid, moves: typing.Iterable[Move]):
    # Apply the computed moves
    for src, dst in moves:
        grid[dst] = True
        grid[src] = False


def print_grid(grid: Grid):
    for row in grid:
        for col in row:
            print('#' if col else '.', end='')
        print()


def run_rounds(
    grid: Grid,
    round_count: int,
    init_direction_order: typing.Iterable[Direction],
):
    direction_order = tuple(init_direction_order)
    for r in range(round_count):
        moves = get_proposed_moves(grid, direction_order)
        apply_moves(grid, moves)
        print('Round', r)
        print_grid(grid)
        print()
        direction_order = (*direction_order[1:], direction_order[0])


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
    grid = np.pad(grid, ROUND_COUNT + 1)
    run_rounds(grid, ROUND_COUNT, Direction)

    trimmed = trim_grid(grid)
    print_grid(trimmed)
    print('Empty spaces', np.count_nonzero(~trimmed))


if __name__ == '__main__':
    main()
