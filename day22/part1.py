import enum
import re
import sys
import termios
import typing

from dataclasses import dataclass

import numpy as np
import numpy.typing as npt


class Cell(enum.IntEnum):
    SPACE = 0
    WALL = 1
    EMPTY = 2

    @classmethod
    def from_char(cls, char: str) -> 'Cell':
        match char:
            case ' ': return Cell.SPACE
            case '#': return Cell.WALL
            case '.': return Cell.EMPTY
            case _: raise ValueError(f'Invalid cell character: {char}')


Board = np.ndarray[tuple[int, int], np.dtype[np.uint8]]


@dataclass(frozen=True)
class MoveForward:
    distance: int


class Turn(enum.Enum):
    LEFT = 'L'
    RIGHT = 'R'


Move = MoveForward | Turn


class Direction(enum.Enum):
    UP = '^'
    DOWN = 'v'
    LEFT = '<'
    RIGHT = '>'

    ORDER: tuple['Direction', ...]

    @staticmethod
    def from_delta(delta_row: int, delta_col: int) -> typing.Optional['Direction']:
        match delta_row, delta_col:
            case 0, 1: return Direction.RIGHT
            case 1, 0: return Direction.DOWN
            case 0, -1: return Direction.LEFT
            case -1, 0: return Direction.UP

    def get_order(self) -> int:
        return Direction.ORDER.index(self)

    def turn(self, turn: Turn):
        current = self.get_order()
        match turn:
            case Turn.LEFT:
                return Direction.ORDER[(current-1) % len(Direction.ORDER)]
            case Turn.RIGHT:
                return Direction.ORDER[(current+1) % len(Direction.ORDER)]

    def __str__(self) -> str:
        return self.value


Direction.ORDER = (Direction.RIGHT, Direction.DOWN,
                   Direction.LEFT, Direction.UP)


class Coord(typing.NamedTuple):
    row: int
    col: int

    @staticmethod
    def from_direction(d: Direction) -> 'Coord':
        match d:
            case Direction.UP: return Coord(-1, 0)
            case Direction.DOWN: return Coord(1, 0)
            case Direction.LEFT: return Coord(0, -1)
            case Direction.RIGHT: return Coord(0, 1)

    def move(self, d: Direction) -> 'Coord':
        return self + Coord.from_direction(d)

    def __add__(self, other: 'Coord') -> 'Coord':
        return Coord(self.row + other.row, self.col + other.col)

    def __mul__(self, scale: int) -> 'Coord':
        return Coord(self.row * scale, self.col * scale)


class Game:
    board: Board
    position: Coord
    direction: Direction

    def __init__(self, board: Board):
        self.board = board
        first_blank = self._find_space_in_dir(Coord(0, 0), Direction.RIGHT)
        if first_blank is None:
            raise Exception('Cannot find starting position')
        self.position = first_blank
        self.direction = Direction.RIGHT

        self.history: list[tuple[Coord, Direction]] = []
        self._append_history()

    def move(self, move: Move):
        match move:
            case MoveForward(dist):
                self._move_forward(dist)
            case Turn():
                self.direction = self.direction.turn(move)

    def __getitem__(self, i: tuple[int, int]) -> Cell:
        try:
            return Cell(self.board[i])
        except IndexError:
            return Cell.SPACE

    def _move_forward(self, distance: int):
        for _ in range(distance):
            try_pos = self.position.move(self.direction)
            match self[try_pos.row, try_pos.col]:
                case Cell.SPACE:
                    # Wrap around
                    wrapped_pos = self._find_space_in_dir(
                        try_pos, self.direction
                    )
                    if wrapped_pos is None:
                        break
                    try_pos = wrapped_pos
                case Cell.WALL:
                    # Blocked, so don't update position, and stop
                    break
                case Cell.EMPTY:
                    pass

            self.position = try_pos
            self._append_history()

    def _find_space_in_dir(self, start: Coord, d: Direction) -> typing.Optional[Coord]:
        match d:
            case Direction.RIGHT:
                col = np.where(self.board[start.row] != Cell.SPACE)[0][0]
                c = Coord(start.row, col)
            case Direction.UP:
                row = np.where(self.board[:, start.col] != Cell.SPACE)[0][-1]
                c = Coord(row, start.col)
            case Direction.LEFT:
                col = np.where(self.board[start.row] != Cell.SPACE)[0][-1]
                c = Coord(start.row, col)
            case Direction.DOWN:
                row = np.where(self.board[:, start.col] != Cell.SPACE)[0][0]
                c = Coord(row, start.col)

        if self.board[c.row, c.col] == Cell.EMPTY:
            return c
        else:
            return None

    def _append_history(self):
        self.history.append((self.position, self.direction))

    def _find_history_dir(self, position: Coord) -> typing.Optional[Direction]:
        for p, d in self.history:
            if p == position:
                return d
        return None

    def print(self):
        for row in range(len(self.board)):
            for col in range(len(self.board[row])):
                if (d := self._find_history_dir(Coord(row, col))) is not None:
                    print(str(d), end='')
                elif self.board[row, col] == Cell.EMPTY:
                    print('.', end='')
                elif self.board[row, col] == Cell.WALL:
                    print('#', end='')
                else:
                    print(' ', end='')

            print()
        print()
        print(f'Position: {self.position}, Direction: {self.direction}')
        print()


def read_board() -> Board:
    board_rows: list[str] = []
    for line in sys.stdin:
        if line.isspace():
            break

        # Remove newline, but keep spaces at the beginning or end
        board_rows.append(line.strip('\n'))

    column_count = max(len(r) for r in board_rows)
    for i in range(len(board_rows)):
        extra_spaces = max(column_count - len(board_rows[i]), 0)
        board_rows[i] = board_rows[i] + ' ' * extra_spaces

    board_cells = [[Cell.from_char(c) for c in row] for row in board_rows]
    board = np.array(board_cells, dtype=np.uint8)

    return board


MOVE_PATTERN = re.compile(r'\d+|R|L')


def parse_move(s: str) -> Move:
    try:
        n = int(s)
        return MoveForward(n)
    except ValueError:
        pass

    match s:
        case 'R': return Turn.RIGHT
        case 'L': return Turn.LEFT

    raise ValueError(f'Invalid move: {s}')


def read_moves() -> list[Move]:
    line = sys.stdin.readline()
    matches = MOVE_PATTERN.finditer(line)
    return [parse_move(m.group()) for m in matches]


def read_input() -> tuple[Board, list[Move]]:
    if sys.stdin.isatty():
        LFLAG = 3
        original_settings = termios.tcgetattr(sys.stdin.fileno())
        settings = list(original_settings)
        settings[LFLAG] &= ~termios.ICANON
        termios.tcsetattr(sys.stdin.fileno(), termios.TCSANOW, settings)

    try:
        result = (read_board(), read_moves())
    finally:
        if sys.stdin.isatty():
            termios.tcsetattr(
                sys.stdin, termios.TCSANOW, original_settings  # type: ignore
            )

    return result


def calc_password(game: Game) -> int:
    return 1000 * (game.position.row + 1) + 4 * (game.position.col + 1) + game.direction.get_order()


def main():
    board, moves = read_input()
    print(f'Board shape: {board.shape}')
    print(f'Move count: {len(moves)}')

    game = Game(board)
    for m in moves:
        game.move(m)
        # game.print()
        # print()

    game.print()
    print()
    print(calc_password(game))


if __name__ == '__main__':
    main()
