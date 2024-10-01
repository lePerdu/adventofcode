import enum
import math
import typing

import numpy as np

from part1 import Direction, Turn, Coord, Board, Cell, Move, MoveForward, read_input
from folding import FoldedNet, fold_net


def compute_folded_net(board: Board) -> FoldedNet:
    # TODO This won't work if the board is square
    face_size = math.gcd(board.shape[0], board.shape[1])
    net_grid = board[::face_size, ::face_size] != Cell.SPACE
    return fold_net(net_grid, face_size)


class Game:
    board: Board

    sector: Coord
    face_position: Coord

    direction: Direction

    def __init__(self, board: Board):
        self.board = board
        self.folded_net = compute_folded_net(board)

        first_non_blank_col = np.where(self.board[0] != Cell.SPACE)[0][0]
        self.sector = Coord(
            row=0, col=first_non_blank_col // self.face_size
        )
        self.face_position = Coord(0, 0)
        self.direction = Direction.RIGHT

        self.history: list[tuple[Coord, Direction]] = []
        self._append_history()

    @property
    def face_size(self) -> int:
        return self.folded_net.face_size

    @property
    def position(self) -> Coord:
        return self.get_board_pos(self.face_position, self.sector)

    def get_sector_offset(self, sector: Coord) -> Coord:
        return sector * self.face_size

    def get_board_pos(self, face_pos: Coord, sector: Coord) -> Coord:
        return self.get_sector_offset(sector) + face_pos

    def in_face_bounds(self, coord: Coord) -> bool:
        return 0 <= coord[0] < self.face_size \
            and 0 <= coord[1] < self.face_size

    def get(self, c: Coord, sector: Coord) -> Cell:
        if self.in_face_bounds(c):
            return Cell(self.board[self.get_board_pos(c, sector)])
        else:
            return Cell.SPACE

    def __getitem__(self, i: Coord) -> Cell:
        return self.get(i, self.sector)

    def move(self, move: Move):
        match move:
            case MoveForward(dist):
                self._move_forward(dist)
            case Turn():
                self.direction = self.direction.turn(move)

    def _move_forward(self, distance: int):
        for _ in range(distance):
            try_pos = self.face_position.move(self.direction)
            if not self.in_face_bounds(try_pos):
                new_sector, new_pos, new_dir = self.folded_net.compute_new_face_coord(
                    src_pos=self.face_position,
                    src_sector=self.sector,
                    direction=self.direction,
                )
                if self.get(new_pos, new_sector) != Cell.EMPTY:
                    break

                self.face_position = new_pos
                self.sector = new_sector
                self.direction = new_dir
                self._append_history()
            elif self[try_pos] == Cell.EMPTY:
                self.face_position = try_pos
                self._append_history()
            else:
                # Hit a wall
                break

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
        print(f'Position: {self.face_position}, Direction: {self.direction}')
        print()


def calc_password(game: Game) -> int:
    return 1000 * (game.position.row + 1) + 4 * (game.position.col + 1) + game.direction.get_order()


def main():
    board, moves = read_input()
    print(f'Board shape: {board.shape}')
    print(f'Move count: {len(moves)}')

    game = Game(board)
    for m in moves:
        game.move(m)
    #     game.print()
    #     print()

    # game.print()
    # print()
    print(calc_password(game))


if __name__ == '__main__':
    main()
