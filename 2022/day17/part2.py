import itertools
import math

from dataclasses import dataclass
import typing

from part1 import (
    BOARD_WIDTH, ROCK_SHAPES, Board, Game, Rock, Row, Rows,
    play_game, read_input
)

TOTAL_ROCK_COUNT = 1000000000000


class TrimmingBoard(Board):
    trimmed_height = 0

    def __init__(self, width: int):
        super().__init__(width)
        self.full_row = int(2**width - 1)

    @property
    def height(self) -> int:
        return self.trimmed_height + super().height

    def in_bounds(self, rock: Rock) -> bool:
        return rock.row >= self.trimmed_height and super().in_bounds(rock)

    def __getitem__(self, row: int) -> Row:
        return super().__getitem__(row - self.trimmed_height)

    def __setitem__(self, row: int, data: Row):
        super().__setitem__(row - self.trimmed_height, data)

    def add_rock(self, rock: Rock):
        super().add_rock(rock)

        # Check for full rows just added (starting from top)
        for pos in range(rock.row + rock.shape.height, rock.row-1, -1):
            if self[pos] == self.full_row:
                # Cut at that point
                cutoff_point = pos + 1 - self.trimmed_height
                self.rows = self.rows[cutoff_point:]
                self.trimmed_height += cutoff_point
                break

        # Check for full "pairs of rows" just added (starting from top)
        # If a pair of rows is full, blocks cannot get past the lower one
        # for pos in range(rock.row + rock.shape.height, rock.row-1, -1):
        #     if self[pos] | self[pos-1] == self.full_row:
        #         # Cut at that point
        #         cutoff_point = pos - self.trimmed_height
        #         self.rows = self.rows[cutoff_point:]
        #         self.trimmed_height += cutoff_point
        #         break


CompressedRows = int


def hash_rows(rows: list[Row], width: int) -> CompressedRows:
    result = 0
    for r in rows:
        result = (result << width) + r
    return result


@dataclass(frozen=True)
class State:
    # mod # moves
    move_count: int
    rows: CompressedRows


@dataclass(frozen=True)
class ResultState:
    height: int
    rock_count: int


def main():
    moves = read_input()
    move_count = len(moves)
    print(move_count)
    game = Game(ROCK_SHAPES, TrimmingBoard(BOARD_WIDTH))

    move_seq = itertools.cycle(moves)

    states: dict[State, ResultState] = {
        State(0, hash_rows([], 7)): ResultState(0, 0),
    }

    repeated_state: typing.Optional[State] = None

    assert TOTAL_ROCK_COUNT % len(ROCK_SHAPES) == 0
    for _ in range(TOTAL_ROCK_COUNT // len(ROCK_SHAPES)):
        play_game(
            game,
            move_seq,
            len(ROCK_SHAPES),
        )

        state = State(
            game.move_count % len(moves),
            hash_rows(game.board.rows, 7),
        )
        result_state = ResultState(
            game.board.height,
            game.rock_count,
        )

        if state in states:
            repeated_state = state
            break

        states[state] = result_state

    if repeated_state is None:
        print(f'Final height (no repeats): {game.board.height}')
        return

    print('After repeat')
    game.board.print()
    print()

    first_time = states[repeated_state]
    repeat_rock_count = game.rock_count - first_time.rock_count
    repeat_height_diff = game.board.height - first_time.height

    print(f'Repeated with state', repeated_state, result_state, game.move_count)
    print(f'Original state', first_time)

    # Play through another "round" manually
    for _ in range(repeat_rock_count):
        play_game(
            game,
            move_seq,
            1,
        )

    print('After repeat again')
    game.board.print()
    print()

    # Check that it actually repeats
    state = State(
        game.move_count % len(moves),
        hash_rows(game.board.rows, 7),
    )
    result_state = ResultState(
        game.board.height,
        game.rock_count,
    )

    print(f'After repeat again', state, result_state, game.move_count)

    assert state == repeated_state, 'States does not match'
    assert result_state.rock_count - first_time.rock_count == 2 * \
        repeat_rock_count, 'Rock count does not match'
    assert result_state.height - first_time.height == 2 * \
        repeat_height_diff, 'Height diff does not match'

    repeat_count, extra = divmod(
        TOTAL_ROCK_COUNT - first_time.rock_count,
        repeat_rock_count,
    )

    # Play through remaining manually
    for _ in range(extra):
        play_game(
            game,
            move_seq,
            1,
        )

    repeated_plus_extra_height = game.board.height

    final_height = repeated_plus_extra_height + \
        (repeat_count - 2) * repeat_height_diff
    print(f'Final height: {final_height}')


if __name__ == '__main__':
    main()
