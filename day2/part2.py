import enum
import sys
import typing

from part1 import Move


class Result(enum.IntEnum):
    WIN = 1
    LOOSE = 2
    TIE = 3

    @staticmethod
    def from_char(c: str) -> typing.Optional['Result']:
        match c:
            case 'X': return Result.LOOSE
            case 'Y': return Result.TIE
            case 'Z': return Result.WIN
        return None


Round = tuple[Move, Result]


def read_input() -> typing.Generator[Round, None, None]:
    for line in sys.stdin:
        opponent, result, *_ = line.split()
        opponent, result = Move.from_char(opponent), Result.from_char(result)
        if opponent is None or result is None:
            raise Exception(f'Invalid moves: {line}')
        yield (opponent, result)


def compute_round_score(round: Round) -> int:
    match round[1]:
        case Result.WIN: return 6 + round[0].get_winning_move().score
        case Result.LOOSE: return round[0].get_loosing_move().score
        case Result.TIE: return 3 + round[0].score


def main():
    print(sum(compute_round_score(r) for r in read_input()))


if __name__ == '__main__':
    main()
