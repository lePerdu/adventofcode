import enum
import sys
import typing


class Move(enum.IntEnum):
    ROCK = 1
    PAPER = 2
    SCISSORS = 3

    @staticmethod
    def from_char(c: str) -> typing.Optional['Move']:
        match c:
            case 'A' | 'X': return Move.ROCK
            case 'B' | 'Y': return Move.PAPER
            case 'C' | 'Z': return Move.SCISSORS
        return None

    @property
    def score(self):
        return self.value

    def score_against(self, other: 'Move') -> int:
        match (self.value - other.value) % 3:
            case 0: return 3
            case 1: return 6
            case 2: return 0
            case _: raise  # Impossible

    def get_winning_move(self) -> 'Move':
        match self:
            case Move.ROCK: return Move.PAPER
            case Move.PAPER: return Move.SCISSORS
            case Move.SCISSORS: return Move.ROCK

    def get_loosing_move(self) -> 'Move':
        match self:
            case Move.ROCK: return Move.SCISSORS
            case Move.PAPER: return Move.ROCK
            case Move.SCISSORS: return Move.PAPER


Round = tuple[Move, Move]


def read_input() -> typing.Generator[Round, None, None]:
    for line in sys.stdin:
        opponent, own, *_ = line.split()
        opponent, own = Move.from_char(opponent), Move.from_char(own)
        if opponent is None or own is None:
            raise Exception(f'Invalid moves: {line}')
        yield (opponent, own)


def compute_round_score(round: Round) -> int:
    return round[1].score + round[1].score_against(round[0])


def main():
    print(sum(compute_round_score(r) for r in read_input()))


if __name__ == '__main__':
    main()
