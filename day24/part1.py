import collections
import enum
import math
import sys
import typing

from dataclasses import dataclass

Time = int


class Direction(enum.Enum):
    N = '^'
    S = 'v'
    W = '<'
    E = '>'


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


@dataclass(frozen=True)
class Blizzard:
    position: Coord
    direction: Direction


class BlizzardsState:
    def __init__(self, blizzards: list[Blizzard]):
        self.blizzards = blizzards
        # Cache locations for faster lookup
        # Performance of creating the cache is negligible since it's only done
        # once per time step
        # TODO Use numpy array or some other structure?
        self.location_cache = {b.position for b in blizzards}

    def has_blizzard(self, c: Coord) -> bool:
        return c in self.location_cache

    def __iter__(self) -> typing.Iterator[Blizzard]:
        return iter(self.blizzards)


class Valley:
    """Class storing immutable data about the valley."""

    def __init__(
        self,
        shape: tuple[int, int],
        init_blizzards: list[Blizzard],
        start_col: int,
        end_col: int,
    ):
        self.shape = shape
        self.blizzard_states = [BlizzardsState(init_blizzards)]

        # Blizzard states repeat after everything has bounced
        blizzard_period = math.lcm(self.shape[0], self.shape[1])
        for _ in range(1, blizzard_period):
            self.blizzard_states.append(
                self._advance(self.blizzard_states[-1])
            )

        # These are off the board, but checked explicitly in in_bounds
        self.start_position = Coord(-1, start_col)
        self.end_position = Coord(self.shape[0], end_col)

    def in_bounds(self, c: Coord) -> bool:
        return (
            (0 <= c.row < self.shape[0] and 0 <= c.col < self.shape[1])
            # Check these manually since they are ouf of bounds
            or c == self.start_position or c == self.end_position
        )

    def get_blizzards_at_time(self, time: Time) -> BlizzardsState:
        """Get valley state at a given time.
        Values are memoized, since the valley state doesn't depend on the player
        """
        # Blizzard positions repeat after all bounce back
        return self.blizzard_states[time % len(self.blizzard_states)]

    def has_blizzard(self, time: Time, c: Coord) -> bool:
        # TODO Cache a set/grid/sorted list each round to make this faster?
        return self.get_blizzards_at_time(time).has_blizzard(c)

    def get_moves_from(self, start: Coord) -> typing.Iterator[Coord]:
        """Enumerate all in-bounds moves (not necessarily) valid ones."""
        # Staying at current position is always an option
        yield start
        for d in Direction:
            new_pos = start + d
            if self.in_bounds(new_pos):
                yield new_pos

    def _move_blizzard(self, b: Blizzard) -> Blizzard:
        new_pos = b.position + b.direction
        return Blizzard(
            position=Coord(
                new_pos.row % self.shape[0], new_pos.col % self.shape[1]
            ),
            direction=b.direction
        )

    def _advance(self, blizzards: BlizzardsState) -> BlizzardsState:
        return BlizzardsState([self._move_blizzard(b) for b in blizzards])


@dataclass(frozen=True)
class State:
    time: Time
    position: Coord


Path = list[Coord]


def find_path(
    valley: Valley, start: Coord, end: Coord, start_time: Time
) -> Path:
    # destionation -> shortest path to destination
    init_state = State(start_time, start)
    visited_paths: dict[State, Path] = {init_state: [init_state.position]}

    valley_size = valley.shape[0] * valley.shape[1]
    queue = collections.deque((init_state,), maxlen=valley_size)

    while len(queue) > 0:
        state = queue.popleft()
        for next_pos in valley.get_moves_from(state.position):
            next_state = State(state.time+1, next_pos)
            if valley.has_blizzard(next_state.time, next_state.position):
                continue

            if next_state in visited_paths:
                continue

            current_path = visited_paths[state]
            next_path = current_path + [next_state.position]
            visited_paths[next_state] = next_path
            if next_state.position == end:
                return next_path

            queue.append(next_state)

    raise Exception('No path found')


def read_input() -> Valley:
    # TODO Actually read start and end columns
    first_line = sys.stdin.readline()  # Skip first row
    columns = len(first_line.strip()) - 2

    blizzards: list[Blizzard] = []
    rows = 0
    for row, line in enumerate(sys.stdin):
        # Strip newline and walls
        line = line.strip()[1:-1]
        assert len(line) == columns
        if '#' in line:
            # Stop when there are walls inside the valley
            break

        rows += 1
        for col, char in enumerate(line):
            if char == '.':
                continue
            else:
                direction = Direction(char)
                blizzards.append(Blizzard(Coord(row, col), direction))

    shape = (rows, columns)
    return Valley(
        shape,
        blizzards,
        0,
        shape[1]-1
    )


def main():
    valley = read_input()

    to_end = find_path(
        valley,
        start=valley.start_position,
        end=valley.end_position,
        start_time=0,
    )
    time_to_end = len(to_end) - 1
    print('To end', time_to_end)
    back_to_start = find_path(
        valley,
        start=valley.end_position,
        end=valley.start_position,
        start_time=time_to_end,
    )
    time_back_to_start = len(back_to_start) - 1
    print('To start', time_back_to_start)
    back_to_end = find_path(
        valley,
        start=valley.start_position,
        end=valley.end_position,
        start_time=len(to_end) - 1 + len(back_to_start) - 1,
    )
    time_back_to_end = len(back_to_end) - 1
    print('To start', time_back_to_end)

    print('Total', time_to_end + time_back_to_start + time_back_to_end)


if __name__ == '__main__':
    main()
