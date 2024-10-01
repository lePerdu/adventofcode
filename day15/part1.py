import bisect
import re
import sys
import typing

from dataclasses import dataclass


@dataclass(frozen=True)
class Vec:
    x: int
    y: int

    def __len__(self) -> int:
        return abs(self.x) + abs(self.y)

    def __add__(self, other: 'Vec') -> 'Vec':
        return Vec(self.x + other.x, self.y + other.y)

    def __sub__(self, other: 'Vec') -> 'Vec':
        return Vec(self.x - other.x, self.y - other.y)


@dataclass(frozen=True)
class Slice:
    start: int
    end: int

    def __len__(self) -> int:
        return self.end - self.start + 1

    def __iter__(self) -> typing.Iterator[int]:
        return iter(range(self.start, self.end+1))

    def contains(self, other: 'Slice') -> bool:
        return self.start <= other.start and other.end <= self.end

    def overlaps(self, other: 'Slice') -> bool:
        """Overlaps or touches"""
        return not (self.end < other.start-1 or other.end < self.start-1)

    def try_merge(self, other: 'Slice') -> typing.Optional['Slice']:
        """Merge slices if they overlap"""
        if self.overlaps(other):
            return Slice(
                min(self.start, other.start), max(self.end, other.end)
            )
        else:
            return None


POINT_FORMAT = r'x=(-?\d+), y=(-?\d+)'
INPUT_FORMAT = re.compile(
    'Sensor at ' + POINT_FORMAT + ': closest beacon is at ' + POINT_FORMAT
)


@dataclass
class SensorLocation:
    sensor: Vec
    beacon: Vec

    def get_sensor_coverage(self) -> typing.Iterator[Vec]:
        """Return an iterator over all the points which this sensor covers.
        I.e. all the points which are the same or less distance away from its
        nearest beacon.
        """

        beacon_dist = len(self.sensor - self.beacon)
        for dx in range(-beacon_dist, beacon_dist+1):
            remaining_y_dist = beacon_dist - abs(dx)
            for dy in range(-remaining_y_dist, remaining_y_dist+1):
                # assert len(delta) <= beacon_dist, 'Delta is out of range'
                yield Vec(self.sensor.x + dx, self.sensor.y + dy)

    def get_sensor_row_coverage(self, y: int) -> typing.Optional[Slice]:
        beacon_dist = len(self.beacon - self.sensor)
        dy = y - self.sensor.y
        remaining_x_dist = beacon_dist - abs(dy)
        if remaining_x_dist < 0:
            return None
        return Slice(
            self.sensor.x - remaining_x_dist, self.sensor.x + remaining_x_dist
        )


def read_input() -> typing.Iterator[SensorLocation]:
    for line in sys.stdin:
        if (matches := INPUT_FORMAT.match(line)) is None:
            raise ValueError(f'Invalid input: {line}')
        sx, sy, bx, by = matches.groups()
        yield SensorLocation(
            Vec(int(sx), int(sy)),
            Vec(int(bx), int(by)),
        )


def merge_slices(slices: typing.Iterable[Slice]) -> list[Slice]:
    sorted_slices = sorted(slices, key=lambda s: s.start)

    result: list[Slice] = []
    for new_slice in sorted_slices:
        if len(result) > 0 and (merged := new_slice.try_merge(result[-1])):
            result[-1] = merged
        else:
            result.append(new_slice)

    return result


def get_coverage_for_row(
    sensors: typing.Iterable[SensorLocation], y: int
) -> typing.Iterator[Slice]:
    for sensor_loc in sensors:
        coverage = sensor_loc.get_sensor_row_coverage(y)
        if coverage is not None:
            yield coverage


ROW_OF_INTEREST = 2_000_000


def main():
    sensors = list(read_input())

    coverage = get_coverage_for_row(sensors, y=ROW_OF_INTEREST)
    non_overlapping_coverage = merge_slices(coverage)
    coverage_size = sum(len(c) for c in non_overlapping_coverage)

    beacons_at_y = len(
        set(s.beacon for s in sensors if s.beacon.y == ROW_OF_INTEREST)
    )
    count = coverage_size - beacons_at_y

    print()
    print(count)


if __name__ == '__main__':
    main()
