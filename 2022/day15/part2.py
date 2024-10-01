import typing

from part1 import (
    SensorLocation, Slice, Vec, get_coverage_for_row, merge_slices, read_input
)

GRID_SIZE = 4_000_000


def subtract_slices(
    outer: Slice, inner: list[Slice]
) -> typing.Iterator[int]:
    if outer.start < inner[0].start:
        yield from Slice(outer.start, inner[0].start)

    for index in range(1, len(inner)):
        # No need to check, as inner is already merged/de-duped
        yield from Slice(inner[index-1].end+1, inner[index].start-1)

    if inner[-1].end < outer.end:
        yield from Slice(inner[-1].end, outer.end)


def find_gaps(
    sensors: list[SensorLocation], grid_size=GRID_SIZE
) -> typing.Iterator[Vec]:
    full_row_slice = Slice(0, grid_size)
    for row in range(GRID_SIZE+1):
        coverage = get_coverage_for_row(sensors, y=row)
        non_overlapping_coverage = merge_slices(coverage)
        covers_full_row = (
            len(non_overlapping_coverage) == 1
            and non_overlapping_coverage[0].contains(full_row_slice)
        )
        if not covers_full_row:
            yield from (
                Vec(x, row)for x in subtract_slices(
                    full_row_slice, non_overlapping_coverage
                )
            )


def main():
    sensors = list(read_input())

    for gap in find_gaps(sensors):
        print(gap)


if __name__ == '__main__':
    main()
