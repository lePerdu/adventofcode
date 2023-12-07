from part1 import DROP_LOCATION, Grid, Point, calculate_bounds, draw_paths, drop_sand, read_input


def process_input() -> Grid:
    paths = list(read_input())
    min_bound, max_bound = calculate_bounds(paths)

    # Add in floor path and increase bounds to fit it
    floor_level = max_bound.y + 2
    floor_start = Point(y=floor_level, x=DROP_LOCATION.x - floor_level)
    floor_end = Point(y=floor_level, x=DROP_LOCATION.x + floor_level)

    paths.append([floor_start, floor_end])
    min_bound, max_bound = calculate_bounds(paths)

    grid = Grid(min_bound, max_bound)
    draw_paths(grid, paths)
    return grid


def main():
    grid = process_input()
    grid.print()
    print()
    sand_count = 0

    while True:
        if drop_sand(grid):
            sand_count += 1
        else:
            break
        # grid.print()
        # print()

    grid.print()
    print()
    print(sand_count)


if __name__ == '__main__':
    main()
