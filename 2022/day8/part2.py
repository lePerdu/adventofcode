from part1 import ALL_DIRECTIONS, Coord, Direction, Grid, get_coords_in_dir, read_input


def get_distance_in_dir(coord: Coord, grid: Grid, direction: Direction) -> int:
    coord_height = grid[coord]
    distance = 0
    for check_coord in get_coords_in_dir(coord, direction, grid):
        distance += 1
        if grid[check_coord] >= coord_height:
            break

    return distance


def compute_scenic_score(coord: Coord, grid: Grid) -> int:
    score = 1
    for d in ALL_DIRECTIONS:
        score *= get_distance_in_dir(coord, grid, d)
    return score


def main():
    grid = read_input()

    print(max(compute_scenic_score(c, grid) for c in grid.all_coords()))


if __name__ == '__main__':
    main()
