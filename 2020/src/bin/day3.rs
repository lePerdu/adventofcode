use std::io::{stdin, Read};

use advent2020::grid;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Cell {
    Open,
    Tree,
}

type Grid = grid::Grid<Cell>;

struct RepeatingGrid {
    base_grid: Grid,
}

impl RepeatingGrid {
    fn new(base_grid: Grid) -> Self {
        Self { base_grid }
    }

    fn get(&self, (row, col): (usize, usize)) -> Option<&Cell> {
        let (_, col_count) = self.base_grid.dim();
        let col = col % col_count;
        self.base_grid.get((row, col))
    }
}

fn count_trees_along(grid: &RepeatingGrid, slope: (usize, usize)) -> usize {
    let coord_seq = std::iter::successors(Some((0, 0)), |(row, col)| {
        Some((row + slope.0, col + slope.1))
    });
    coord_seq
        .map_while(|coord| grid.get(coord))
        .filter(|&&cell| cell == Cell::Tree)
        .count()
}

fn part1(grid: &RepeatingGrid) {
    println!("Part1: {}", count_trees_along(grid, (1, 3)));
}

fn part2(grid: &RepeatingGrid) {
    const SLOPES: [(usize, usize); 5] = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)];

    let ans: usize = SLOPES
        .into_iter()
        .map(|slope| count_trees_along(grid, slope))
        .product();

    println!("Part2: {}", ans);
}

fn main() {
    let mut input_raw = String::new();
    stdin()
        .read_to_string(&mut input_raw)
        .expect("Failed to read input");
    let grid = parse::grid(&input_raw).expect("Failed to parse");
    let grid = RepeatingGrid::new(grid);
    part1(&grid);
    part2(&grid);
}

mod parse {
    use crate::*;

    fn cell(c: char) -> Option<Cell> {
        match c {
            '.' => Some(Cell::Open),
            '#' => Some(Cell::Tree),
            _ => None,
        }
    }

    pub(crate) fn grid(input: &str) -> Option<Grid> {
        grid::parse(input, cell)
    }
}
