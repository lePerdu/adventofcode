use std::io::{stdin, Read};

use advent2020::grid::Grid;
use ndarray::{Array, Dim, Dimension, IntoDimension, Ix, Ixs, NdIndex, Slice};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Cell {
    Active,
    Inactive,
}

type Input = Grid<Cell>;

#[derive(Debug)]
struct ConwayGrid<const D: usize>
where
    Dim<[Ix; D]>: Dimension,
{
    grid: Array<Cell, Dim<[Ix; D]>>,
}

fn check_coord(x: isize, axis_len: usize) -> Option<usize> {
    if 0 <= x && x < axis_len as isize {
        Some(x as usize)
    } else {
        None
    }
}

fn check_nd_coord<const D: usize>(coord: [Ixs; D], axes: Dim<[Ix; D]>) -> Option<[Ix; D]>
where
    Dim<[Ix; D]>: Dimension,
{
    let mut final_coord = [0; D];
    for i in 0..D {
        final_coord[i] = check_coord(coord[i], axes[i])?;
    }
    Some(final_coord)
}

impl<const D: usize> ConwayGrid<D>
where
    Dim<[Ix; D]>: Dimension,
    [Ix; D]: NdIndex<Dim<[Ix; D]>>,
{
    pub fn new(grid: Array<Cell, Dim<[Ix; D]>>) -> Self {
        Self { grid }
    }

    pub fn from_slice(input: &Grid<Cell>) -> Self {
        let mut final_shape = Dim::<[Ix; D]>::default();
        final_shape[0] = input.dim().0;
        final_shape[1] = input.dim().1;
        for i in 2..final_shape.ndim() {
            final_shape[i] = 1;
        }
        Self::new(
            input
                .to_shape(final_shape)
                .expect("Could not reshape")
                .to_owned(),
        )
    }

    pub fn get(&self, coord: [Ixs; D]) -> Cell {
        check_nd_coord(coord, self.grid.dim().into_dimension())
            .map_or(Cell::Inactive, |c| self.grid[c])
    }

    pub fn count_neighbors(&self, coord: [Ixs; D]) -> u8 {
        // Array version of `coord` to make indexing easier
        let view = self.grid.slice_each_axis(|ax| {
            let coord_val: isize = coord[ax.axis.index()];
            let low = (coord_val - 1).max(0) as usize;
            let high = (coord_val + 1).min(ax.len as isize - 1) as usize;
            Slice::from(low..=high)
        });

        let active_in_view = view.iter().filter(|cell| **cell == Cell::Active).count() as u8;
        // Subtract out the center if necessary
        if self.get(coord) == Cell::Active {
            active_in_view - 1
        } else {
            active_in_view
        }
    }

    pub fn run_cycle(&self) -> Self {
        let mut new_state = Array::from_elem(
            {
                let mut d = self.grid.raw_dim();
                for i in 0..D {
                    d[i] += 2;
                }
                d
            },
            Cell::Inactive,
        );

        for (new_state_coord, new_cell) in new_state.indexed_iter_mut() {
            let new_state_coord = new_state_coord.into_dimension();
            let mut original_coord: [Ixs; D] = [0; D];
            for i in 0..D {
                original_coord[i] = new_state_coord[i] as Ixs - 1;
            }

            let neighbors = self.count_neighbors(original_coord);
            *new_cell = match self.get(original_coord) {
                Cell::Active => {
                    if neighbors == 2 || neighbors == 3 {
                        Cell::Active
                    } else {
                        Cell::Inactive
                    }
                }
                Cell::Inactive => {
                    if neighbors == 3 {
                        Cell::Active
                    } else {
                        Cell::Inactive
                    }
                }
            };
        }

        Self { grid: new_state }
    }

    pub fn count_active(&self) -> usize {
        self.grid
            .iter()
            .filter(|cell| **cell == Cell::Active)
            .count()
    }

    pub fn trim(&mut self) {
        let sliced = self.grid.slice_each_axis(|axis| {
            let low_slice = self.grid.slice_axis(axis.axis, Slice::from(..=0));
            let high_slice = self.grid.slice_axis(axis.axis, Slice::from(axis.len - 1..));

            let low_bound = if low_slice
                .iter()
                .filter(|cell| **cell == Cell::Active)
                .count()
                == 0
            {
                1
            } else {
                0
            };

            let high_bound = if high_slice
                .iter()
                .filter(|cell| **cell == Cell::Active)
                .count()
                == 0
            {
                axis.len - 2
            } else {
                axis.len - 1
            };

            Slice::from(low_bound..=high_bound)
        });
        // TODO Mutate in place instead of copying ths slice
        self.grid = sliced.into_owned();
    }
}

fn part1(input: &Input) {
    let mut state = ConwayGrid::<3>::from_slice(input);
    for _ in 1..=6 {
        state = state.run_cycle();
        state.trim();
    }

    println!("Part2: {}", state.count_active());
}

fn part2(input: &Input) {
    let mut state = ConwayGrid::<4>::from_slice(input);
    for _ in 1..=6 {
        state = state.run_cycle();
        state.trim();
    }

    println!("Part2: {}", state.count_active());
}

fn main() {
    let mut raw_input = String::new();
    stdin()
        .read_to_string(&mut raw_input)
        .expect("Failed to read input");
    let input = parse::full_input(&raw_input).expect("Failed to parse input");
    part1(&input);
    part2(&input);
}

mod parse {
    use super::*;

    use advent2020::grid;

    fn parse_cell(c: char) -> Option<Cell> {
        match c {
            '.' => Some(Cell::Inactive),
            '#' => Some(Cell::Active),
            _ => None,
        }
    }

    pub(crate) fn full_input(input: &str) -> Option<Input> {
        grid::parse(input, parse_cell)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    // TODO Implement tests for 4D
    mod count_neighbors_3d {
        use super::*;
        use ndarray::{arr3, Array3};

        #[test]
        fn singleton_has_no_neighbors() {
            let g = ConwayGrid::<3>::new(Array3::from_elem((1, 1, 1), Cell::Inactive));
            assert_eq!(g.count_neighbors([0, 0, 0]), 0);
        }

        #[test]
        fn counts_in_2_d_slice() {
            let g = ConwayGrid::<3>::new(arr3(&[[
                [Cell::Active, Cell::Inactive, Cell::Inactive],
                [Cell::Inactive, Cell::Active, Cell::Active],
                [Cell::Active, Cell::Inactive, Cell::Active],
            ]]));
            assert_eq!(g.count_neighbors([0, 1, 1]), 4);
        }

        #[test]
        fn counts_non_centered() {
            let g = ConwayGrid::<3>::new(arr3(&[
                [
                    [Cell::Active, Cell::Inactive, Cell::Inactive],
                    [Cell::Inactive, Cell::Active, Cell::Active],
                    [Cell::Active, Cell::Inactive, Cell::Active],
                ],
                [
                    [Cell::Inactive, Cell::Inactive, Cell::Inactive],
                    [Cell::Inactive, Cell::Active, Cell::Active],
                    [Cell::Active, Cell::Inactive, Cell::Active],
                ],
                [
                    [Cell::Active, Cell::Inactive, Cell::Inactive],
                    [Cell::Inactive, Cell::Active, Cell::Active],
                    [Cell::Active, Cell::Inactive, Cell::Active],
                ],
                [
                    [Cell::Active, Cell::Inactive, Cell::Inactive],
                    [Cell::Inactive, Cell::Active, Cell::Active],
                    [Cell::Active, Cell::Inactive, Cell::Active],
                ],
            ]));
            assert_eq!(g.count_neighbors([2, 1, 1]), 13);
        }

        #[test]
        fn counts_out_of_bounds() {
            let g = ConwayGrid::<3>::new(arr3(&[
                [
                    [Cell::Active, Cell::Inactive, Cell::Inactive],
                    [Cell::Inactive, Cell::Active, Cell::Active],
                    [Cell::Active, Cell::Inactive, Cell::Active],
                ],
                [
                    [Cell::Active, Cell::Inactive, Cell::Inactive],
                    [Cell::Active, Cell::Active, Cell::Inactive],
                    [Cell::Active, Cell::Inactive, Cell::Inactive],
                ],
                [
                    [Cell::Active, Cell::Inactive, Cell::Inactive],
                    [Cell::Inactive, Cell::Active, Cell::Active],
                    [Cell::Active, Cell::Inactive, Cell::Active],
                ],
                [
                    [Cell::Active, Cell::Inactive, Cell::Inactive],
                    [Cell::Inactive, Cell::Active, Cell::Active],
                    [Cell::Active, Cell::Inactive, Cell::Active],
                ],
            ]));
            // Top left
            assert_eq!(g.count_neighbors([-1, -1, -1]), 1);
            // Top right
            assert_eq!(g.count_neighbors([-1, -1, 3]), 0);
            // Bottom right
            assert_eq!(g.count_neighbors([4, 3, 3]), 1);
            // Bottom right 2x2x1
            assert_eq!(g.count_neighbors([4, 2, 2]), 3);
            // Bottom right 2x2x2
            assert_eq!(g.count_neighbors([3, 2, 2]), 5);
        }
    }

    mod count_active_3d {
        use super::*;
        use ndarray::{arr3, Array3};

        #[test]
        fn singleton_active() {
            let g = ConwayGrid::<3>::new(Array3::from_elem((1, 1, 1), Cell::Active));
            assert_eq!(g.count_active(), 1);
        }

        #[test]
        fn singleton_unactive() {
            let g = ConwayGrid::<3>::new(Array3::from_elem((1, 1, 1), Cell::Inactive));
            assert_eq!(g.count_active(), 0);
        }

        #[test]
        fn counts_in_2d_slice() {
            let g = ConwayGrid::<3>::new(arr3(&[[
                [Cell::Active, Cell::Inactive, Cell::Inactive],
                [Cell::Inactive, Cell::Active, Cell::Active],
                [Cell::Active, Cell::Inactive, Cell::Active],
            ]]));
            assert_eq!(g.count_active(), 5);
        }

        #[test]
        fn counts_in_3d() {
            let g = ConwayGrid::<3>::new(arr3(&[
                [
                    [Cell::Active, Cell::Inactive, Cell::Inactive],
                    [Cell::Inactive, Cell::Active, Cell::Active],
                    [Cell::Active, Cell::Inactive, Cell::Active],
                ],
                [
                    [Cell::Inactive, Cell::Inactive, Cell::Inactive],
                    [Cell::Inactive, Cell::Active, Cell::Active],
                    [Cell::Active, Cell::Inactive, Cell::Active],
                ],
                [
                    [Cell::Active, Cell::Inactive, Cell::Inactive],
                    [Cell::Inactive, Cell::Active, Cell::Active],
                    [Cell::Active, Cell::Inactive, Cell::Active],
                ],
                [
                    [Cell::Active, Cell::Inactive, Cell::Inactive],
                    [Cell::Inactive, Cell::Active, Cell::Active],
                    [Cell::Active, Cell::Inactive, Cell::Active],
                ],
            ]));
            assert_eq!(g.count_active(), 19);
        }
    }
}
