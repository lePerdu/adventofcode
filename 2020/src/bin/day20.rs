#![feature(drain_filter)]

use advent2020::{
    coord::{Direction, UCoord, ALL_DIR4},
    grid,
};
use ndarray::{iter::IndexedIter, s, Array1, Array2, ArrayBase, ArrayView2, Data, Dim, Ix2};
use std::{
    io::{stdin, Read},
    iter::FilterMap,
};

type Pixel = bool;
type TileId = u32;
type TileData = Array2<Pixel>;
type TileDataView<'a> = ArrayView2<'a, Pixel>;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Tile {
    id: TileId,
    data: TileData,
}

#[derive(Debug, Clone)]
struct TileView<'t> {
    id: TileId,
    data: TileDataView<'t>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Rot {
    R0,
    R90,
    R180,
    R270,
    T,
    T1,
    Fx,
    Fy,
}

static ALL_ROT: [Rot; 8] = [
    Rot::R0,
    Rot::R90,
    Rot::R180,
    Rot::R270,
    Rot::T,
    Rot::T1,
    Rot::Fx,
    Rot::Fy,
];

fn rotate_tile<S, T>(tile: &ArrayBase<S, Ix2>, rot: Rot) -> ArrayView2<'_, T>
where
    S: Data<Elem = T>,
{
    match rot {
        Rot::R0 => tile.view(),
        Rot::R90 => tile.t().slice_move(s![..;-1, ..]),
        Rot::R180 => tile.slice(s![..;-1, ..;-1]),
        Rot::R270 => tile.t().slice_move(s![.., ..;-1]),
        Rot::T => tile.t(),
        Rot::T1 => tile.t().slice_move(s![..;-1, ..;-1]),
        Rot::Fx => tile.slice(s![..;-1, ..]),
        Rot::Fy => tile.slice(s![.., ..;-1]),
    }
}

fn tile_matches_in_dir(a: &TileDataView, dir: Direction, b: &TileDataView) -> bool {
    match dir {
        Direction::R => a.slice(s![.., -1]) == b.slice(s![.., 0]),
        Direction::U => a.slice(s![0, ..]) == b.slice(s![-1, ..]),
        Direction::L => a.slice(s![.., 0]) == b.slice(s![.., -1]),
        Direction::D => a.slice(s![-1, ..]) == b.slice(s![0, ..]),
        _ => false,
    }
}

fn find_matching_rotation<'a>(
    tile_to_rotate: &'a TileDataView<'a>,
    dir: Direction,
    fixed_tile: &'a TileDataView<'a>,
) -> Option<Rot> {
    let res = ALL_ROT
        .iter()
        .copied()
        .find(|rot| tile_matches_in_dir(&rotate_tile(tile_to_rotate, *rot), dir, fixed_tile));
    res
}

struct Puzzle<'t> {
    known_grid: Array2<Option<TileView<'t>>>,
}

impl<'t> Puzzle<'t> {
    fn from_starting_piece(tile: &'t Tile) -> Self {
        let mut known_grid = Array2::from_elem((3, 3), None);
        known_grid[(1, 1)] = Some(TileView {
            id: tile.id,
            data: tile.data.view(),
        });
        Self { known_grid }
    }

    fn has_adjacent_tile(&self, coord: UCoord) -> bool {
        coord
            .iter_adj4_bounded(self.known_grid.dim())
            .any(|c| self.known_grid[c].is_some())
    }

    /// Iterate over candidate spaces for adding new tiles.
    /// Each space must be:
    /// - Unoccupied
    /// - Adjacent to an existing tile
    ///
    /// For some reason, this needs a very specific type annotation to compile
    fn iter_open_spaces<'a>(
        &'a self,
    ) -> FilterMap<
        IndexedIter<'a, Option<TileView<'t>>, Dim<[usize; 2]>>,
        impl FnMut(((usize, usize), &'a Option<TileView<'t>>)) -> Option<UCoord>,
    > {
        self.known_grid.indexed_iter().filter_map(|(coord, tile)| {
            let coord = coord.clone().into();
            if tile.is_none() && self.has_adjacent_tile(coord) {
                Some(coord)
            } else {
                None
            }
        })
    }

    /// Attempt to insert a new tile into the puzzle
    fn find_insertion(&self, tile: &Tile) -> Option<(UCoord, Rot)> {
        let coord_bound = self.known_grid.dim();
        self.iter_open_spaces().find_map(|space| {
            ALL_DIR4
                .iter()
                .copied()
                // Find existing adjacent tiles
                .filter_map(|dir| {
                    let adj_coord = space.move_dir(dir, coord_bound)?;
                    let adj_tile = self.known_grid[adj_coord].as_ref()?;
                    Some((dir, adj_tile))
                })
                // Try to match against each individually
                .map(|(dir, adj_tile)| {
                    find_matching_rotation(&tile.data.view(), dir, &adj_tile.data.view())
                })
                // Make sure all rotations are the same
                .reduce(|acc, opt_rotation| if acc == opt_rotation { acc } else { None })
                // join options together
                .and_then(|x| x)
                .map(|rot| (space, rot))
        })
    }

    fn expand_if_at_edge(&mut self, coord: UCoord) {
        let (row_max, col_max) = self.known_grid.dim();
        if coord.row == 0 {
            let mut new_grid = Array2::from_elem((row_max + 1, col_max), None);
            new_grid.slice_mut(s![1.., ..]).assign(&self.known_grid);
            self.known_grid = new_grid;
        } else if coord.row == row_max - 1 {
            self.known_grid
                .push_row(Array1::from_elem(col_max, None).view())
                .unwrap();
        }

        // Re-compute in case they changed
        let (row_max, col_max) = self.known_grid.dim();
        if coord.col == 0 {
            let mut new_grid = Array2::from_elem((row_max, col_max + 1), None);
            new_grid.slice_mut(s![.., 1..]).assign(&self.known_grid);
            self.known_grid = new_grid;
        } else if coord.col == col_max - 1 {
            self.known_grid
                .push_column(Array1::from_elem(row_max, None).view())
                .unwrap();
        }
    }

    fn try_insert(&mut self, tile: &'t Tile) -> bool {
        if let Some((coord, rotation)) = self.find_insertion(tile) {
            self.known_grid[coord] = Some(TileView {
                id: tile.id,
                data: rotate_tile(&tile.data, rotation),
            });
            self.expand_if_at_edge(coord);
            return true;
        } else {
            return false;
        }
    }

    /// Convert into a grid of `Tile`s if the puzzle is "solved" (i.e. the puzzle is a complete
    /// rectangle)
    fn try_to_solved_grid(&self) -> Option<Array2<TileView<'t>>> {
        let inner_grid = self.known_grid.slice(s![1..-1, 1..-1]);
        if inner_grid.iter().all(|opt| opt.is_some()) {
            Some(inner_grid.map(|opt_tile| opt_tile.as_ref().unwrap().clone()))
        } else {
            None
        }
    }

    /// Convert the tile grid into a single tile representing the whole image
    /// - Remove borders of all tiles
    /// - Combine tiles into a master grid
    fn try_to_final_grid(&self) -> Option<TileData> {
        let inner_grid = self.known_grid.slice(s![1..-1, 1..-1]);
        if inner_grid.iter().all(|opt| opt.is_some()) {
            let example_tile = inner_grid[(0, 0)].as_ref().unwrap();
            let (tile_rows, tile_cols) = example_tile.data.dim();
            let (new_tile_rows, new_tile_cols) = (tile_rows - 2, tile_cols - 2);
            let (rows, cols) = inner_grid.dim();
            let mut final_grid =
                Array2::from_elem((rows * new_tile_rows, cols * new_tile_cols), false);
            for ((row, col), tile) in inner_grid.indexed_iter() {
                let reduced_tile = tile.as_ref().unwrap().data.slice(s![1..-1, 1..-1]);
                final_grid
                    .slice_mut(s![
                        row * new_tile_rows..(row + 1) * new_tile_rows,
                        col * new_tile_cols..(col + 1) * new_tile_cols,
                    ])
                    .assign(&reduced_tile);
            }

            Some(final_grid)
        } else {
            None
        }
    }
}

type Input = Vec<Tile>;

fn part1<'t>(input: &'t Input) -> Puzzle<'t> {
    let mut all_tiles: Vec<&Tile> = input.iter().collect();
    let first_tile = all_tiles.pop().expect("Must be at least 1 tile");
    let mut puzzle = Puzzle::from_starting_piece(first_tile);

    while !all_tiles.is_empty() {
        let inserted = all_tiles
            .drain_filter(|new_tile| puzzle.try_insert(new_tile))
            .count();
        if inserted == 0 {
            panic!("Could not place tiles: {:?}", all_tiles);
        } else {
            println!("Inserted {}", inserted);
        }
    }

    let solved_grid = puzzle
        .try_to_solved_grid()
        .expect("Puzzle should be solved after all tiles are placed");
    println!("Solved puzzle!");
    let (n_rows, n_cols) = solved_grid.dim();
    let corner_prod = solved_grid[(0, 0)].id as usize
        * solved_grid[(0, n_cols - 1)].id as usize
        * solved_grid[(n_rows - 1, 0)].id as usize
        * solved_grid[(n_rows - 1, n_cols - 1)].id as usize;
    println!("Part1: {}", corner_prod);

    return puzzle;
}

fn count_matches(main_image: &ArrayView2<bool>, search_image: &ArrayView2<bool>) -> usize {
    main_image
        .windows(search_image.dim())
        .into_iter()
        .filter(|win| (win & search_image) == search_image)
        .count()
}

fn count_pixels(image: &Array2<bool>) -> usize {
    image.iter().filter(|p| **p).count()
}

fn part2(input: &Puzzle) {
    let final_image = input
        .try_to_final_grid()
        .expect("Puzzle should already be solved");
    let final_image_pixels = count_pixels(&final_image);

    let monster_image = grid::parse(
        "..................#.
#....##....##....###
.#..#..#..#..#..#...",
        parse::parse_pixel,
    )
    .expect("Failed to parse monster image");
    let monster_pixels = count_pixels(&monster_image);

    let matches = ALL_ROT
        .iter()
        .filter_map(|&rot| {
            let matches = count_matches(&rotate_tile(&final_image, rot), &monster_image.view());
            if matches > 0 {
                Some(matches)
            } else {
                None
            }
        })
        .max()
        .expect("No matches found");

    let ans = final_image_pixels - monster_pixels * matches;
    println!("Part2: {}", ans);
}

fn main() {
    let mut raw_input = String::new();
    stdin()
        .read_to_string(&mut raw_input)
        .expect("Failed to read input");
    let (_, input) = parse::full_input(&raw_input).expect("Failed to parse input");
    let solved_puzzle = part1(&input);
    part2(&solved_puzzle);
}

mod parse {
    use super::*;
    use advent2020::grid;
    use nom::{
        bytes::complete::{tag, take_until},
        character::complete::{multispace1, newline, u32},
        combinator::{all_consuming, map_opt},
        multi::fold_many1,
        sequence::{delimited, separated_pair, terminated},
        Parser,
    };

    type IResult<'a, O> = nom::IResult<&'a str, O>;

    pub(crate) fn parse_pixel(c: char) -> Option<Pixel> {
        match c {
            '#' => Some(true),
            '.' => Some(false),
            _ => None,
        }
    }

    fn tile(input: &str) -> IResult<Tile> {
        let tile_id = delimited(tag("Tile "), u32, tag(":"));
        let tile_contents = map_opt(take_until("\n\n"), |content| {
            grid::parse(content, parse_pixel)
        });

        separated_pair(tile_id, newline, tile_contents)
            .map(|(id, data)| Tile { id, data })
            .parse(input)
    }

    pub(crate) fn full_input(input: &str) -> IResult<Input> {
        all_consuming(fold_many1(
            terminated(tile, multispace1),
            Vec::new,
            |mut acc, t| {
                acc.push(t);
                acc
            },
        ))
        .parse(input)
    }

    #[cfg(test)]
    mod test {
        use super::*;
        use ndarray::array;

        #[test]
        fn parses_tile() {
            assert_eq!(
                tile("Tile 123:\n.#\n##\n\n"),
                Ok((
                    "\n\n",
                    Tile {
                        id: 123,
                        data: array![[false, true], [true, true]],
                    }
                ))
            );
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use ndarray::{array, aview2};

    #[test]
    fn rotate_0() {
        let tile = aview2(&[[1, 2], [3, 4]]);
        assert_eq!(rotate_tile(&tile, Rot::R0), tile);
    }

    #[test]
    fn rotate_90() {
        let tile = aview2(&[[1, 2], [3, 4]]);
        assert_eq!(rotate_tile(&tile, Rot::R90), aview2(&[[2, 4], [1, 3]]));
    }

    #[test]
    fn rotate_180() {
        let tile = aview2(&[[1, 2], [3, 4]]);
        assert_eq!(rotate_tile(&tile, Rot::R180), aview2(&[[4, 3], [2, 1]]));
    }

    #[test]
    fn rotate_270() {
        let tile = aview2(&[[1, 2], [3, 4]]);
        assert_eq!(rotate_tile(&tile, Rot::R270), aview2(&[[3, 1], [4, 2]]));
    }

    #[test]
    fn finds_match_on_right() {
        let tile_a = array![
            [true, true, true],
            [false, false, false],
            [false, true, false],
        ];
        let tile_b = array![
            [true, true, false],
            [false, false, false],
            [false, true, false],
        ];

        assert!(tile_matches_in_dir(
            &tile_a.view(),
            Direction::R,
            &tile_b.view()
        ));
    }

    #[test]
    fn finds_no_match() {
        let tile_a = array![
            [true, true, true],
            [false, false, false],
            [false, true, false],
        ];
        let tile_b = array![
            [true, true, false],
            [true, false, false],
            [false, true, false],
        ];

        assert!(!tile_matches_in_dir(
            &tile_a.view(),
            Direction::R,
            &tile_b.view()
        ));
    }

    #[test]
    fn finds_match_after_rotating() {
        let tile_rot = array![
            [true, true, false],
            [true, false, false],
            [false, true, false],
        ];
        let tile_fixed = array![
            [true, true, true],
            [false, false, false],
            [false, true, false],
        ];

        assert_eq!(
            find_matching_rotation(&tile_rot.view(), Direction::U, &tile_fixed.view()),
            Some(Rot::R180)
        );
    }

    #[test]
    fn puzzle_open_spaces_from_initial() {
        let tile = Tile {
            id: 1,
            data: array![[true, true], [false, false]],
        };
        let p = Puzzle::from_starting_piece(&tile);
        let n_spaces = p.iter_open_spaces().count();
        assert_eq!(n_spaces, 4);
    }

    #[test]
    fn puzzle_with_space_in_middle() {
        let tile_data = [
            array![[false, true], [true, false]],
            array![[true, true], [false, false]],
            array![[false, false], [false, true]],
        ];
        let p = Puzzle {
            known_grid: array![
                [None, None, None, None],
                [
                    None,
                    Some(TileView {
                        id: 1,
                        data: tile_data[0].view(),
                    }),
                    Some(TileView {
                        id: 2,
                        data: tile_data[1].view(),
                    }),
                    None
                ],
                [
                    None,
                    None,
                    Some(TileView {
                        id: 3,
                        data: tile_data[2].view(),
                    }),
                    None
                ],
                [None, None, None, None]
            ],
        };
        let n_spaces = p.iter_open_spaces().count();
        assert_eq!(n_spaces, 7);
    }

    #[test]
    fn puzzle_find_insertion_point() {
        let tile = Tile {
            id: 1,
            data: array![[true, true], [false, false]],
        };
        let p = Puzzle::from_starting_piece(&tile);
        let new_tile = Tile {
            id: 2,
            data: array![[true, true], [true, false]],
        };
        let insertion = p.find_insertion(&new_tile);
        assert_eq!(insertion, Some((UCoord::new(0, 1), Rot::R90)));
    }
}
