use std::{
    collections::HashSet,
    io::{stdin, Read},
    ops::{Add, Sub},
};

use advent2020::grid::Grid;
use ndarray::Array2;

/// Basis vector for hexagonal grid. This contains the E and SE vectors.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct HexCoord {
    e: i32,
    se: i32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Move {
    E,
    SE,
    SW,
    W,
    NW,
    NE,
}

const ALL_MOVES: [Move; 6] = {
    use Move::*;
    [E, SE, SW, W, NW, NE]
};

impl From<Move> for HexCoord {
    fn from(m: Move) -> Self {
        use Move::*;
        match m {
            E => HexCoord { e: 1, se: 0 },
            SE => HexCoord { e: 0, se: 1 },
            SW => HexCoord { e: -1, se: 1 },
            W => HexCoord { e: -1, se: 0 },
            NW => HexCoord { e: 0, se: -1 },
            NE => HexCoord { e: 1, se: -1 },
        }
    }
}

impl Add for HexCoord {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        HexCoord {
            e: self.e + rhs.e,
            se: self.se + rhs.se,
        }
    }
}

impl Sub for HexCoord {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        HexCoord {
            e: self.e - rhs.e,
            se: self.se - rhs.se,
        }
    }
}

impl FromIterator<Move> for HexCoord {
    fn from_iter<T: IntoIterator<Item = Move>>(iter: T) -> Self {
        iter.into_iter()
            .fold(HexCoord::default(), |acc, c| acc + c.into())
    }
}

impl HexCoord {
    fn iter_adj<'a>(&'a self) -> impl Iterator<Item = Self> + 'a {
        ALL_MOVES.iter().copied().map(|m| *self + m.into())
    }
}

type Input = Vec<Vec<Move>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Color {
    White,
    Black,
}

impl Color {
    fn flip(&self) -> Self {
        use Color::*;
        match self {
            White => Black,
            Black => White,
        }
    }
}

fn compute_flipped_coords(input: &[Vec<Move>]) -> HashSet<HexCoord> {
    let mut flipped = HashSet::<HexCoord>::new();
    for seq in input {
        let final_coord = seq.iter().copied().collect();
        // Toggle membership (doesn't seem to be a single operation for this.
        // Maybe a map of bool is better.
        if !flipped.insert(final_coord) {
            flipped.remove(&final_coord);
        }
    }

    flipped
}

#[derive(Debug)]
struct TileGrid {
    ref_tile_offset: HexCoord,
    tiles: Grid<Color>,
}

impl TileGrid {
    fn from_black_coords(black_coords: &HashSet<HexCoord>) -> Self {
        assert!(!black_coords.is_empty());
        let (min, max) = black_coords.iter().fold(
            (
                HexCoord {
                    e: i32::MAX,
                    se: i32::MAX,
                },
                HexCoord {
                    e: i32::MIN,
                    se: i32::MIN,
                },
            ),
            |(min, max), new_coord| {
                let min = HexCoord {
                    e: min.e.min(new_coord.e),
                    se: min.se.min(new_coord.se),
                };
                let max = HexCoord {
                    e: max.e.max(new_coord.e),
                    se: max.se.max(new_coord.se),
                };
                (min, max)
            },
        );

        let grid_size = ((max.e - min.e + 1) as usize, (max.se - min.se + 1) as usize);
        let ref_tile_offset = HexCoord {
            e: min.e,
            se: min.se,
        };

        let tiles = Array2::from_shape_fn(grid_size, |(e, se)| {
            let shifted_coord = HexCoord {
                e: e as i32,
                se: se as i32,
            } + ref_tile_offset;
            if black_coords.contains(&shifted_coord) {
                Color::Black
            } else {
                Color::White
            }
        });

        Self {
            ref_tile_offset,
            tiles,
        }
    }

    fn get(&self, coord: &HexCoord) -> Color {
        let (e_max, se_max) = self.tiles.dim();
        let adjusted = *coord - self.ref_tile_offset;
        if (0 <= adjusted.e && adjusted.e < e_max as i32)
            && (0 <= adjusted.se && adjusted.se < se_max as i32)
        {
            self.tiles[(adjusted.e as usize, adjusted.se as usize)]
        } else {
            Color::White
        }
    }

    fn count_black(&self) -> u32 {
        self.tiles
            .iter()
            .filter(|&&cell| cell == Color::Black)
            .count() as u32
    }

    fn count_black_neighbors(&self, coord: &HexCoord) -> u8 {
        coord
            .iter_adj()
            .filter(|c| self.get(c) == Color::Black)
            .count() as u8
    }

    fn advance(&self) -> Self {
        let old_dim = self.tiles.dim();
        let new_dim = (old_dim.0 + 2, old_dim.1 + 2);
        let new_tile_offset = self.ref_tile_offset - HexCoord { e: 1, se: 1 };
        let mut new_tiles = Array2::from_elem(new_dim, Color::White);

        for ((e_val, se_val), cell) in new_tiles.indexed_iter_mut() {
            let global_coord = HexCoord {
                e: e_val as i32,
                se: se_val as i32,
            } + new_tile_offset;

            let neighbors = self.count_black_neighbors(&global_coord);
            use Color::*;
            *cell = match self.get(&global_coord) {
                Black => {
                    if neighbors == 0 || neighbors > 2 {
                        White
                    } else {
                        Black
                    }
                }
                White => {
                    if neighbors == 2 {
                        Black
                    } else {
                        White
                    }
                }
            };
        }

        Self {
            ref_tile_offset: new_tile_offset,
            tiles: new_tiles,
        }
    }
}

// fn compute_initial_layout(input: &[Vec<Move>]) -> TileGrid {
//     let flipped = compute_flipped_coords(input);
// }

fn part1(input: &Input) {
    let ans = compute_flipped_coords(input).len();
    println!("Part1: {}", ans);
}

fn part2(input: &Input) {
    let mut tiles = TileGrid::from_black_coords(&compute_flipped_coords(input));
    println!("Day 0: {}", tiles.count_black());
    for i in 1..=100 {
        tiles = tiles.advance();
        println!("Day {}: {}", i, tiles.count_black());
    }

    let ans = tiles.count_black();
    println!("Part2: {}", ans);
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
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::{multispace0, newline},
        combinator::{complete, value},
        multi::{many1, separated_list1},
        sequence::terminated,
        Parser,
    };

    use super::*;

    type Err<'a> = nom::Err<nom::error::Error<&'a str>>;
    type IResult<'a, O> = nom::IResult<&'a str, O>;

    fn parse_move(input: &str) -> IResult<Move> {
        alt((
            value(Move::E, tag("e")),
            value(Move::SE, tag("se")),
            value(Move::SW, tag("sw")),
            value(Move::W, tag("w")),
            value(Move::NW, tag("nw")),
            value(Move::NE, tag("ne")),
        ))
        .parse(input)
    }

    pub(crate) fn full_input(input: &str) -> Result<Input, Err> {
        let move_seq = many1(parse_move);
        let all_seqs = separated_list1(newline, move_seq);
        complete(terminated(all_seqs, multispace0))
            .parse(input)
            .map(|(_rest, value)| value)
    }

    #[cfg(test)]
    mod test {
        use super::*;

        use Move::*;

        #[test]
        fn parses_single_line() {
            assert_eq!(full_input("eswe"), Ok(vec![vec![E, SW, E]]));
        }

        #[test]
        fn parses_multiple_line() {
            assert_eq!(
                full_input("eswe\nnwwswee"),
                Ok(vec![vec![E, SW, E], vec![NW, W, SW, E, E]])
            );
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn iter_adj_inculdes_all() {
        let mut coords: Vec<HexCoord> = HexCoord::default().iter_adj().collect();
        let mut expected = vec![
            HexCoord { e: -1, se: 0 },
            HexCoord { e: -1, se: 1 },
            HexCoord { e: 0, se: -1 },
            HexCoord { e: 0, se: 1 },
            HexCoord { e: 1, se: 0 },
            HexCoord { e: 1, se: -1 },
        ];
        coords.sort();
        expected.sort();
        assert_eq!(coords, expected,);
    }

    #[test]
    fn creates_grid_of_size_1_with_single_coord() {
        let flipped = HashSet::from([HexCoord { e: 1, se: 2 }]);
        let tile_grid = TileGrid::from_black_coords(&flipped);
        assert_eq!(tile_grid.tiles.dim(), (1, 1));
        assert_eq!(tile_grid.ref_tile_offset, HexCoord { e: 1, se: 2 });
    }

    #[test]
    fn creates_grid_with_single_coord_black() {
        let flipped = HashSet::from([HexCoord { e: 1, se: 2 }]);
        let tile_grid = TileGrid::from_black_coords(&flipped);
        assert_eq!(tile_grid.get(&HexCoord { e: 1, se: 2 }), Color::Black);
        // Other coordinates should be white
        assert_eq!(tile_grid.get(&HexCoord { e: 1, se: 1 }), Color::White);
        assert_eq!(tile_grid.get(&HexCoord { e: -1, se: 1 }), Color::White);
    }

    #[test]
    fn count_0_neighbors() {
        let flipped = HashSet::from([HexCoord { e: 1, se: 2 }]);
        let tile_grid = TileGrid::from_black_coords(&flipped);
        assert_eq!(
            tile_grid.count_black_neighbors(&HexCoord { e: 0, se: 0 }),
            0
        );
    }

    #[test]
    fn count_e_neighbor() {
        let flipped = HashSet::from([HexCoord { e: 0, se: 0 }, HexCoord { e: 1, se: 0 }]);
        let tile_grid = TileGrid::from_black_coords(&flipped);
        assert_eq!(
            tile_grid.count_black_neighbors(&HexCoord { e: 0, se: 0 }),
            1
        );
    }

    #[test]
    fn count_se_neighbor() {
        let flipped = HashSet::from([HexCoord { e: 0, se: 0 }, HexCoord { e: 0, se: 1 }]);
        let tile_grid = TileGrid::from_black_coords(&flipped);
        assert_eq!(
            tile_grid.count_black_neighbors(&HexCoord { e: 0, se: 0 }),
            1
        );
    }

    #[test]
    fn count_sw_neighbor() {
        let flipped = HashSet::from([HexCoord { e: 0, se: 0 }, HexCoord { e: -1, se: 1 }]);
        let tile_grid = TileGrid::from_black_coords(&flipped);
        assert_eq!(
            tile_grid.count_black_neighbors(&HexCoord { e: 0, se: 0 }),
            1
        );
    }

    #[test]
    fn count_w_neighbor() {
        let flipped = HashSet::from([HexCoord { e: 0, se: 0 }, HexCoord { e: -1, se: 0 }]);
        let tile_grid = TileGrid::from_black_coords(&flipped);
        assert_eq!(
            tile_grid.count_black_neighbors(&HexCoord { e: 0, se: 0 }),
            1
        );
    }

    #[test]
    fn count_nw_neighbor() {
        let flipped = HashSet::from([HexCoord { e: 0, se: 0 }, HexCoord { e: 0, se: -1 }]);
        let tile_grid = TileGrid::from_black_coords(&flipped);
        assert_eq!(
            tile_grid.count_black_neighbors(&HexCoord { e: 0, se: 0 }),
            1
        );
    }

    #[test]
    fn count_ne_neighbor() {
        let flipped = HashSet::from([HexCoord { e: 0, se: 0 }, HexCoord { e: 1, se: -1 }]);
        let tile_grid = TileGrid::from_black_coords(&flipped);
        assert_eq!(
            tile_grid.count_black_neighbors(&HexCoord { e: 0, se: 0 }),
            1
        );
    }

    #[test]
    fn count_6_neighbors_when_surrounded() {
        let flipped = HashSet::from([
            HexCoord { e: -1, se: -1 },
            HexCoord { e: -1, se: 0 },
            HexCoord { e: -1, se: 1 },
            HexCoord { e: 0, se: -1 },
            HexCoord { e: 0, se: 0 },
            HexCoord { e: 0, se: 1 },
            HexCoord { e: 1, se: -1 },
            HexCoord { e: 1, se: 0 },
            HexCoord { e: 1, se: 1 },
        ]);
        let tile_grid = TileGrid::from_black_coords(&flipped);
        assert_eq!(tile_grid.tiles.dim(), (3, 3));
        assert_eq!(
            tile_grid.count_black_neighbors(&HexCoord { e: 0, se: 0 }),
            6
        );
    }
}
