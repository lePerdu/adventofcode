#![feature(iterator_try_collect)]

use std::io::stdin;

use ndarray::{Array2, Axis, Slice};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FB {
    Front,
    Back,
}

impl Default for FB {
    fn default() -> Self {
        FB::Back
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RL {
    Left,
    Right,
}

impl Default for RL {
    fn default() -> Self {
        RL::Left
    }
}

#[derive(Debug, Clone)]
struct SeatPath {
    row_path: [FB; 7],
    col_path: [RL; 3],
}

fn coord_to_id((row, col): (usize, usize)) -> usize {
    row * 8 + col
}

impl SeatPath {
    fn seat_coord(&self) -> (u8, u8) {
        let row = self.row_path.iter().fold(0, |acc, fb| 2 * acc + *fb as u8);
        let col = self.col_path.iter().fold(0, |acc, rl| 2 * acc + *rl as u8);
        (row, col)
    }

    fn seat_id(&self) -> usize {
        let (r, c) = self.seat_coord();
        coord_to_id((r as usize, c as usize))
    }
}

fn part1(input: &[SeatPath]) {
    let ans = input.iter().map(|s| s.seat_id()).max().expect("No seats");
    println!("Part1: {}", ans);
}

fn part2(input: &[SeatPath]) {
    let mut taken_seats = Array2::from_elem((128, 8), false);
    for s in input {
        let (row, col) = s.seat_coord();
        taken_seats[(row as usize, col as usize)] = true;
    }

    let mut non_empty_rows = taken_seats
        .rows()
        .into_iter()
        .enumerate()
        .filter(|(_, row)| row.iter().any(|&filled| filled))
        .map(|(row_idx, _)| row_idx);

    let first_row = non_empty_rows.next().expect("No first row") + 1;
    let last_row = non_empty_rows.last().expect("No last row") - 1;

    let open_seat = taken_seats
        .slice_axis(Axis(0), Slice::from(first_row..=last_row))
        .indexed_iter()
        .filter(|(_, &taken)| !taken)
        .map(|((row, col), _)| (row + first_row, col))
        .next()
        .expect("No open seat");

    println!("Part2: {}", coord_to_id(open_seat));
}

fn main() {
    let input = stdin()
        .lines()
        .map(|l| l.expect("Failed to read input"))
        .map(|l| parse::seat_path(&l))
        .try_collect::<Vec<_>>()
        .expect("Failed to parse");
    part1(&input);
    part2(&input);
}

mod parse {
    use crate::*;

    fn parse_fb(c: char) -> Option<FB> {
        match c {
            'B' => Some(FB::Back),
            'F' => Some(FB::Front),
            _ => None,
        }
    }

    fn parse_rl(c: char) -> Option<RL> {
        match c {
            'L' => Some(RL::Left),
            'R' => Some(RL::Right),
            _ => None,
        }
    }

    fn map_len<T: Default + Copy, F: Fn(char) -> Option<T>, const L: usize>(
        s: &str,
        f: F,
    ) -> Option<[T; L]> {
        let vec = s.chars().map(f).try_collect::<Vec<T>>()?;
        if vec.len() == L {
            let mut buf: [T; L] = [T::default(); L];
            buf.copy_from_slice(&vec);
            Some(buf)
        } else {
            None
        }
    }

    pub(crate) fn seat_path(s: &str) -> Option<SeatPath> {
        let (fb, rl) = s.split_at(7);
        Some(SeatPath {
            row_path: map_len(fb, parse_fb)?,
            col_path: map_len(rl, parse_rl)?,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn fb_to_bit() {
        assert_eq!(FB::Front as u8, 0);
        assert_eq!(FB::Back as u8, 1);
    }

    #[test]
    fn rl_to_bit() {
        assert_eq!(RL::Left as u8, 0);
        assert_eq!(RL::Right as u8, 1);
    }

    #[test]
    fn seat_path() {
        use FB::*;
        use RL::*;
        let sp = SeatPath {
            row_path: [Back, Back, Front, Front, Back, Back, Front],
            col_path: [Right, Left, Left],
        };
        assert_eq!(sp.seat_coord(), (102, 4));
        assert_eq!(sp.seat_id(), 820);
    }
}
