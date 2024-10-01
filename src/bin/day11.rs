use std::io::{stdin, Read};

use advent2020::{
    coord::{self, Coord, Direction, UCoord},
    grid,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Seat {
    Floor,
    Empty,
    Full,
}

type Layout = grid::Grid<Seat>;

fn run_round(layout: &Layout) -> Option<Layout> {
    let mut new_layout = layout.clone();
    let mut did_change = false;
    for (coord, &seat) in layout.indexed_iter() {
        if seat == Seat::Floor {
            continue;
        }

        let coord = Coord::from(coord);
        let adj = coord.iter_adj8_bounded(layout.dim());
        let adj_occupied = adj
            .filter(|&adj_coord| layout[adj_coord] == Seat::Full)
            .count();

        if seat == Seat::Empty && adj_occupied == 0 {
            new_layout[coord] = Seat::Full;
            did_change = true;
        } else if seat == Seat::Full && adj_occupied >= 4 {
            new_layout[coord] = Seat::Empty;
            did_change = true;
        }
    }

    if did_change {
        Some(new_layout)
    } else {
        None
    }
}

fn run_until_stopped(layout: &Layout, run_single: impl Fn(&Layout) -> Option<Layout>) -> Layout {
    let mut layout = layout.clone();
    loop {
        if let Some(next) = run_single(&layout) {
            layout = next;
        } else {
            return layout;
        }
    }
}

fn occupied_in_dir(layout: &Layout, coord: UCoord, dir: Direction) -> bool {
    let first_chair = coord
        .scan_dir(dir, layout.dim())
        .map(|c| layout[c])
        .find(|&seat| seat != Seat::Floor);
    first_chair == Some(Seat::Full)
}

fn run_round_scan(layout: &Layout) -> Option<Layout> {
    let mut new_layout = layout.clone();
    let mut did_change = false;
    for (coord, &seat) in layout.indexed_iter() {
        if seat == Seat::Floor {
            continue;
        }

        let coord = Coord::from(coord);
        let adj_occupied = coord::ALL_DIR8
            .iter()
            .filter(|&&dir| occupied_in_dir(layout, coord, dir))
            .count();

        if seat == Seat::Empty && adj_occupied == 0 {
            new_layout[coord] = Seat::Full;
            did_change = true;
        } else if seat == Seat::Full && adj_occupied >= 5 {
            new_layout[coord] = Seat::Empty;
            did_change = true;
        }
    }

    if did_change {
        Some(new_layout)
    } else {
        None
    }
}

fn part1(input: &Layout) {
    let final_state = run_until_stopped(input, run_round);
    let final_occupied = final_state.iter().filter(|&&s| s == Seat::Full).count();
    println!("Part1: {}", final_occupied);
}

fn part2(input: &Layout) {
    let final_state = run_until_stopped(input, run_round_scan);
    let final_occupied = final_state.iter().filter(|&&s| s == Seat::Full).count();
    println!("Part2: {}", final_occupied);
}

fn main() {
    let mut input_raw = String::new();
    stdin()
        .read_to_string(&mut input_raw)
        .expect("Failed to read input");
    let layout = parse::grid(&input_raw).expect("Failed to parse");
    part1(&layout);
    part2(&layout);
}

mod parse {
    use crate::*;

    fn cell(c: char) -> Option<Seat> {
        match c {
            '.' => Some(Seat::Floor),
            'L' => Some(Seat::Empty),
            '#' => Some(Seat::Full),
            _ => None,
        }
    }

    pub(crate) fn grid(input: &str) -> Option<Layout> {
        grid::parse(input, cell)
    }
}
