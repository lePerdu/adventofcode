#![feature(iterator_try_collect)]

use std::io::{stdin, Read};

use advent2020::coord::{Coord, ICoord};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CardDir {
    N = 0,
    E = 1,
    S = 2,
    W = 3,
}

impl CardDir {
    fn rotate(&self, dir: TurnDir, amount: TurnAmount) -> Self {
        let clockwise_count = match dir {
            TurnDir::L => 4 - amount as usize,
            TurnDir::R => amount as usize,
        };

        use CardDir::*;
        static CLOCKWISE_ORDER: [CardDir; 4] = [N, E, S, W];
        let new_index = (*self as usize + clockwise_count) % 4;
        CLOCKWISE_ORDER[new_index]
    }

    fn apply_to(&self, coord: &ICoord, dist: isize) -> ICoord {
        match self {
            CardDir::N => Coord::new(coord.row - dist, coord.col),
            CardDir::E => Coord::new(coord.row, coord.col + dist),
            CardDir::S => Coord::new(coord.row + dist, coord.col),
            CardDir::W => Coord::new(coord.row, coord.col - dist),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TurnAmount {
    Q90 = 1,
    Q180 = 2,
    Q270 = 3,
}

impl TurnAmount {
    fn to_right_turn(self, dir: TurnDir) -> Self {
        use TurnAmount::*;
        match dir {
            TurnDir::L => match self {
                Q90 => Q270,
                Q180 => Q180,
                Q270 => Q90,
            },
            TurnDir::R => self,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TurnDir {
    L,
    R,
}

type Amount = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Step {
    Absolute {
        direction: CardDir,
        amount: Amount,
    },
    Forward {
        amount: Amount,
    },
    Turn {
        direction: TurnDir,
        amount: TurnAmount,
    },
}

#[derive(Debug)]
struct StateOld {
    position: ICoord,
    direction: CardDir,
}

impl Default for StateOld {
    fn default() -> Self {
        StateOld {
            position: Coord::new(0, 0),
            direction: CardDir::E,
        }
    }
}

impl StateOld {
    fn apply_step(&self, step: &Step) -> Self {
        match step {
            Step::Absolute { direction, amount } => Self {
                direction: self.direction,
                position: direction.apply_to(&self.position, *amount as isize),
            },
            Step::Forward { amount } => Self {
                direction: self.direction,
                position: self.direction.apply_to(&self.position, *amount as isize),
            },
            Step::Turn { direction, amount } => Self {
                direction: self.direction.rotate(*direction, *amount),
                position: self.position,
            },
        }
    }
}

#[derive(Debug)]
struct StateNew {
    position: ICoord,
    waypoint: ICoord,
}

impl Default for StateNew {
    fn default() -> Self {
        StateNew {
            position: Coord::new(0, 0),
            waypoint: Coord::new(-1, 10),
        }
    }
}

fn rotate_coord(coord: ICoord, dir: TurnDir, amount: TurnAmount) -> ICoord {
    fn rotate_r90(coord: ICoord) -> ICoord {
        Coord::new(coord.col, -coord.row)
    }
    fn rotate_180(coord: ICoord) -> ICoord {
        Coord::new(-coord.row, -coord.col)
    }
    fn rotate_r270(coord: ICoord) -> ICoord {
        Coord::new(-coord.col, coord.row)
    }

    match amount.to_right_turn(dir) {
        TurnAmount::Q90 => rotate_r90(coord),
        TurnAmount::Q180 => rotate_180(coord),
        TurnAmount::Q270 => rotate_r270(coord),
    }
}

impl StateNew {
    fn apply_step(&self, step: &Step) -> Self {
        match step {
            Step::Absolute { direction, amount } => Self {
                position: self.position,
                waypoint: direction.apply_to(&self.waypoint, *amount as isize),
            },
            Step::Forward { amount } => Self {
                position: self.position + self.waypoint * (*amount as isize),
                waypoint: self.waypoint,
            },
            Step::Turn { direction, amount } => Self {
                position: self.position,
                waypoint: rotate_coord(self.waypoint, *direction, *amount),
            },
        }
    }
}

fn part1(input: &[Step]) {
    let final_state = input
        .iter()
        .fold(StateOld::default(), |state, step| state.apply_step(step));
    let dist_to_origin = final_state.position.manhattan(&Coord::default());
    println!("Part1: {} ({:?})", dist_to_origin, final_state);
}

fn part2(input: &[Step]) {
    let final_state = input
        .iter()
        .fold(StateNew::default(), |state, step| state.apply_step(step));
    let dist_to_origin = final_state.position.manhattan(&Coord::default());
    println!("Part1: {} ({:?})", dist_to_origin, final_state);
}

fn main() {
    let mut raw_input = String::new();
    stdin()
        .read_to_string(&mut raw_input)
        .expect("Failed to read input");
    let input = parse::input(&raw_input).expect("Failed to parse input");
    part1(&input);
    part2(&input);
}

mod parse {
    use crate::*;

    fn ensure_turn_amount(raw_amount: u32) -> Option<TurnAmount> {
        match raw_amount {
            90 => Some(TurnAmount::Q90),
            180 => Some(TurnAmount::Q180),
            270 => Some(TurnAmount::Q270),
            _ => None,
        }
    }

    fn step(line: &str) -> Option<Step> {
        let (move_str, amount_str) = line.split_at(1);
        let move_char = if move_str.len() == 1 {
            move_str.chars().next()
        } else {
            None
        }?;
        let amount = amount_str.parse().ok()?;

        match move_char {
            'N' => Some(Step::Absolute {
                direction: CardDir::N,
                amount,
            }),
            'S' => Some(Step::Absolute {
                direction: CardDir::S,
                amount,
            }),
            'E' => Some(Step::Absolute {
                direction: CardDir::E,
                amount,
            }),
            'W' => Some(Step::Absolute {
                direction: CardDir::W,
                amount,
            }),
            'L' => Some(Step::Turn {
                direction: TurnDir::L,
                amount: ensure_turn_amount(amount)?,
            }),
            'R' => Some(Step::Turn {
                direction: TurnDir::R,
                amount: ensure_turn_amount(amount)?,
            }),
            'F' => Some(Step::Forward { amount }),
            _ => None,
        }
    }

    pub(crate) fn input(input: &str) -> Option<Box<[Step]>> {
        input.lines().map(step).try_collect()
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn parse_forward() {
            assert_eq!(step("F10"), Some(Step::Forward { amount: 10 }));
        }

        #[test]
        fn parse_absolute() {
            assert_eq!(
                step("W5"),
                Some(Step::Absolute {
                    direction: CardDir::W,
                    amount: 5
                })
            );
        }

        #[test]
        fn parse_turn() {
            assert_eq!(
                step("R270"),
                Some(Step::Turn {
                    direction: TurnDir::R,
                    amount: TurnAmount::Q270
                })
            );
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn card_dir_rotate_r180() {
        assert_eq!(CardDir::N.rotate(TurnDir::R, TurnAmount::Q180), CardDir::S);
    }
    #[test]
    fn card_dir_rotate_l180() {
        assert_eq!(CardDir::E.rotate(TurnDir::L, TurnAmount::Q180), CardDir::W);
    }
    #[test]
    fn card_dir_rotate_l90() {
        assert_eq!(CardDir::W.rotate(TurnDir::L, TurnAmount::Q90), CardDir::S);
    }
    #[test]
    fn card_dir_rotate_r270() {
        assert_eq!(CardDir::S.rotate(TurnDir::R, TurnAmount::Q270), CardDir::E);
    }
}
