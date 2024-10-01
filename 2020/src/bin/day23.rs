#![feature(iterator_try_collect)]

use std::io::{stdin, Read};

type Cup = u32;

type Input = Vec<Cup>;

struct NextsList(Box<[Cup]>);

struct NextListIter<'a> {
    list: &'a NextsList,
    start: Cup,
    current: Option<Cup>,
}

impl NextsList {
    fn from_extended_list(input: &[Cup], max_value: Cup) -> Self {
        // Store 1 more element at the front as a sentinel for missing cup pointers
        let mut all_cups = vec![0; max_value as usize + 1];
        let first = input[0];
        let mut previous = first;

        let extended_iter = input[1..]
            .into_iter()
            .copied()
            .chain((input.len() + 1) as u32..=max_value);
        for next in extended_iter {
            all_cups[previous as usize] = next;
            previous = next;
        }

        all_cups[previous as usize] = first;

        Self(all_cups.into_boxed_slice())
    }

    fn max_cup(&self) -> Cup {
        (self.0.len() - 1) as Cup
    }

    fn get_next(&self, cup: Cup) -> Option<Cup> {
        if cup == 0 || cup as usize >= self.0.len() {
            None
        } else {
            Some(self.0[cup as usize])
        }
    }

    fn set_next(&mut self, prev: Cup, next: Cup) {
        // TODO Range checking
        self.0[prev as usize] = next;
    }

    fn iter_from_1<'a>(&'a self) -> NextListIter<'a> {
        NextListIter { list: self, start: 1, current: Some(1) }
    }
}

impl From<&[Cup]> for NextsList {
    fn from(slice: &[Cup]) -> Self {
        NextsList::from_extended_list(slice, slice.len() as Cup)
    }
}

impl<'a> Iterator for NextListIter<'a> {
    type Item = Cup;

    fn next(&mut self) -> Option<Self::Item> {
        let current_cup = self.current?;
        let next_cup = self.list.get_next(current_cup);
        self.current = if next_cup == Some(self.start) {
            None
        } else {
            next_cup
        };

        Some(current_cup)
    }
}

fn find_next_dest(current_cup: Cup, max_cup: Cup, excluded: &[Cup]) -> Cup {
    let mut candidate = current_cup;
    loop {
        candidate = if candidate == 1 { max_cup } else { candidate - 1 };
        if !excluded.contains(&candidate) {
            return candidate;
        }
    }
}

fn cycle(all_cups: &mut NextsList, current_cup: Cup) -> Option<Cup> {
    let first_removed = all_cups.get_next(current_cup)?;
    let mid_removed = all_cups.get_next(first_removed)?;
    let last_removed = all_cups.get_next(mid_removed)?;
    let after_removed = all_cups.get_next(last_removed)?;

    let destination_cup = find_next_dest(
        current_cup,
        all_cups.max_cup(),
        &[first_removed, mid_removed, last_removed],
    );
    let after_destination = all_cups.get_next(destination_cup)?;

    all_cups.set_next(current_cup, after_removed);
    all_cups.set_next(last_removed, after_destination);
    all_cups.set_next(destination_cup, first_removed);

    Some(after_removed)
}

fn run_game(all_cups: &mut NextsList, starting_cup: Cup, count: usize) {
    let mut current_cup = starting_cup;
    for _ in 1..=count {
        current_cup = cycle(all_cups, current_cup).expect("Failed to run round");
    }
}

fn part1(input: &[Cup]) {
    let mut cups= input.into();
    run_game(&mut cups, input[0], 100);
    let labels_after_1: String = cups.iter_from_1()
        .skip(1)
        .map(|cup| char::from_digit(cup as u32, 10))
        .try_collect()
        .expect("Digit out of range");
    println!("Part1: {}", labels_after_1);
}

const INPUT_SIZE: u32 = 1_000_000;

fn part2(input: &[Cup]) {
    let mut cups = NextsList::from_extended_list(input, INPUT_SIZE);
    run_game(&mut cups, input[0], 10 * INPUT_SIZE as usize);
    let labels_after_1: Vec<Cup> = cups.iter_from_1().collect();
    //println!("final cups: {:?}", labels_after_1);
    let ans = labels_after_1[1] as usize * labels_after_1[2] as usize;
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
    use super::*;

    pub(crate) fn full_input(input: &str) -> Result<Input, String> {
        let line = input.lines().next().ok_or("Input is empty".to_string())?;
        line.chars()
            .map(|c| c.to_digit(10))
            .try_collect()
            .ok_or("Input contains non-digits".to_string())
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn parses_line() {
            assert_eq!(full_input("135246"), Ok(vec![1, 3, 5, 2, 4, 6]));
        }

        #[test]
        fn parses_with_line_ending() {
            assert_eq!(full_input("135246\n"), Ok(vec![1, 3, 5, 2, 4, 6]));
        }

        #[test]
        fn parses_with_2_line_endings() {
            assert_eq!(full_input("135246\n\n"), Ok(vec![1, 3, 5, 2, 4, 6]));
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    /*
       #[test]
       fn find_index_immediate() {
       let cups = [3, 0, 0, 0, 2, 5, 4, 6, 7];
       assert_eq!(find_new_dest(&cups, 3), 4);
       }

       #[test]
       fn find_index_skip_1() {
       let cups = [3, 0, 0, 0, 5, 1, 4, 6, 7];
       assert_eq!(find_new_dest(&cups, 3), 5);
       }

       #[test]
       fn find_index_loop_around() {
       let cups = [3, 0, 0, 0, 4, 5, 8, 6, 7];
       assert_eq!(find_new_dest(&cups, 3), 6);
       }
       */

    #[test]
    fn cycle_once() {
        let mut cups = [3, 8, 9, 1, 2, 5, 4, 6, 7];
        let starting_index = 0;
        cycle(&mut cups, starting_index);
        assert_eq!(cups, [3, 2, 8, 9, 1, 5, 4, 6, 7]);
    }

    #[test]
    fn cycle_once_2() {
        let mut cups = NextsList::from_extended_list(&[3, 8, 9, 1, 2, 5, 4, 6, 7], 9);
        let starting_cup = 3;
        let next_cup = cycle2(&mut cups, starting_cup);
        assert_eq!(next_cup, Some(2));
        assert_eq!(cups.iter_from_1().collect::<Vec<Cup>>(), vec![1, 5, 4, 6, 7, 3, 2, 8, 9]);
    }

    #[test]
    fn cycle_once_wrap() {
        let mut cups = NextsList::from_extended_list(&[3, 2, 8, 9, 1, 5, 4, 6, 7], 9);
        let starting_cup = 6;
        let next_cup = cycle2(&mut cups, starting_cup);
        assert_eq!(next_cup, Some(8));
        assert_eq!(cups.iter_from_1().collect::<Vec<Cup>>(), vec![1, 5, 7, 3, 2, 4, 6, 8, 9]);
    }
}
