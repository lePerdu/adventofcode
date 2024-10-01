#![feature(iterator_try_collect)]

use std::io::{stdin, Read};

type Input = Box<[u32]>;

fn compute_naive(input: &Input, count: usize) -> u32 {
    let mut history = Vec::with_capacity(count);
    history.extend_from_slice(input);
    for index in history.len()..count {
        let top = history[index - 1];
        let new_value = history
            .iter()
            .enumerate()
            .rev()
            // Don't include the last element
            .skip(1)
            .find(|(_, &n)| n == top)
            .map_or(0, |(i, _)| index - 1 - i);
        history.push(new_value as u32);
    }

    history[count - 1]
}

/// Map implementation optimized for mostly-contiguous indices
#[derive(Debug, Clone)]
struct VecMap<T>(Vec<Option<T>>);

impl<T> Default for VecMap<T> {
    fn default() -> Self {
        VecMap(Vec::new())
    }
}

impl<T> VecMap<T> {
    fn get(&self, index: usize) -> Option<&T> {
        self.0.get(index).and_then(|res| res.as_ref())
    }

    fn insert(&mut self, index: usize, elem: T) {
        while self.0.len() <= index {
            self.0.push(None);
        }

        self.0[index] = Some(elem);
    }
}

fn compute_cached(input: &Input, count: usize) -> u32 {
    // Mapping from number -> most recent index it occurred
    let mut history = VecMap::default();

    // Load all but the last element of the input into the history cache
    for (index, &n) in input.iter().take(input.len() - 1).enumerate() {
        history.insert(n as usize, index);
    }

    let mut top = *input.last().unwrap() as usize;
    for index in input.len()..count {
        let new_value = history
            .get(top)
            .map_or(0, |prev_index| index - 1 - prev_index);
        history.insert(top, index - 1);
        top = new_value;
    }

    top as u32
}

fn part1(input: &Input) {
    let ans = compute_naive(input, 2020);
    println!("Part1: {}", ans);
}

fn part2(input: &Input) {
    let ans = compute_cached(input, 30_000_000);
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
    use std::num::ParseIntError;

    use super::*;

    pub(crate) fn full_input(input: &str) -> Result<Input, ParseIntError> {
        input.trim().split(',').map(|n| n.parse()).try_collect()
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_input() {
            assert_eq!(full_input("0,3,6\n"), Ok(vec![0, 3, 6].into_boxed_slice()));
        }
    }
}

#[cfg(test)]
mod test {}
