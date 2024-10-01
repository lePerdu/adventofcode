#![feature(iterator_try_collect)]

use std::{
    io::{stdin, Read},
    ops::{BitAnd, BitOr},
};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct Answers(u32);

impl Answers {
    const fn all() -> Self {
        Self(0b11111111111111111111111111) // 26 ones
    }

    fn from_index(question_index: u32) -> Self {
        Self(1 << question_index)
    }

    fn count_yes(&self) -> u32 {
        self.0.count_ones()
    }
}

impl BitOr for Answers {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl BitAnd for Answers {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl FromIterator<Answers> for Answers {
    fn from_iter<T: IntoIterator<Item = Answers>>(iter: T) -> Self {
        iter.into_iter().fold(Self(0), |a, b| a | b)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct GroupAnswers(Vec<Answers>);

impl GroupAnswers {
    fn union(&self) -> Answers {
        Answers::from_iter(self.0.iter().copied())
    }

    fn intersection(&self) -> Answers {
        self.0.iter().fold(Answers::all(), |a, &b| a & b)
    }
}

fn part1(input: &[GroupAnswers]) {
    let total: u32 = input.iter().map(|g| g.union().count_yes()).sum();
    println!("Part1: {}", total);
}

fn part2(input: &[GroupAnswers]) {
    let total: u32 = input.iter().map(|g| g.intersection().count_yes()).sum();
    println!("Part2: {}", total);
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
    use crate::{Answers, GroupAnswers};

    fn single_answer(c: char) -> Option<Answers> {
        if ('a'..='z').contains(&c) {
            Some(Answers::from_index(c as u32 - 'a' as u32))
        } else {
            None
        }
    }

    fn multiple_answers(s: &str) -> Option<Answers> {
        s.trim().chars().map(|c| single_answer(c)).try_collect()
    }

    fn group_answers(s: &str) -> Option<GroupAnswers> {
        s.lines()
            .map(multiple_answers)
            .try_collect()
            .map(|v| GroupAnswers(v))
    }

    pub(crate) fn full_input(s: &str) -> Option<Vec<GroupAnswers>> {
        s.split("\n\n").map(group_answers).try_collect()
    }

    #[cfg(test)]
    mod test {
        use std::vec;

        use crate::{Answers, GroupAnswers};

        use super::full_input;

        #[test]
        fn test_single_group() {
            assert_eq!(
                full_input("abc\nabd\nabe"),
                Some(vec![GroupAnswers(vec![
                    Answers(0b111),
                    Answers(0b1011),
                    Answers(0b10011),
                ])])
            )
        }

        #[test]
        fn test_multi_group() {
            assert_eq!(
                full_input("abc\nabd\nabe\n\na\na\n"),
                Some(vec![
                    GroupAnswers(vec![Answers(0b111), Answers(0b1011), Answers(0b10011),]),
                    GroupAnswers(vec![Answers(0b1), Answers(0b1),]),
                ])
            )
        }
    }
}
