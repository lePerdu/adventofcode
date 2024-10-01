use std::{
    io::{stdin, Read},
    ops::RangeInclusive,
};

#[derive(Debug, PartialEq, Eq)]
struct Policy1 {
    letter: char,
    count: RangeInclusive<usize>,
}

#[derive(Debug, PartialEq, Eq)]
struct Policy2 {
    letter: char,
    positions: (usize, usize),
}

trait Policy {
    fn matches(&self, pass: &str) -> bool;
}

impl Policy for Policy1 {
    fn matches(&self, pass: &str) -> bool {
        let num = pass.chars().filter(|c| *c == self.letter).count();
        self.count.contains(&num)
    }
}

impl Policy for Policy2 {
    fn matches(&self, pass: &str) -> bool {
        let mut it = pass.chars();
        let pos1 = it.nth(self.positions.0 - 1);
        let pos2 = it.nth(self.positions.1 - self.positions.0 - 1);
        let some_letter = Some(self.letter);
        (pos1 == some_letter) ^ (pos2 == some_letter)
    }
}

type Input<'a> = Vec<(Policy1, &'a str)>;

fn policy_1_to_2(policy: &Policy1) -> Policy2 {
    Policy2 {
        letter: policy.letter,
        positions: (*policy.count.start(), *policy.count.end()),
    }
}

fn main() {
    let mut raw_input = String::new();
    stdin()
        .read_to_string(&mut raw_input)
        .expect("Failed to read input");
    let (_, input) = parse::full_input(&raw_input).expect("Failed to parse input");

    let part1 = input
        .iter()
        .filter(|(policy, password)| policy.matches(password))
        .count();
    println!("Part1: {}", part1);
    let part2 = input
        .iter()
        .filter(|(policy, password)| policy_1_to_2(policy).matches(password))
        .count();
    println!("Part2: {}", part2);
}

#[cfg(test)]
mod test {
    use crate::*;

    #[test]
    fn test_policy1_in_range() {
        let p = Policy1 {
            letter: 'a',
            count: 1..=3,
        };
        assert!(p.matches("abcde"));
        assert!(p.matches("abade"));
        assert!(p.matches("abaae"));
    }

    #[test]
    fn test_policy1_low_count() {
        assert!(!Policy1 {
            letter: 'a',
            count: 2..=3
        }
        .matches("abcde"));
    }

    #[test]
    fn test_policy1_high_range() {
        assert!(!Policy1 {
            letter: 'a',
            count: 1..=3
        }
        .matches("abaaa"));
    }

    #[test]
    fn test_policy2_single_pos() {
        let p = Policy2 {
            letter: 'a',
            positions: (1, 4),
        };
        assert!(p.matches("a__b"));
        assert!(p.matches("b__a"));
    }

    #[test]
    fn test_policy2_both_pos() {
        let p = Policy2 {
            letter: 'a',
            positions: (1, 4),
        };
        assert!(!p.matches("a__a"));
    }

    #[test]
    fn test_policy2_neither_pos() {
        let p = Policy2 {
            letter: 'a',
            positions: (1, 4),
        };
        assert!(!p.matches("baab"));
    }
}

mod parse {
    use nom::{
        character::complete::{alphanumeric1, anychar, char, digit1, multispace0, newline, space1},
        combinator::{eof, map_res},
        multi::separated_list1,
        sequence::Tuple,
    };

    use crate::*;

    type IResult<'a, O> = nom::IResult<&'a str, O>;

    fn number(input: &str) -> IResult<usize> {
        map_res(digit1, |s: &str| s.parse::<usize>())(input)
    }

    fn range(input: &str) -> IResult<RangeInclusive<usize>> {
        let (input, (low, _, high)) = (number, char('-'), number).parse(input)?;
        Ok((input, (low..=high)))
    }

    fn policy1(input: &str) -> IResult<(Policy1, &str)> {
        let (input, (range, _, letter, _, _, password)) =
            (range, space1, anychar, char(':'), space1, alphanumeric1).parse(input)?;
        Ok((
            input,
            (
                Policy1 {
                    letter,
                    count: range,
                },
                password,
            ),
        ))
    }

    pub(crate) fn full_input(input: &str) -> IResult<Input> {
        let (input, (res, _, _)) =
            (separated_list1(newline, policy1), multispace0, eof).parse(input)?;
        Ok((input, res))
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_policy() {
            assert_eq!(
                policy1("1-53 a: abcde"),
                Ok((
                    "",
                    (
                        Policy1 {
                            letter: 'a',
                            count: (1..=53)
                        },
                        "abcde"
                    )
                ))
            )
        }

        #[test]
        fn test_full_input() {
            assert_eq!(
                full_input("1-3 a: abcde\n2-9 b: cdefg\n"),
                Ok((
                    "",
                    vec![
                        (
                            Policy1 {
                                letter: 'a',
                                count: (1..=3)
                            },
                            "abcde"
                        ),
                        (
                            Policy1 {
                                letter: 'b',
                                count: (2..=9)
                            },
                            "cdefg"
                        ),
                    ]
                ))
            )
        }
    }
}
