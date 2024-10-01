use std::collections::BTreeMap;
use std::io::{stdin, Read};

type RuleId = u32;

#[derive(Debug, PartialEq, Eq)]
struct RuleSeq {
    rules: Vec<RuleId>,
}

#[derive(Debug, PartialEq, Eq)]
enum Rule {
    Char(char),
    Seq(Vec<RuleSeq>),
    Repeat(RuleId),
    RepeatNested(RuleId, RuleId),
}

#[derive(Debug, PartialEq, Eq)]
struct RuleSet {
    rules: Vec<Rule>,
}

// TODO Refactor to avoid using so many boxes?
/// Return matches in a `dyn Iterator` because it makes code much simpler than trying to keep state
/// in a custom iterator.
type MatchIter<'a> = Box<dyn Iterator<Item = &'a str> + 'a>;

impl RuleSet {
    pub fn try_from_map(mut rule_map: BTreeMap<RuleId, Rule>) -> Result<Self, String> {
        let count = rule_map.len();
        if count == 0 {
            return Err("Must contain at least 1 rule".to_owned());
        }

        let mut rule_vec = Vec::with_capacity(count);
        for id in 0..count {
            let (key, value) = rule_map.pop_first().ok_or(format!("Rule {} missing", id))?;
            // Ensure the map is "complete": i.e. it contains all keys from 0..len()
            if key != id as RuleId {
                return Err(format!("Rule {} missing", id));
            }
            rule_vec.push(value);
        }

        Ok(Self { rules: rule_vec })
    }

    fn get(&self, id: RuleId) -> &Rule {
        &self.rules[id as usize]
    }

    fn matches_seq<'a>(&'a self, s: &'a str, sequence: &'a [RuleId]) -> MatchIter<'a> {
        if sequence.is_empty() {
            Box::new(std::iter::once(s))
        } else {
            Box::new(
                self.matches_rule(s, sequence[0])
                    .flat_map(|rest| self.matches_seq(rest, &sequence[1..])),
            )
        }
    }

    fn matches_repeat<'a>(&'a self, s: &'a str, rule_id: RuleId) -> MatchIter<'a> {
        Box::new(
            self.matches_rule(s, rule_id).flat_map(move |rest| {
                std::iter::once(rest).chain(self.matches_repeat(rest, rule_id))
            }),
        )
    }

    fn matches_n_times<'a>(&'a self, s: &'a str, rule_id: RuleId, n: usize) -> MatchIter<'a> {
        if n == 0 {
            Box::new(std::iter::once(s))
        } else {
            Box::new(
                self.matches_rule(s, rule_id)
                    .flat_map(move |rest| self.matches_n_times(rest, rule_id, n - 1)),
            )
        }
    }

    fn matches_nesting<'a>(
        &'a self,
        s: &'a str,
        left_rule: RuleId,
        right_rule: RuleId,
    ) -> MatchIter<'a> {
        Box::new(self.matches_repeat(s, left_rule).enumerate().flat_map(
            move |(n_left, rest_after_left)| {
                self.matches_n_times(rest_after_left, right_rule, n_left + 1)
            },
        ))
    }

    fn matches_rule<'a>(&'a self, s: &'a str, rule_id: RuleId) -> MatchIter<'a> {
        match self.get(rule_id) {
            Rule::Char(c) => {
                if s.chars().next() == Some(*c) {
                    Box::new(std::iter::once(&s[1..]))
                } else {
                    Box::new(std::iter::empty())
                }
            }
            Rule::Seq(sequences) => Box::new(
                sequences
                    .iter()
                    .flat_map(|seq| self.matches_seq(s, &seq.rules)),
            ),
            Rule::Repeat(repeat) => self.matches_repeat(s, *repeat),
            Rule::RepeatNested(repeat_a, repeat_b) => self.matches_nesting(s, *repeat_a, *repeat_b),
        }
    }

    pub fn matches(&self, s: &str) -> bool {
        self.matches_rule(s, 0)
            .filter(|rest| rest.is_empty())
            .next()
            .is_some()
    }

    pub fn update_rule(&mut self, rule_id: RuleId, new_rule: Rule) {
        self.rules[rule_id as usize] = new_rule;
    }

    fn do_termination_scan(
        &self,
        cache: &mut BTreeMap<RuleId, bool>,
        terminal_stack: &mut Vec<RuleId>,
        rule_id: RuleId,
    ) -> bool {
        if let Some(&cached) = cache.get(&rule_id) {
            return cached;
        }

        let terminates = match self.get(rule_id) {
            Rule::Char(_) => true,
            Rule::Seq(sequences) => {
                if terminal_stack.contains(&rule_id) {
                    false
                } else {
                    terminal_stack.push(rule_id);
                    let result = sequences
                        .iter()
                        .flat_map(|s| s.rules.iter())
                        .all(|&sub_rule| self.do_termination_scan(cache, terminal_stack, sub_rule));
                    terminal_stack.pop();
                    result
                }
            }
            _ => false,
        };
        cache.insert(rule_id, terminates);
        terminates
    }

    pub fn find_terminating_rules(&self) -> BTreeMap<RuleId, bool> {
        let mut terminating = BTreeMap::new();
        let mut call_stack = Vec::new();

        for rule_id in 0..self.rules.len() {
            self.do_termination_scan(&mut terminating, &mut call_stack, rule_id as RuleId);
        }

        terminating
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Input {
    pub rule_set: RuleSet,
    pub messages: Vec<String>,
}

fn part1(input: &Input) {
    let matched = input.messages.iter().filter(|m| input.rule_set.matches(m));
    let mut count = 0;
    for m in matched {
        println!("Matched: {}", m);
        count += 1;
    }
    println!();
    println!("Part1: {}", count);
}

fn part2(input: &mut Input) {
    // Mutates the input, but that's ok since part2 runs last
    input.rule_set.update_rule(8, Rule::Repeat(42));
    input.rule_set.update_rule(11, Rule::RepeatNested(42, 31));

    // let classification = input.rule_set.find_terminating_rules();
    // for (id, c) in classification.iter() {
    //     println!(
    //         "{}: {}",
    //         id,
    //         if *c {
    //             "deterministic"
    //         } else {
    //             "non-deterministic"
    //         }
    //     );
    // }

    let matched = input.messages.iter().filter(|m| input.rule_set.matches(m));
    let mut count = 0;
    for m in matched {
        println!("Matched: {}", m);
        count += 1;
    }
    println!();
    println!("Part2: {}", count);
}

fn main() {
    let mut raw_input = String::new();
    stdin()
        .read_to_string(&mut raw_input)
        .expect("Failed to read input");
    let mut input = parse::full_input(&raw_input).expect("Failed to parse input");
    part1(&input);
    part2(&mut input);
}

mod parse {
    use super::*;
    use nom::{
        branch::alt,
        character::complete::{
            anychar, char, multispace0, newline, not_line_ending, space0, space1, u32,
        },
        combinator::{all_consuming, map_res, verify},
        multi::{fold_many1, separated_list1},
        sequence::{delimited, separated_pair, terminated, tuple},
        Parser,
    };

    type IResult<'a, O> = nom::IResult<&'a str, O>;

    fn rule_id(input: &str) -> IResult<RuleId> {
        u32(input)
    }

    fn char_rule(input: &str) -> IResult<Rule> {
        delimited(char('"'), anychar, char('"'))
            .map(|c| Rule::Char(c))
            .parse(input)
    }

    fn seq_rule(input: &str) -> IResult<Rule> {
        let seq = separated_list1(space1, rule_id).map(|ids| RuleSeq { rules: ids });
        separated_list1(delimited(space0, char('|'), space0), seq)
            .map(|sequences| Rule::Seq(sequences))
            .parse(input)
    }

    fn rule_line(input: &str) -> IResult<(RuleId, Rule)> {
        let rule_def = alt((char_rule, seq_rule));
        tuple((rule_id, char(':'), space0, rule_def))
            .map(|(id, _, _, def)| (id, def))
            .parse(input)
    }

    fn rule_set(input: &str) -> IResult<RuleSet> {
        map_res(
            fold_many1(
                terminated(rule_line, newline),
                BTreeMap::new,
                |mut acc, (id, new_rule)| {
                    acc.insert(id, new_rule);
                    acc
                },
            ),
            |map| RuleSet::try_from_map(map),
        )
        .parse(input)
    }

    fn messages(input: &str) -> IResult<Vec<String>> {
        separated_list1(
            newline,
            verify(not_line_ending, |s: &str| s.len() > 0).map(|s: &str| s.to_owned()),
        )
        .parse(input)
    }

    pub(crate) fn full_input(input: &str) -> Result<Input, nom::Err<nom::error::Error<&str>>> {
        all_consuming(terminated(
            separated_pair(rule_set, newline, messages),
            multispace0,
        ))
        .map(|(rule_set, messages)| Input { rule_set, messages })
        .parse(input)
        .map(|(_, input)| input)
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_char_rule() {
            assert_eq!(char_rule("\"g\""), Ok(("", Rule::Char('g'))));
        }

        #[test]
        fn test_seq_rule_single_item() {
            assert_eq!(
                seq_rule("34"),
                Ok(("", Rule::Seq(vec![RuleSeq { rules: vec![34] }])))
            );
        }

        #[test]
        fn test_seq_rule_multi_item_single_branch() {
            assert_eq!(
                seq_rule("1 2"),
                Ok(("", Rule::Seq(vec![RuleSeq { rules: vec![1, 2] }])))
            );
        }

        #[test]
        fn test_seq_rule_multi_branch_single_items() {
            assert_eq!(
                seq_rule("1 | 2"),
                Ok((
                    "",
                    Rule::Seq(vec![RuleSeq { rules: vec![1] }, RuleSeq { rules: vec![2] }])
                ))
            );
        }

        #[test]
        fn test_many_rules() {
            assert_eq!(
                rule_set(
                    "\
                    0: 1 2\n\
                    1: \"a\"\n\
                    2: 1 3 | 3 1\n\
                    3: \"b\"\n\
                    "
                ),
                Ok((
                    "",
                    RuleSet {
                        rules: vec![
                            Rule::Seq(vec![RuleSeq { rules: vec![1, 2] }]),
                            Rule::Char('a'),
                            Rule::Seq(vec![
                                RuleSeq { rules: vec![1, 3] },
                                RuleSeq { rules: vec![3, 1] }
                            ]),
                            Rule::Char('b'),
                        ]
                    }
                ))
            );
        }

        #[test]
        fn test_fails_on_missing_rule() {
            assert!(rule_set(
                "\
                    0: 1 2\n\
                    2: 1 3 | 3 1\n\
                    3: \"b\"\n\
                    "
            )
            .is_err());
        }

        #[test]
        fn test_many_rules_out_of_order() {
            assert_eq!(
                rule_set(
                    "\
                    1: \"a\"\n\
                    0: 1 2\n\
                    3: \"b\"\n\
                    2: 1 3 | 3 1\n\
                    "
                ),
                Ok((
                    "",
                    RuleSet {
                        rules: vec![
                            Rule::Seq(vec![RuleSeq { rules: vec![1, 2] }]),
                            Rule::Char('a'),
                            Rule::Seq(vec![
                                RuleSeq { rules: vec![1, 3] },
                                RuleSeq { rules: vec![3, 1] }
                            ]),
                            Rule::Char('b'),
                        ]
                    }
                ))
            );
        }

        #[test]
        fn test_full_input() {
            assert_eq!(
                full_input(
                    "\
                    1: \"a\"\n\
                    0: 1 2\n\
                    3: \"b\"\n\
                    2: 1 3 | 3 1\n\
                    \n\
                    abaa\n\
                    wkejrhe\n\
                    ddejklwe\n\
                    "
                ),
                Ok(Input {
                    rule_set: RuleSet {
                        rules: vec![
                            Rule::Seq(vec![RuleSeq { rules: vec![1, 2] }]),
                            Rule::Char('a'),
                            Rule::Seq(vec![
                                RuleSeq { rules: vec![1, 3] },
                                RuleSeq { rules: vec![3, 1] }
                            ]),
                            Rule::Char('b'),
                        ]
                    },
                    messages: vec![
                        "abaa".to_string(),
                        "wkejrhe".to_string(),
                        "ddejklwe".to_string(),
                    ],
                })
            );
        }
    }
}

#[cfg(test)]
mod test {}
