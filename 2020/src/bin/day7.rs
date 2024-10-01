use std::{
    collections::{HashMap, HashSet, VecDeque},
    io::{stdin, Read},
};

type BagId = u32;

#[derive(Debug, Clone, PartialEq, Eq)]
struct ContainsRule<B>(Vec<(B, u32)>);

impl<B: Eq> ContainsRule<B> {
    fn contains_bag(&self, bag_type: B) -> bool {
        self.0.iter().any(|(b, _)| *b == bag_type)
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Rule<'a> {
    bag_type: &'a str,
    contains: ContainsRule<&'a str>,
}

struct RuleSet {
    bag_ids: HashMap<String, BagId>,
    contains_rules: HashMap<BagId, ContainsRule<BagId>>,
}

impl<'a> FromIterator<Rule<'a>> for RuleSet {
    fn from_iter<T: IntoIterator<Item = Rule<'a>>>(rules: T) -> Self {
        let rules = rules.into_iter();
        // Init capacity is the number of rules
        let init_capacity = rules.size_hint().0;
        let mut bag_ids: HashMap<String, BagId> = HashMap::with_capacity(init_capacity);
        let mut contains_rules: HashMap<BagId, ContainsRule<BagId>> =
            HashMap::with_capacity(init_capacity);

        let mut get_or_create_id = |bag_type: &str| {
            match bag_ids.get(bag_type) {
                Some(id) => *id,
                None => {
                    let next_id = bag_ids.len() as BagId;
                    bag_ids.insert(bag_type.to_string(), next_id);
                    next_id
                }
            }
            // Not sure whether or not this is faster. Only 1 hash/lookup, but always allocates
            // a String, even when not inserting
            // bag_ids
            //     .try_insert(bag_type.to_string(), bag_ids.len() as BagId)
            //     .map_or_else(|occupied| occupied.value, |new_id| *new_id)
        };

        for rule in rules {
            let bag_id = get_or_create_id(rule.bag_type);
            let new_contains = ContainsRule(
                rule.contains
                    .0
                    .iter()
                    .map(|(bag_type, bag_count)| (get_or_create_id(bag_type), *bag_count))
                    .collect(),
            );
            contains_rules.insert(bag_id, new_contains);
        }

        Self {
            bag_ids,
            contains_rules,
        }
    }
}

impl RuleSet {
    fn get_bag_id(&self, name: &str) -> Option<BagId> {
        self.bag_ids.get(name).copied()
    }

    fn directly_includes<'a>(&'a self, target: BagId) -> impl Iterator<Item = BagId> + 'a {
        self.contains_rules
            .iter()
            .filter(move |(_, contains)| contains.contains_bag(target))
            .map(|(bag_id, _)| *bag_id)
    }

    fn count_includes(&self, target: BagId) -> u32 {
        let mut visited = HashSet::new();
        let mut total = 0;
        let mut queue = VecDeque::from([target]);
        while let Some(next) = queue.pop_front() {
            if !visited.insert(next) {
                continue;
            }

            total += 1;
            queue.extend(self.directly_includes(next));
        }

        total - 1 // Don't count the bag itself
    }

    fn count_included_in(&self, target: BagId) -> u32 {
        self.contains_rules.get(&target).map_or(0, |contains| {
            contains
                .0
                .iter()
                .map(|(bag, count)| (self.count_included_in(*bag) + 1) * count)
                .sum()
        })
    }
}

fn part1(rules: &RuleSet) {
    let gold_id = rules.get_bag_id("shiny gold").expect("bag not found");
    let count = rules.count_includes(gold_id);
    println!("Part1: {}", count);
}

fn part2(rules: &RuleSet) {
    let gold_id = rules.get_bag_id("shiny gold").expect("bag not found");
    let count = rules.count_included_in(gold_id);
    println!("Part1: {}", count);
}

fn main() {
    let mut raw_input = String::new();
    stdin()
        .read_to_string(&mut raw_input)
        .expect("Failed to read input");
    let (_, rules) = parse::full_input(&raw_input).expect("Failed to parse input");
    let rule_set = RuleSet::from_iter(rules.into_iter());
    part1(&rule_set);
    part2(&rule_set);
}

mod parse {
    use nom::{
        bytes::complete::tag,
        character::complete::{alpha1, char, newline, u32},
        combinator::{complete, opt, recognize, value},
        multi::{separated_list0, separated_list1},
        sequence::{separated_pair, terminated, tuple},
        Parser,
    };

    use crate::{ContainsRule, Rule};

    type IResult<'a, O> = nom::IResult<&'a str, O>;

    fn bag_type(input: &str) -> IResult<&str> {
        terminated(
            recognize(tuple((alpha1, char(' '), alpha1))),
            tuple((char(' '), tag("bag"), opt(char('s')))),
        )(input)
    }

    fn contains_rule(input: &str) -> IResult<ContainsRule<&str>> {
        let single = separated_pair(u32, char(' '), bag_type).map(|(count, name)| (name, count));
        let contains_list = separated_list1(tag(", "), single).map(|vec| ContainsRule(vec));
        let empty_list = value(ContainsRule(Vec::default()), tag("no other bags"));
        contains_list.or(empty_list).parse(input)
    }

    fn rule(input: &str) -> IResult<Rule> {
        terminated(
            separated_pair(bag_type, tag(" contain "), contains_rule)
                .map(|(bag_type, contains)| Rule { bag_type, contains }),
            char('.'),
        )
        .parse(input)
    }

    pub(crate) fn full_input(input: &str) -> IResult<Vec<Rule>> {
        complete(separated_list0(newline, rule))(input)
    }

    #[cfg(test)]
    mod test {
        use crate::Rule;

        use super::{bag_type, rule};

        #[test]
        fn test_bag_type() {
            assert_eq!(bag_type("light red bags"), Ok(("", "light red")));
            assert_eq!(bag_type("light red bag"), Ok(("", "light red")));
        }

        #[test]
        fn test_bag_type_multi_space() {
            assert!(bag_type("light  red").is_err());
        }

        #[test]
        fn test_rule() {
            assert_eq!(
                rule("light red bags contain 1 bright white bag, 2 muted yellow bags."),
                Ok((
                    "",
                    Rule {
                        bag_type: "light red",
                        contains: crate::ContainsRule(vec![
                            ("bright white", 1),
                            ("muted yellow", 2)
                        ]),
                    }
                ))
            );
        }
    }
}
