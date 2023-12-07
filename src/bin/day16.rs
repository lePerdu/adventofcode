#![feature(drain_filter)]

use std::{
    collections::HashMap,
    io::{stdin, Read},
    ops::RangeInclusive,
};

type Value = u32;

type Range = RangeInclusive<u32>;

#[derive(Debug, PartialEq, Eq, Hash)]
struct Rule {
    field_name: String,
    range1: Range,
    range2: Range,
}

impl Rule {
    fn contains(&self, value: &u32) -> bool {
        self.range1.contains(value) || self.range2.contains(value)
    }
}

#[derive(Debug)]
struct Ticket(Vec<Value>);

#[derive(Debug)]
struct Input {
    rules: Vec<Rule>,
    my_ticket: Ticket,
    nearby_tickets: Vec<Ticket>,
}

impl Input {
    fn is_value_valid(&self, value: &u32) -> bool {
        self.rules.iter().any(|r| r.contains(value))
    }

    fn is_valid(&self, ticket: &Ticket) -> bool {
        ticket.0.iter().all(|v| self.is_value_valid(v))
    }
}

fn part1(input: &Input) {
    let all_values = input
        .nearby_tickets
        .iter()
        .flat_map(|t| t.0.iter().copied());
    let ans: Value = all_values.filter(|v| !input.is_value_valid(v)).sum();
    println!("Part1: {}", ans);
}

fn determine_field_name_for_col<'r>(
    rules: impl IntoIterator<Item = &'r Rule>,
    column: &[Value],
) -> Option<&'r str> {
    let possible_rules = rules
        .into_iter()
        .filter(|rule| column.iter().all(|value| rule.contains(&value)))
        .collect::<Vec<&Rule>>();
    if possible_rules.len() == 1 {
        Some(&possible_rules[0].field_name)
    } else {
        None
    }
}

fn part2(input: &Input) {
    let valid_nearby_tickets = input.nearby_tickets.iter().filter(|t| input.is_valid(t));
    let valid_tickets = std::iter::once(&input.my_ticket)
        .chain(valid_nearby_tickets)
        .collect::<Vec<_>>();
    let field_count = input.my_ticket.0.len();
    assert_eq!(field_count, input.rules.len());

    let mut remaining_rules: HashMap<&str, &Rule> = input
        .rules
        .iter()
        .map(|r| (r.field_name.as_ref(), r))
        .collect();
    let mut remaining_columns: Vec<(usize, Vec<Value>)> = (0..field_count)
        .map(|col| {
            (
                col,
                valid_tickets.iter().map(|ticket| ticket.0[col]).collect(),
            )
        })
        .collect();
    let mut resolved_column_names = vec![None; field_count];

    while remaining_rules.len() > 0 {
        let n_removed = remaining_columns
            .drain_filter(|(col_index, col_values)| {
                if let Some(field_name) =
                    determine_field_name_for_col(remaining_rules.values().copied(), col_values)
                {
                    resolved_column_names[*col_index] = Some(field_name);
                    remaining_rules.remove(field_name);
                    return true;
                } else {
                    return false;
                }
            })
            .count();

        assert_ne!(n_removed, 0, "We're stuck!");
        assert_eq!(
            remaining_columns.len(),
            remaining_rules.len(),
            "Length mis-match"
        );
    }

    println!("Columns: {:?}", resolved_column_names);

    let ans: u64 = resolved_column_names
        .iter()
        .enumerate()
        .filter(|(_, name)| name.expect("Missing column name").starts_with("departure"))
        .map(|(col, _)| input.my_ticket.0[col] as u64)
        .product();
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
    use crate::{Input, Range, Rule, Ticket};
    use nom::{
        bytes::complete::{tag, take_until1},
        character::complete::{char, newline, u32},
        combinator::complete,
        multi::separated_list1,
        sequence::{separated_pair, tuple},
        Parser,
    };

    type IResult<'a, O> = nom::IResult<&'a str, O>;

    fn range(input: &str) -> IResult<Range> {
        separated_pair(u32, char('-'), u32)
            .map(|(min, max)| Range::new(min, max))
            .parse(input)
    }

    fn rule(input: &str) -> IResult<Rule> {
        tuple((take_until1(":"), tag(": "), range, tag(" or "), range))
            .map(|(field_name, _, range1, _, range2)| Rule {
                field_name: field_name.to_string(),
                range1,
                range2,
            })
            .parse(input)
    }

    fn ticket(input: &str) -> IResult<Ticket> {
        separated_list1(char(','), u32)
            .map(|t| Ticket(t))
            .parse(input)
    }

    pub(crate) fn full_input(input: &str) -> Result<Input, nom::Err<nom::error::Error<&str>>> {
        complete(tuple((
            separated_list1(newline, rule),
            tag("\n\nyour ticket:\n"),
            ticket,
            tag("\n\nnearby tickets:\n"),
            separated_list1(newline, ticket),
        )))
        .map(|(rules, _, my_ticket, _, nearby_tickets)| Input {
            rules,
            my_ticket,
            nearby_tickets,
        })
        .parse(input)
        .map(|(_, result)| result)
    }

    #[cfg(test)]
    mod test {}
}

#[cfg(test)]
mod test {}
