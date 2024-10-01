#![feature(iterator_try_collect)]

use std::io::{stdin, Read};

use num::{integer::ExtendedGcd, Integer};

type Time = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BusId {
    Period(Time),
    OutOfService,
}

#[derive(Debug, PartialEq, Eq)]
struct Input {
    earliest_depart: Time,
    bus_ids: Box<[BusId]>,
}

fn part1(input: &Input) {
    let next_depart_times = input
        .bus_ids
        .iter()
        .filter_map(|b| match b {
            BusId::Period(t) => Some(t),
            BusId::OutOfService => None,
        })
        .map(|period| (*period, period - input.earliest_depart % period));
    let ans = next_depart_times
        .min_by_key(|(_id, delay)| *delay)
        .expect("No departure times");
    let (bus_id, first_depart_time) = ans;
    println!("Part1: {} {:?}", first_depart_time * bus_id, ans);
}

fn part2(input: &Input) {
    let (time, modulus) = input
        .bus_ids
        .iter()
        .enumerate()
        .filter_map(|(index, b)| match b {
            // u64 and i64 both overflow in intermediate steps, even though the final result fits
            // TODO Use BigInt, or some modular arithmetic utilities which don't suffer from the
            // same problem in intermediate steps?
            BusId::Period(period) => Some((index as i128, *period as i128)),
            BusId::OutOfService => None,
        })
        // Each bus equates to the constraint
        //  depart_time + offset = 0 (mod period)
        // Which can be folded into a single constraint using the Chinese Remainder Theorem,
        // provided the periods are co-prime
        .try_fold((0, 1), |(time, modulus), (offset, period)| {
            let ExtendedGcd { gcd, x, y } = modulus.extended_gcd(&period);
            if gcd == 1 {
                let new_time = time * period * y - offset * modulus * x;
                let new_modulus = modulus * period;
                Some((new_time.rem_euclid(new_modulus), new_modulus))
            } else {
                None
            }
        })
        .expect("Times not co-prime");
    println!("Part2: {} (mod {})", time, modulus);
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

    fn bus_id(input: &str) -> Option<BusId> {
        if input == "x" {
            Some(BusId::OutOfService)
        } else if let Ok(id) = input.parse() {
            Some(BusId::Period(id))
        } else {
            None
        }
    }

    pub(crate) fn full_input(input: &str) -> Option<Input> {
        let (earliest_str, bus_ids_str) = input.split_once('\n')?;
        let earliest_depart = earliest_str.parse().ok()?;
        let bus_ids = bus_ids_str
            .trim()
            .split(',')
            .map(bus_id)
            .try_collect::<Box<[BusId]>>()?;
        Some(Input {
            earliest_depart,
            bus_ids,
        })
    }

    #[cfg(test)]
    mod test {
        use crate::{BusId, Input};

        use super::full_input;

        #[test]
        fn parse_input() {
            assert_eq!(
                full_input("939\n7,13,x,x,59,x,31,19"),
                Some(Input {
                    earliest_depart: 939,
                    bus_ids: Box::new([
                        BusId::Period(7),
                        BusId::Period(13),
                        BusId::OutOfService,
                        BusId::OutOfService,
                        BusId::Period(59),
                        BusId::OutOfService,
                        BusId::Period(31),
                        BusId::Period(19),
                    ])
                })
            );
        }
    }
}
