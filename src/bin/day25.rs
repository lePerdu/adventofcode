use std::io::{stdin, Read};

type Key = u64;

#[derive(Debug)]
struct Input {
    card_pk: Key,
    door_pk: Key,
}

const PK_SUBJECT_NUMBER: Key = 7;
const PK_MODULUS: Key = 20201227;

fn transform(subject_number: Key, loop_size: Key) -> Key {
    // TODO Exponentiate by squaring if necessary
    let mut value = 1;
    for _ in 1..=loop_size {
        value = (value * subject_number) % PK_MODULUS;
    }

    value
}

fn find_loop_size(public_key: Key) -> Key {
    let mut value = 1;
    for n in 1..=PK_MODULUS {
        value = (value * PK_SUBJECT_NUMBER) % PK_MODULUS;

        if value == public_key {
            return n;
        }
    }

    panic!("No loop size matched");
}

fn part1(input: &Input) {
    let card_loop_size = find_loop_size(input.card_pk);
    let door_loop_size = find_loop_size(input.door_pk);

    let encryption_key1 = transform(input.card_pk, door_loop_size);
    let encryption_key2 = transform(input.door_pk, card_loop_size);
    assert_eq!(
        encryption_key1, encryption_key2,
        "Encryption keys do not match"
    );

    let ans = encryption_key1;
    println!("Part1: {}", ans);
}

fn main() {
    let mut raw_input = String::new();
    stdin()
        .read_to_string(&mut raw_input)
        .expect("Failed to read input");
    let input = parse::full_input(&raw_input).expect("Failed to parse input");
    part1(&input);
}

mod parse {
    use super::*;

    pub(crate) fn full_input(input: &str) -> Result<Input, String> {
        let ns: Vec<&str> = input.lines().take(2).collect();
        if ns.len() != 2 {
            return Err("Not enough lines".to_string());
        }
        Ok(Input {
            card_pk: ns[0].parse::<Key>().map_err(|e| e.to_string())?,
            door_pk: ns[1].parse::<Key>().map_err(|e| e.to_string())?,
        })
    }
}
