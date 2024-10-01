#![feature(iter_next_chunk)]

use std::{
    collections::HashMap,
    io::{stdin, Read},
};

/// 36-bit word representation.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
struct Word(u64);

impl From<u64> for Word {
    fn from(value: u64) -> Self {
        const BIT_MASK_36: u64 = 0b111111111111111111111111111111111111;
        Self(value & BIT_MASK_36)
    }
}

impl From<Word> for u64 {
    fn from(value: Word) -> Self {
        value.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BitMaskBit {
    Zero,
    One,
    DontCare,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct BitMask {
    /// Bitmask with a "1" at every position a "1" is forced and "0" everywhere else.
    or_mask: Word,
    /// Bitmask with a "0" at every position a "0" is forced and "1" everywhere else.
    and_mask: Word,
}

impl BitMask {
    /// Build an efficient bitmask from individual bits.
    fn new(bitmask_bits: &[BitMaskBit; 36]) -> Self {
        let mut or_mask = 0;
        let mut and_mask = 0;
        for &bit in bitmask_bits {
            or_mask = (or_mask << 1) + (if bit == BitMaskBit::One { 1 } else { 0 });
            and_mask = (and_mask << 1) + (if bit == BitMaskBit::Zero { 0 } else { 1 });
        }
        Self {
            or_mask: or_mask.into(),
            and_mask: and_mask.into(),
        }
    }

    fn apply(&self, word: Word) -> Word {
        Word::from((word.0 | self.or_mask.0) & self.and_mask.0)
    }
}

/// Default is all "don't care"s
impl Default for BitMask {
    fn default() -> Self {
        Self {
            or_mask: Word::from(0),
            and_mask: Word::from(u64::MAX),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Command {
    Mask([BitMaskBit; 36]),
    Write { address: Word, value: Word },
}

type Input = [Command];

fn part1(input: &Input) {
    let mut mask = BitMask::default();
    let mut memory: HashMap<Word, Word> = HashMap::new();

    for command in input {
        match command {
            Command::Mask(new_mask) => {
                mask = BitMask::new(new_mask);
            }
            Command::Write { address, value } => {
                memory.insert(*address, mask.apply(*value));
            }
        }
    }

    let total: u64 = memory.into_values().map(|word| word.0).sum();
    println!("Part1: {}", total);
}

struct MaskedAddressIter {
    /// Base addressed with the "0"s and "1"s of the mask applied and "X" bits set to 0.
    or_masked_address: Word,

    /// Pairs of (bit_index, state) for all of the "X"s in the mask
    dont_care_states: Box<[(u8, bool)]>,

    finished: bool,
}

impl MaskedAddressIter {
    fn new(base_address: Word, mask: &[BitMaskBit; 36]) -> Self {
        let mut or_masked_address = base_address;
        let mut dont_care_states = Vec::new();
        for (bit_index, bit) in mask.iter().rev().enumerate() {
            match bit {
                BitMaskBit::Zero => {}
                BitMaskBit::One => {
                    or_masked_address.0 |= 1 << bit_index;
                }
                BitMaskBit::DontCare => {
                    or_masked_address.0 &= !(1 << bit_index);
                    dont_care_states.push((bit_index as u8, false));
                }
            }
        }

        Self {
            or_masked_address,
            dont_care_states: dont_care_states.into_boxed_slice(),
            finished: false,
        }
    }
}

impl Iterator for MaskedAddressIter {
    type Item = Word;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        // Build up new address
        let mut address = self.or_masked_address;
        for (bit_index, state) in self.dont_care_states.iter() {
            if *state {
                address.0 |= 1 << bit_index;
            }
        }

        // Advance state for next iteration
        self.finished = true;
        for dont_care_index in 0..self.dont_care_states.len() {
            // Flip bit at position
            let state = &mut self.dont_care_states[dont_care_index].1;
            *state = !*state;
            if *state {
                self.finished = false;
                break;
            }
        }

        Some(address)
    }
}

fn part2(input: &Input) {
    let mut mask = [BitMaskBit::Zero; 36];
    let mut memory: HashMap<Word, Word> = HashMap::with_capacity(512);

    for command in input {
        match command {
            Command::Mask(new_mask) => {
                mask = new_mask.clone();
            }
            Command::Write { address, value } => {
                for masked_address in MaskedAddressIter::new(*address, &mask) {
                    memory.insert(masked_address, *value);
                }
            }
        }
    }

    let total: u64 = memory.into_values().map(|word| word.0).sum();
    println!("Part2: {}", total);
}

fn main() {
    let mut raw_input = String::new();
    stdin()
        .read_to_string(&mut raw_input)
        .expect("Failed to read input");
    let (_, input) = parse::full_input(&raw_input).expect("Failed to parse input");
    part1(&input);
    part2(&input);
}

mod parse {
    use nom::{
        bytes::complete::tag,
        character::{
            self,
            complete::{char, newline},
        },
        combinator::value,
        multi::{count, separated_list0},
        sequence::{preceded, tuple},
        Parser,
    };

    use crate::*;

    type IResult<'a, O> = nom::IResult<&'a str, O>;

    fn bitmask_bit(input: &str) -> IResult<BitMaskBit> {
        value(BitMaskBit::Zero, char('0'))
            .or(value(BitMaskBit::One, char('1')))
            .or(value(BitMaskBit::DontCare, char('X')))
            .parse(input)
    }

    fn bitmask(input: &str) -> IResult<[BitMaskBit; 36]> {
        count(bitmask_bit, 36)
            .map(|bits| bits.as_slice().try_into().unwrap())
            .parse(input)
    }

    fn command(input: &str) -> IResult<Command> {
        let mask_command = preceded(tag("mask = "), bitmask).map(|mask| Command::Mask(mask));
        let mem_command = tuple((
            tag("mem["),
            character::complete::u64,
            tag("] = "),
            character::complete::u64,
        ))
        .map(|(_, address, _, value)| Command::Write {
            address: address.into(),
            value: value.into(),
        });

        mask_command.or(mem_command).parse(input)
    }

    pub(crate) fn full_input(input: &str) -> IResult<Box<Input>> {
        separated_list0(newline, command)
            .map(|vec| vec.into_boxed_slice())
            .parse(input)
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_mask() {
            let mut bits = [BitMaskBit::DontCare; 36];
            bits[29] = BitMaskBit::One;
            bits[34] = BitMaskBit::Zero;
            assert_eq!(
                command("mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"),
                Ok(("", Command::Mask(bits)))
            );
        }

        #[test]
        fn test_write() {
            assert_eq!(
                command("mem[88] = 123"),
                Ok((
                    "",
                    Command::Write {
                        address: Word(88),
                        value: Word(123),
                    }
                ))
            );
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn convert_bitmask_dont_care() {
        let bits = [BitMaskBit::DontCare; 36];
        assert_eq!(
            BitMask::new(&bits),
            BitMask {
                or_mask: Word::from(0),
                and_mask: Word::from(u64::MAX),
            }
        );
    }

    #[test]
    fn convert_bitmask_ones() {
        let bits = [BitMaskBit::One; 36];
        assert_eq!(
            BitMask::new(&bits),
            BitMask {
                or_mask: Word::from(u64::MAX),
                and_mask: Word::from(u64::MAX),
            }
        );
    }

    #[test]
    fn convert_bitmask_zeros() {
        let bits = [BitMaskBit::Zero; 36];
        assert_eq!(
            BitMask::new(&bits),
            BitMask {
                or_mask: Word::from(0),
                and_mask: Word::from(0),
            }
        );
    }

    #[test]
    fn convert_bitmask_mixed() {
        let mut bits = [BitMaskBit::DontCare; 36];
        bits[29] = BitMaskBit::One;
        bits[34] = BitMaskBit::Zero;
        assert_eq!(
            BitMask::new(&bits),
            BitMask {
                or_mask: Word::from(0b1000000),
                and_mask: Word::from(!0b10),
            }
        );
    }

    #[test]
    fn apply_dont_care_mask() {
        assert_eq!(BitMask::default().apply(Word(0b10)), Word(0b10));
    }

    #[test]
    fn apply_one_mask() {
        assert_eq!(
            BitMask {
                or_mask: Word(0b11),
                and_mask: Word(0b11)
            }
            .apply(Word(0b10)),
            Word(0b11),
        );
    }

    #[test]
    fn apply_zero_mask() {
        assert_eq!(
            BitMask {
                or_mask: Word(0b00),
                and_mask: Word(0b00)
            }
            .apply(Word(0b10)),
            Word(0b00),
        );
    }

    #[test]
    fn iter_masked_address() {
        let mut mask = [BitMaskBit::Zero; 36];
        mask[30] = BitMaskBit::DontCare;
        mask[31] = BitMaskBit::One;
        mask[34] = BitMaskBit::One;
        mask[35] = BitMaskBit::DontCare;
        let mut it = MaskedAddressIter::new(Word(42), &mask);
        assert_eq!(
            it.next_chunk().unwrap(),
            [Word(26), Word(27), Word(58), Word(59)]
        );
        assert_eq!(it.next(), None);
    }
}
