#![feature(iter_collect_into)]

use std::{
    collections::{HashSet, VecDeque},
    io::{stdin, Read},
};

use log::debug;

type Card = u32;

#[derive(Debug, PartialEq, Eq)]
struct Input {
    player1: Vec<Card>,
    player2: Vec<Card>,
}

type Deck = VecDeque<Card>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Game {
    player1: Deck,
    player2: Deck,
}

#[derive(Debug, PartialEq, Eq)]
enum GameResult {
    P1Wins(Deck),
    P2Wins(Deck),
    Error(String),
}

impl From<&Input> for Game {
    fn from(input: &Input) -> Self {
        // Find total card count to pre-allocate all the necessary space
        let total_cards = input.player1.len() + input.player2.len();
        Self::from_size_and_iters(
            total_cards,
            input.player1.iter().copied(),
            input.player2.iter().copied(),
        )
    }
}

impl Game {
    fn from_size_and_iters<I>(total_count: usize, p1_cards: I, p2_cards: I) -> Self
    where
        I: Iterator<Item = Card>,
    {
        let mut p1 = VecDeque::with_capacity(total_count);
        p1_cards.collect_into(&mut p1);
        let mut p2 = VecDeque::with_capacity(total_count);
        p2_cards.collect_into(&mut p2);

        Self {
            player1: p1,
            player2: p2,
        }
    }

    fn play_round(mut self) -> Result<Game, GameResult> {
        match (self.player1.pop_front(), self.player2.pop_front()) {
            (Some(p1_top), Some(p2_top)) => {
                if p1_top == p2_top {
                    return Err(GameResult::Error("Cards should not be equal".to_string()));
                } else if p1_top > p2_top {
                    self.player1.push_back(p1_top);
                    self.player1.push_back(p2_top);
                    if self.player2.is_empty() {
                        return Err(GameResult::P1Wins(self.player1));
                    }
                } else if p2_top > p1_top {
                    self.player2.push_back(p2_top);
                    self.player2.push_back(p1_top);
                    if self.player1.is_empty() {
                        return Err(GameResult::P2Wins(self.player2));
                    }
                }

                return Ok(self);
            }
            _ => Err(GameResult::Error(
                "No cards available in the game".to_string(),
            )),
        }
    }

    fn play_game(self) -> GameResult {
        match self.play_round() {
            Ok(g) => g.play_game(),
            Err(res) => res,
        }
    }

    fn clone_for_round(&self, p1_count: Card, p2_count: Card) -> Self {
        Self::from_size_and_iters(
            (p1_count + p2_count) as usize,
            self.player1.range(..p1_count as usize).copied(),
            self.player2.range(..p2_count as usize).copied(),
        )
    }

    fn play_round_rec(mut self) -> Result<Game, GameResult> {
        debug!("");
        debug!("-- Round --");
        debug!("Player 1's deck: {:?}", self.player1);
        debug!("Player 2's deck: {:?}", self.player2);

        match (self.player1.pop_front(), self.player2.pop_front()) {
            (Some(p1_top), Some(p2_top)) => {
                debug!("Player 1 plays: {}", p1_top);
                debug!("Player 2 plays: {}", p2_top);

                let round_res = if p1_top as usize <= self.player1.len()
                    && p2_top as usize <= self.player2.len()
                {
                    // Recurse if possible
                    debug!("Starting sub-game");
                    let sub_game_res = self.clone_for_round(p1_top, p2_top).play_game_rec();
                    debug!("... Back to prior game");
                    sub_game_res
                } else if p1_top > p2_top {
                    GameResult::P1Wins(VecDeque::new())
                } else if p2_top > p1_top {
                    GameResult::P2Wins(VecDeque::new())
                } else {
                    /* p1_top == p2_top */
                    GameResult::Error("Cards should not be equal".to_string())
                };

                match round_res {
                    GameResult::P1Wins(_) => {
                        debug!("Player 1 wins");
                        self.player1.push_back(p1_top);
                        self.player1.push_back(p2_top);
                        if self.player2.is_empty() {
                            return Err(GameResult::P1Wins(self.player1));
                        }
                    }
                    GameResult::P2Wins(_) => {
                        debug!("Player 2 wins");
                        self.player2.push_back(p2_top);
                        self.player2.push_back(p1_top);
                        if self.player1.is_empty() {
                            return Err(GameResult::P2Wins(self.player2));
                        }
                    }
                    GameResult::Error(_) => return Err(round_res),
                }

                return Ok(self);
            }
            _ => Err(GameResult::Error(
                "No cards available in the game".to_string(),
            )),
        }
    }

    fn play_game_rec(mut self) -> GameResult {
        debug!("");
        debug!("=== New Game ===");

        let mut seen_states = HashSet::new();

        loop {
            // Player 1 wins by default if there is a loop
            if !seen_states.insert(self.clone()) {
                return GameResult::P1Wins(self.player1);
            }

            match self.play_round_rec() {
                Ok(next) => {
                    self = next;
                }
                Err(res) => return res,
            }
        }
    }
}

fn compute_deck_score(deck: &Deck) -> usize {
    deck.iter()
        .rev()
        .enumerate()
        .map(|(index, value)| (index + 1) * (*value as usize))
        .sum()
}

fn part1(input: &Input) {
    let ans = match Game::from(input).play_game() {
        GameResult::P1Wins(deck) => {
            println!("P1 wins: {:?}", deck);
            compute_deck_score(&deck)
        }
        GameResult::P2Wins(deck) => {
            println!("P2 wins: {:?}", deck);
            compute_deck_score(&deck)
        }
        GameResult::Error(msg) => {
            println!("Game error: {}", msg);
            return;
        }
    };

    println!("Part1: {}", ans);
}

fn part2(input: &Input) {
    let ans = match Game::from(input).play_game_rec() {
        GameResult::P1Wins(deck) => {
            println!("P1 wins: {:?}", deck);
            compute_deck_score(&deck)
        }
        GameResult::P2Wins(deck) => {
            println!("P2 wins: {:?}", deck);
            compute_deck_score(&deck)
        }
        GameResult::Error(msg) => {
            println!("Game error: {}", msg);
            return;
        }
    };

    println!("Part2: {}", ans);
}

fn main() {
    let mut raw_input = String::new();
    stdin()
        .read_to_string(&mut raw_input)
        .expect("Failed to read input");
    let input = parse::full_input(&raw_input).expect("Failed to parse input");
    // part1(&input);
    part2(&input);
}

mod parse {
    use nom::{
        bytes::complete::tag,
        character::complete::{multispace0, newline, u32},
        combinator::all_consuming,
        multi::{many1_count, separated_list1},
        sequence::{preceded, separated_pair, terminated},
        Parser,
    };

    use super::*;

    type IResult<'a, O> = nom::IResult<&'a str, O>;

    fn deck(input: &str) -> IResult<Vec<Card>> {
        separated_list1(newline, u32).parse(input)
    }

    pub(crate) fn full_input(input: &str) -> Result<Input, nom::Err<nom::error::Error<&str>>> {
        all_consuming(terminated(
            separated_pair(
                preceded(tag("Player 1:\n"), deck),
                many1_count(newline), // use _count to avoid allocations
                preceded(tag("Player 2:\n"), deck),
            ),
            multispace0,
        ))
        .map(|(player1, player2)| Input { player1, player2 })
        .parse(input)
        .map(|(_, res)| res)
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_full_input() {
            assert_eq!(
                full_input("Player 1:\n1\n5\n2\n\nPlayer 2:\n4\n6\n3\n\n"),
                Ok(Input {
                    player1: vec![1, 5, 2],
                    player2: vec![4, 6, 3],
                })
            );
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_normal_round() {
        assert_eq!(
            Game {
                player1: VecDeque::from(vec![1, 5, 2]),
                player2: VecDeque::from(vec![4, 6, 3]),
            }
            .play_round(),
            Ok(Game {
                player1: VecDeque::from(vec![5, 2]),
                player2: VecDeque::from(vec![6, 3, 4, 1]),
            })
        );
    }
}
