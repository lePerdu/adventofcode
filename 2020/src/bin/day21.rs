#![feature(hash_drain_filter)]
#![feature(iter_intersperse)]

use std::collections::{HashMap, HashSet};
use std::io::{stdin, Read};

// Type wrappers to avoid accidentally using one as another

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Ingredient<'a>(&'a str);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Allergen<'a>(&'a str);

#[derive(Debug, PartialEq, Eq)]
struct Recipe<'a> {
    ingredients: Vec<Ingredient<'a>>,
    allergens: Vec<Allergen<'a>>,
}

#[derive(Debug)]
struct SolveState<'a> {
    potential_sources: HashMap<Allergen<'a>, HashSet<Ingredient<'a>>>,
    ingredients_mapping: HashMap<Ingredient<'a>, Allergen<'a>>,
    found_allergens: HashSet<Allergen<'a>>,
}

impl<'a> SolveState<'a> {
    fn new() -> Self {
        Self {
            potential_sources: HashMap::new(),
            ingredients_mapping: HashMap::new(),
            found_allergens: HashSet::new(),
        }
    }

    fn extract_ingredient(&mut self, allergen: Allergen<'a>) {
        if let Some(existing) = self.potential_sources.remove(&allergen) {
            let only_ingredient = *existing
                .iter()
                .next()
                .expect("Only call extract_ingredient with a singleton set");
            println!("Found {:?} -> {:?}", allergen, only_ingredient);
            self.found_allergens.insert(allergen);
            self.ingredients_mapping.insert(only_ingredient, allergen);

            // Have to have a temporary store for these since we can't borrow mutably inside the loop
            let mut newly_known = Vec::new();
            for (&allergen, ingredients) in self.potential_sources.iter_mut() {
                ingredients.remove(&only_ingredient);
                if ingredients.len() == 1 {
                    newly_known.push(allergen);
                }
            }

            for new_to_extract in newly_known {
                self.extract_ingredient(new_to_extract);
            }
        }
    }

    fn solve(mut self, recipes: &[Recipe<'a>]) -> HashMap<Ingredient<'a>, Allergen<'a>> {
        for r in recipes {
            for &allergen in r.allergens.iter() {
                if self.found_allergens.contains(&allergen) {
                    println!("Already found {:?}", allergen);
                    // No-op
                } else if let Some(existing) = self.potential_sources.get_mut(&allergen) {
                    println!("Merging {:?}", allergen);
                    existing.drain_filter(|i| !r.ingredients.contains(i));
                    match existing.len() {
                        0 => panic!("No ingredients match {:?}", allergen),
                        1 => self.extract_ingredient(allergen),
                        _ => {}
                    }
                } else {
                    println!("New allergen {:?}", allergen);
                    self.potential_sources
                        .insert(allergen, r.ingredients.iter().copied().collect());
                }
            }
        }

        if !self.potential_sources.is_empty() {
            panic!(
                "Did not find all allergens. Found {:?}, still have {:?}",
                self.ingredients_mapping, self.potential_sources
            );
        }

        self.ingredients_mapping
    }
}

fn solve<'a>(recipes: &[Recipe<'a>]) -> HashMap<Ingredient<'a>, Allergen<'a>> {
    SolveState::new().solve(recipes)
}

type Input<'a> = Vec<Recipe<'a>>;

fn part1<'a>(input: &[Recipe<'a>]) -> HashMap<Ingredient<'a>, Allergen<'a>> {
    let mapping = solve(input);
    println!("{:?}", mapping);

    let ans = input
        .iter()
        .flat_map(|r| r.ingredients.iter())
        .filter(|ing| !mapping.contains_key(ing))
        .count();
    println!("Part1: {}", ans);
    mapping
}

fn part2<'a>(mapping: &HashMap<Ingredient<'a>, Allergen<'a>>) {
    let mut l: Vec<(Ingredient<'a>, Allergen<'a>)> =
        mapping.iter().map(|(&i, &a)| (i, a)).collect();
    l.sort_by_key(|(_ing, allergen)| allergen.0);
    let ans = l
        .iter()
        .map(|(ingredient, _allergen)| ingredient.0)
        .intersperse(",")
        .collect::<String>();
    println!("Part2: {}", ans);
}

fn main() {
    let mut raw_input = String::new();
    stdin()
        .read_to_string(&mut raw_input)
        .expect("Failed to read input");
    let input = parse::full_input(&raw_input).expect("Failed to parse input");
    let mapping = part1(&input);
    part2(&mapping);
}

mod parse {
    use nom::{
        bytes::complete::tag,
        character::complete::{alpha1, char, multispace0, newline, space0, space1},
        combinator::all_consuming,
        multi::separated_list1,
        sequence::{delimited, terminated, tuple},
        Parser,
    };

    use super::*;

    type IResult<'a, O> = nom::IResult<&'a str, O>;

    fn recipe(input: &str) -> IResult<Recipe> {
        tuple((
            separated_list1(space1, alpha1.map(Ingredient)),
            delimited(
                tag(" (contains "),
                separated_list1(tuple((char(','), space0)), alpha1.map(Allergen)),
                char(')'),
            ),
        ))
        .map(|(ingredients, allergens)| Recipe {
            ingredients,
            allergens,
        })
        .parse(input)
    }

    pub(crate) fn full_input<'a>(
        input: &'a str,
    ) -> Result<Input<'a>, nom::Err<nom::error::Error<&'a str>>> {
        all_consuming(terminated(separated_list1(newline, recipe), multispace0))
            .parse(input)
            .map(|(_rest, result)| result)
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_recipe() {
            assert_eq!(
                recipe("abc def (contains eggs, milk)"),
                Ok((
                    "",
                    Recipe {
                        ingredients: vec![Ingredient("abc"), Ingredient("def")],
                        allergens: vec![Allergen("eggs"), Allergen("milk")],
                    }
                ))
            )
        }

        #[test]
        fn test_many_recipe() {
            assert_eq!(
                full_input("abc def (contains eggs, milk)\nhijk lmnopq rs (contains soy)\n\n"),
                Ok(vec![
                    Recipe {
                        ingredients: vec![Ingredient("abc"), Ingredient("def")],
                        allergens: vec![Allergen("eggs"), Allergen("milk")],
                    },
                    Recipe {
                        ingredients: vec![
                            Ingredient("hijk"),
                            Ingredient("lmnopq"),
                            Ingredient("rs")
                        ],
                        allergens: vec![Allergen("soy")],
                    },
                ])
            )
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
}
