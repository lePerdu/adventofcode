use std::io::{stdin, Read};

fn part1(input: &Input) {}

fn part2(input: &Input) {}

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

    pub(crate) fn full_input(input: &str) -> _ {}

    #[cfg(test)]
    mod test {
        use super::*;
    }
}

#[cfg(test)]
mod test {
    use super::*;
}
