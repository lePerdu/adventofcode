use std::io::stdin;

type Adapter = u32;

fn part1(input: &[Adapter]) {
    let deltas = (1..input.len()).map(|snd_index| input[snd_index] - input[snd_index - 1]);
    let (ones, threes) = deltas.fold((0, 0), |(ones, threes), d| match d {
        1 => (ones + 1, threes),
        2 => (ones, threes),
        3 => (ones, threes + 1),
        _ => {
            println!("Warning: invalid delta: {}", d);
            (ones, threes)
        }
    });
    let ans = ones * threes;
    println!("Part1: {} ({} * {})", ans, ones, threes);
}

fn iter_successor_indices<'a>(
    input: &'a [Adapter],
    from_index: usize,
) -> impl Iterator<Item = usize> + 'a {
    let from_value = input[from_index];
    input
        .iter()
        .enumerate()
        .skip(from_index + 1)
        .take_while(move |(_, value)| *value - from_value <= 3)
        .map(|(index, _)| index)
}

fn part2(input: &[Adapter]) {
    let mut routes_to_cache: Vec<u64> = vec![0; input.len()];
    routes_to_cache[0] = 1;
    for index in 0..input.len() {
        let count_to_current = routes_to_cache[index];
        for followers in iter_successor_indices(input, index) {
            routes_to_cache[followers] += count_to_current;
        }
    }

    println!("{:?}", routes_to_cache);
    let count_to_end = routes_to_cache.last().unwrap();
    print!("Part2: {}", count_to_end);
}

fn build_adapter_chain(it: impl Iterator<Item = Adapter>) -> Box<[Adapter]> {
    let mut result = Vec::with_capacity(it.size_hint().0 + 2);
    result.push(0);
    result.extend(it);
    result.sort();
    result.push(result.last().unwrap() + 3);
    result.into_boxed_slice()
}
fn main() {
    let input = build_adapter_chain(
        stdin()
            .lines()
            .map(|l| l.expect("Failed to read input"))
            .map(|l| l.parse().expect("Failed to parse input")),
    );
    part1(&input);
    part2(&input);
}

#[cfg(test)]
mod test {
    use crate::iter_successor_indices;

    #[test]
    fn test_succs() {
        assert_eq!(
            iter_successor_indices(&[0, 2, 3, 4, 5], 0).collect::<Vec<usize>>(),
            vec![1, 2]
        );
    }

    #[test]
    fn test_succs_at_end() {
        assert_eq!(
            iter_successor_indices(&[0, 2, 3, 4, 5], 4).collect::<Vec<usize>>(),
            vec![]
        );
    }
}
