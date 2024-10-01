use std::io::stdin;

fn find_2_sum_to(arr: &[u32], total: u32) -> Option<(u32, u32)> {
    for (first_index, &n) in arr.iter().enumerate() {
        if n > total {
            continue;
        }
        let rest = total - n;
        if arr[first_index + 1..].iter().any(|m| *m == rest) {
            return Some((n, rest));
        }
    }

    None
}

fn find_3_sum_to(arr: &[u32], total: u32) -> Option<(u32, u32, u32)> {
    for (first_index, &a) in arr.iter().enumerate() {
        if a > total {
            continue;
        }
        let rest = total - a;
        if let Some((b, c)) = find_2_sum_to(&arr[first_index + 1..], rest) {
            return Some((a, b, c));
        }
    }

    None
}

const TOTAL_SUM: u32 = 2020;

fn part1(input: &[u32]) {
    if let Some((a, b)) = find_2_sum_to(input, TOTAL_SUM) {
        println!("Part1: {}", a * b);
    } else {
        println!("Part1: No answer");
    }
}

fn part2(input: &[u32]) {
    if let Some((a, b, c)) = find_3_sum_to(input, TOTAL_SUM) {
        println!("Part1: {}", a * b * c);
    } else {
        println!("Part1: No answer");
    }
}

fn main() {
    let numbers: Vec<u32> = stdin()
        .lines()
        .map(|l| l.unwrap().parse().expect("Failed to parse"))
        .collect();

    part1(&numbers);
    part2(&numbers);
}
