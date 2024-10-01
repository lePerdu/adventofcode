use std::io::stdin;

#[derive(Debug, Clone)]
struct Message(Vec<u64>);

fn find_2_sum_to(arr: &[u64], total: u64) -> Option<(u64, u64)> {
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

fn find_slice_sum_to(arr: &[u64], total: u64) -> Option<&[u64]> {
    for low_index in 0..=arr.len() - 2 {
        let mut running_total = arr[low_index];
        for high_index in low_index + 1..=arr.len() - 1 {
            running_total += arr[high_index];
            if total == running_total {
                return Some(&arr[low_index..=high_index]);
            } else if running_total > total {
                break;
            }
        }
    }

    None
}

const WINDOW_SIZE: usize = 25;

impl Message {
    fn len(&self) -> usize {
        self.0.len()
    }

    fn is_index_valid(&self, index: usize) -> bool {
        if index < WINDOW_SIZE {
            true
        } else {
            self.0
                .get(index)
                .and_then(|value| find_2_sum_to(&self.0[index - WINDOW_SIZE..index], *value))
                .is_some()
        }
    }

    fn find_first_invalid(&self) -> Option<usize> {
        (WINDOW_SIZE..self.len()).find(|i| !self.is_index_valid(*i))
    }
}

fn part1(input: &Message) -> u64 {
    let ans = input.find_first_invalid().expect("No invalid numbers");
    println!("Part1: [{}] = {}", ans, input.0[ans]);
    input.0[ans]
}

fn part2(input: &Message, first_invalid_value: u64) {
    let sum_slice = find_slice_sum_to(&input.0, first_invalid_value).expect("No slice sum");
    let lowest = sum_slice.iter().min().copied().unwrap();
    let highest = sum_slice.iter().max().copied().unwrap();
    println!("Part1: ({}, {}) = {}", lowest, highest, lowest + highest);
}

fn main() {
    let raw_numbers = stdin()
        .lines()
        .map(|l| l.expect("Failed to read input"))
        .map(|l| l.parse::<u64>().expect("Failed to parse input"))
        .collect();
    let message = Message(raw_numbers);

    let first_invalid = part1(&message);
    part2(&message, first_invalid);
}
