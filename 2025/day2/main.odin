package main

import "core:fmt"
import "core:os"
import "core:strconv"
import "core:strings"

main :: proc() {
	data, err := os.read_entire_file_or_err("input.txt")
	if err != nil {
		os.print_error(os.stderr, err, "failed to read input")
		os.exit(1)
	}

	input := strings.trim_right_space(string(data))
	fmt.printfln("Part 1: %d", part1(input))
	fmt.printfln("Part 2: %d", part2(input))
}

part1 :: proc(input: string) -> u64 {
	invalid_id_sum: u64 = 0

	remaining := input
	for range in input_iter_ranges(&remaining) {
		for id in range.start ..= range.end {
			if is_repeated_twice(id) {
				invalid_id_sum += id
			}
		}
	}
	return invalid_id_sum
}

part2 :: proc(input: string) -> u64 {
	invalid_id_sum: u64 = 0

	remaining := input
	for range in input_iter_ranges(&remaining) {
		for id in range.start ..= range.end {
			if is_repeated_twice_or_more(id) {
				invalid_id_sum += id
			}
		}
	}
	return invalid_id_sum
}

Range :: struct {
	start: u64,
	end:   u64,
}

input_iter_ranges :: proc(input: ^string) -> (Range, bool) {
	range_str, ok := strings.split_by_byte_iterator(input, ',')
	if !ok {
		return {}, false
	}

	return parse_range(range_str)
}

parse_range :: proc(str: string) -> (range: Range, ok: bool) {
	hyphen := strings.index_byte(str, '-')
	if hyphen == -1 {
		fmt.fprintfln(os.stderr, "invalid range: %s", str)
		return
	}

	range.start, ok = strconv.parse_u64(str[0:hyphen], 10)
	if !ok {
		fmt.fprintfln(os.stderr, "invalid range start: %s", str)
		return
	}
	range.end, ok = strconv.parse_u64(str[hyphen + 1:], 10)
	if !ok {
		fmt.fprintfln(os.stderr, "invalid range end: %s", str)
		return
	}
	if range.end < range.start {
		fmt.fprintfln(os.stderr, "invalid range bounds: %s: %v", str, range)
		return {}, false
	}
	return
}

MAX_U64_BASE_10_LEN :: 20

base_10_len :: proc(n: u64) -> int {
	limit: u64 = 10
	for exponent in 1 ..< MAX_U64_BASE_10_LEN {
		if n < limit {
			return exponent
		}
		limit *= 10
	}
	return MAX_U64_BASE_10_LEN
}

is_repeated_twice :: proc(id: u64) -> bool {
	id_len := base_10_len(id)
	if id_len % 2 == 1 {
		return false
	}

	repeat_len := id_len / 2

	cutoff_pow_10: u64 = 10
	for _ in 1 ..< repeat_len {
		cutoff_pow_10 *= 10
	}

	left_part := id / cutoff_pow_10
	right_part := id % cutoff_pow_10
	return left_part == right_part
}

is_repeated_twice_or_more :: proc(id: u64) -> bool {
	cutoff_pow_10: u64 = 10
	for _ in 1 ..< MAX_U64_BASE_10_LEN {
		if id <= cutoff_pow_10 {
			return false
		}

		if repeats_with_factor(id, cutoff_pow_10) {
			return true
		}
		cutoff_pow_10 *= 10
	}
	return false
}

repeats_with_factor :: proc(id: u64, repeat_factor: u64) -> bool {
	first_part := id % repeat_factor
	if first_part < (repeat_factor / 10) {
		// Repeated part has a leading 0
		return false
	}
	for rest := id / repeat_factor; rest > 0; rest /= repeat_factor {
		if rest % repeat_factor != first_part {
			return false
		}
	}
	return true
}

import "core:testing"

@(test)
test_base_10_len :: proc(t: ^testing.T) {
	testing.expect(t, base_10_len(0) == 1)
	testing.expect(t, base_10_len(9) == 1)
	testing.expect(t, base_10_len(10) == 2)
	testing.expect(t, base_10_len(28893) == 5)
	testing.expect(t, base_10_len(max(u64)) == MAX_U64_BASE_10_LEN)
}

@(test)
test_is_valid_id :: proc(t: ^testing.T) {
	testing.expect(t, is_repeated_twice(66))
	testing.expect(t, is_repeated_twice(1212))
	testing.expect(t, is_repeated_twice(10131013))

	testing.expect(t, !is_repeated_twice(22022))
	testing.expect(t, !is_repeated_twice(10081341))
	testing.expect(t, !is_repeated_twice(1001))
}

@(test)
test_repeats_with_factor :: proc(t: ^testing.T) {
	testing.expect(t, repeats_with_factor(66, 10))
	testing.expect(t, repeats_with_factor(1111111, 10))
	testing.expect(t, repeats_with_factor(1212121212, 100))
	testing.expect(t, repeats_with_factor(321321321, 1000))

	testing.expect(t, !repeats_with_factor(1212, 10))
	testing.expect(t, repeats_with_factor(1212, 100))
	testing.expect(t, !repeats_with_factor(1212, 1000))

	testing.expect(t, !repeats_with_factor(43210, 10))
	testing.expect(t, !repeats_with_factor(43210, 100))
	testing.expect(t, !repeats_with_factor(43210, 1000))
	testing.expect(t, !repeats_with_factor(43210, 10000))
}

@(test)
test_is_repeated_twice_or_more :: proc(t: ^testing.T) {
	testing.expect(t, is_repeated_twice_or_more(11))
	testing.expect(t, is_repeated_twice_or_more(22))
	testing.expect(t, is_repeated_twice_or_more(99))
	testing.expect(t, is_repeated_twice_or_more(111))
	testing.expect(t, is_repeated_twice_or_more(999))
	testing.expect(t, is_repeated_twice_or_more(1010))
	testing.expect(t, is_repeated_twice_or_more(1188511885))
	testing.expect(t, is_repeated_twice_or_more(222222))
	testing.expect(t, is_repeated_twice_or_more(446446))
	testing.expect(t, is_repeated_twice_or_more(38593859))
	testing.expect(t, is_repeated_twice_or_more(565656))
	testing.expect(t, is_repeated_twice_or_more(824824824))
	testing.expect(t, is_repeated_twice_or_more(2121212121))
}
