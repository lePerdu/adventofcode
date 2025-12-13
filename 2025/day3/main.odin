package main

import "core:fmt"
import "core:os"
import "core:slice"
import "core:strings"
import "core:testing"

main :: proc() {
	data, err := os.read_entire_file_or_err("input.txt")
	if err != nil {
		os.print_error(os.stderr, err, "failed to read input")
		os.exit(1)
	}

	input, ok := parse_input(string(data))
	if !ok {
		os.exit(1)
	}
	fmt.printfln("Part 1: %d", part1(input))
	fmt.printfln("Part 2: %d", part2(input))
}

parse_input :: proc(input: string) -> ([][]u8, bool) {
	parsed: [dynamic][]u8
	line_iter := input
	for line in strings.split_lines_iterator(&line_iter) {
		batteries := make([]u8, len(line))
		for i in 0 ..< len(line) {
			ok: bool
			batteries[i], ok = to_digit(line[i])
			if !ok {
				fmt.fprintfln(os.stderr, "invalid battery bank: %s", input)
				return nil, false
			}
		}
		append(&parsed, batteries)
	}

	return parsed[:], true
}

to_digit :: proc(ch: u8) -> (u8, bool) {
	if '0' <= ch && ch <= '9' {
		return ch - '0', true
	} else {
		return 0, false
	}
}

part1 :: proc(input: [][]u8) -> u64 {
	return calc_total_max_joltage(input, 2)
}

part2 :: proc(input: [][]u8) -> u64 {
	return calc_total_max_joltage(input, 12)
}

calc_total_max_joltage :: proc(banks: [][]u8, digits: int) -> u64 {
	total: u64 = 0
	for bank in banks {
		m := calc_max_joltage(bank, digits)
		total += m
	}
	return total
}

calc_max_joltage :: proc(bank: []u8, digits: int) -> u64 {
	acc: u64 = 0
	subbank := bank

	for remaining := digits; remaining > 0; remaining -= 1 {
		assert(len(subbank) >= remaining)

		next_index: int = 0
		for index := 1; index < len(subbank) - (remaining - 1); index += 1 {
			if subbank[index] > subbank[next_index] {
				next_index = index
			}
		}

		acc = 10 * acc + u64(subbank[next_index])
		subbank = subbank[next_index + 1:]
	}

	return acc
}

@(test)
test_calc_max_joltage :: proc(t: ^testing.T) {
	testing.expect(t, calc_max_joltage([]u8{9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1}, 2) == 98)
	testing.expect(t, calc_max_joltage([]u8{8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9}, 2) == 89)
	testing.expect(t, calc_max_joltage([]u8{2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 7, 8}, 2) == 78)
	testing.expect(t, calc_max_joltage([]u8{8, 1, 8, 1, 8, 1, 9, 1, 1, 1, 1, 2, 1, 1, 1}, 2) == 92)

	testing.expect(
		t,
		calc_max_joltage([]u8{9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1}, 12) == 987654321111,
	)
	testing.expect(
		t,
		calc_max_joltage([]u8{8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9}, 12) == 811111111119,
	)
	testing.expect(
		t,
		calc_max_joltage([]u8{2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 7, 8}, 12) == 434234234278,
	)
	testing.expect(
		t,
		calc_max_joltage([]u8{8, 1, 8, 1, 8, 1, 9, 1, 1, 1, 1, 2, 1, 1, 1}, 12) == 888911112111,
	)
}
