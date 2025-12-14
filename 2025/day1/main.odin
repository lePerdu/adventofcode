package day1

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

	input := string(data)
	fmt.printfln("Part 1: %d", part1(input))
	fmt.printfln("Part 2: %d", part2(input))
}

part1 :: proc(input: string) -> uint {
	pos := 50
	hit_0_count: uint = 0

	remaining := input
	for line in strings.split_lines_iterator(&remaining) {
		clicks, ok := parse_rotation(line)
		if !ok {
			os.exit(1)
		}
		pos = (pos + clicks) %% 100
		if pos == 0 {
			hit_0_count += 1
		}
	}

	return hit_0_count
}

part2 :: proc(input: string) -> uint {
	pos := 50
	crossings: uint = 0

	remaining := input
	for line in strings.split_lines_iterator(&remaining) {
		clicks, ok := parse_rotation(line)
		if !ok {
			os.exit(1)
		}
		crossings += calculate_0_crossings(pos, clicks)
		pos = (pos + clicks) %% 100
	}

	return crossings
}

calculate_0_crossings :: proc(pos: int, rot: int) -> uint {
	assert(0 <= pos)
	assert(pos < 100)

	end := pos + rot
	if end > 0 {
		return uint(end) / 100
	} else if pos == 0 {
		return uint(-end) / 100
	} else {
		return uint(-end + 100) / 100
	}
}

parse_rotation :: proc(line: string) -> (int, bool) {
	sign: int
	switch line[0] {
	case 'L':
		sign = 1
	case 'R':
		sign = -1
	case:
		fmt.fprintfln(os.stderr, "invalid rotation direction: %s", line)
		return 0, false
	}

	clicks, ok := strconv.parse_u64(line[1:], 10)
	if !ok {
		fmt.fprintfln(os.stderr, "invalid rotation amount: %s", line)
		return 0, false
	}
	return sign * int(clicks), true
}

import "core:testing"

@(test)
test_0_crossings_positive :: proc(t: ^testing.T) {
	testing.expect(t, calculate_0_crossings(0, 24) == 0)
	testing.expect(t, calculate_0_crossings(0, 100) == 1)
	testing.expect(t, calculate_0_crossings(99, 1) == 1)
	testing.expect(t, calculate_0_crossings(50, 49) == 0)
	testing.expect(t, calculate_0_crossings(50, 50) == 1)
	testing.expect(t, calculate_0_crossings(50, 65) == 1)
	testing.expect(t, calculate_0_crossings(50, 250) == 3)
}

@(test)
test_0_crossings_negative :: proc(t: ^testing.T) {
	testing.expect(t, calculate_0_crossings(0, -24) == 0)
	testing.expect(t, calculate_0_crossings(0, -100) == 1)
	testing.expect(t, calculate_0_crossings(99, -98) == 0)
	testing.expect(t, calculate_0_crossings(99, -99) == 1)
	testing.expect(t, calculate_0_crossings(50, -49) == 0)
	testing.expect(t, calculate_0_crossings(50, -50) == 1)
	testing.expect(t, calculate_0_crossings(50, -65) == 1)
	testing.expect(t, calculate_0_crossings(50, -250) == 3)
}
