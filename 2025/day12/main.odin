package day12

import "core:fmt"
import "core:math/bits"
import "core:os"
import "core:strconv"
import "core:strings"

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
}

// 3x3 shape pattern stored as a bitmask
Shape :: distinct u16

shape_occupies :: proc(s: Shape, #any_int row: u8, #any_int col: u8) -> bool {
	index := row * 3 + col
	return s & (1 << index) == 1
}

shape_area :: proc(s: Shape) -> u8 {
	return u8(bits.count_ones(s))
}

// TODO: Is it always 6, or is that just my input/example?
SHAPE_COUNT :: 6

Region :: struct {
	width, height: u8,
	shape_counts:  [SHAPE_COUNT]u8,
}

Input :: struct {
	shapes:  [SHAPE_COUNT]Shape,
	regions: []Region,
}

parse_input :: proc(text: string) -> (input: Input, ok: bool) {
	cursor := text
	for i in 0 ..< SHAPE_COUNT {
		input.shapes[i], ok = parse_shape(&cursor, i)
		if !ok do return
	}

	regions: [dynamic]Region
	for line in strings.split_lines_iterator(&cursor) {
		if r, ok := parse_region(line); ok {
			append(&regions, r)
		} else {
			return {}, false
		}
	}
	input.regions = regions[:]

	ok = true
	return
}

parse_shape :: proc(cursor: ^string, expect_index: int) -> (Shape, bool) {
	index_line, ok := strings.split_lines_iterator(cursor)
	if !ok {
		return {}, false
	}

	if index_line[len(index_line) - 1] != ':' {
		return {}, false
	}
	if index_val, ok := strconv.parse_u64(index_line[:len(index_line) - 1], 10); ok {
		if index_val != u64(expect_index) {
			return {}, false
		}
	} else {
		return {}, false
	}

	shape: Shape
	for row in 0 ..< 3 {
		line, ok := strings.split_lines_iterator(cursor)
		if !ok {
			return {}, false
		}

		if len(line) != 3 {
			return {}, false
		}

		for col in 0 ..< 3 {
			switch line[col] {
			case '#':
				shape |= 1 << u8(3 * row + col)
			case '.':
			case:
				return {}, false
			}
		}
	}

	cursor^ = strings.trim_left_space(cursor^)
	return shape, true
}

parse_region :: proc(line: string) -> (region: Region, ok: bool) {
	cursor := line

	region.width, ok = split_parse_u8(&cursor, 'x')
	if !ok {
		return
	}

	region.height, ok = split_parse_u8(&cursor, ':')
	if !ok {
		return
	}

	cursor = strings.trim_left_space(cursor)
	for i in 0 ..< SHAPE_COUNT {
		n: u8
		n, ok = split_parse_u8(&cursor, ' ')
		if !ok {
			return
		}
		region.shape_counts[i] = n
	}
	return
}

split_parse_u8 :: proc(cursor: ^string, c: u8) -> (n: u8, ok: bool) {
	n_str: string
	n_64: u64
	n_str, ok = strings.split_by_byte_iterator(cursor, c)
	if !ok {
		return
	}
	n_64, ok = strconv.parse_u64(n_str, 10)
	if !ok {
		return
	}
	if n_64 > u64(max(u8)) {
		return 0, false
	}
	return u8(n_64), true
}

part1 :: proc(input: Input) -> u64 {
	// The input is always "trivial": If the blocks' total area fit in the region,
	// it's always possible to organize them in the region.
	possible: u64
	for r in input.regions {
		// Upcast to avoid overflow
		region_area := u32(r.width) * u32(r.height)
		present_area: u32
		for n, i in r.shape_counts {
			present_area += u32(n) * u32(shape_area(input.shapes[i]))
		}

		if region_area >= present_area {
			possible += 1
		}
	}

	return possible
}
