package main

import "core:fmt"
import "core:os"
import "core:strings"

import "../common/grid"

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

Cell :: enum {
	Empty = 0,
	Paper,
}

Grid :: grid.Grid(Cell)

parse_input :: proc(input: string) -> (Grid, bool) {
	builder: grid.Builder(Cell)
	grid.builder_init(&builder)

	iter := input
	for line in strings.split_lines_iterator(&iter) {
		row, err := grid.add_row(&builder, len(line))
		if err != nil {
			fmt.fprintfln(os.stderr, "failed to add row: %v", err)
			return {}, false
		}
		for col in 0 ..< len(line) {
			switch line[col] {
			case '.':
				row[col] = .Empty
			case '@':
				row[col] = .Paper
			case:
				fmt.fprintfln(os.stderr, "invalid cell: %c", line[col])
				return {}, false
			}
		}
	}

	return grid.build(builder), true
}

part1 :: proc(input: Grid) -> uint {
	total: uint = 0
	for row in 0 ..< input.rows {
		for col in 0 ..< input.cols {
			if grid.get(input, row, col) == .Paper && count_surrounding(input, row, col) < 4 {
				total += 1
			}
		}
	}
	return total
}

part2 :: proc(input: Grid) -> uint {
	total_removed: uint = 0

	state := grid.clone(input)
	for {
		iteration_removed: uint = 0
		for row in 0 ..< state.rows {
			for col in 0 ..< state.cols {
				if grid.get(state, row, col) == .Paper && count_surrounding(state, row, col) < 4 {
					iteration_removed += 1
					grid.set(state, row, col, Cell.Empty)
				}
			}
		}

		if iteration_removed == 0 {
			break
		} else {
			total_removed += iteration_removed
		}
	}

	return total_removed
}

count_surrounding :: proc(g: Grid, row: int, col: int) -> uint {
	n: uint = 0
	if grid.get_or_default(g, row - 1, col - 1, Cell.Empty) == .Paper do n += 1
	if grid.get_or_default(g, row - 1, col, Cell.Empty) == .Paper do n += 1
	if grid.get_or_default(g, row - 1, col + 1, Cell.Empty) == .Paper do n += 1
	if grid.get_or_default(g, row, col - 1, Cell.Empty) == .Paper do n += 1
	if grid.get_or_default(g, row, col + 1, Cell.Empty) == .Paper do n += 1
	if grid.get_or_default(g, row + 1, col - 1, Cell.Empty) == .Paper do n += 1
	if grid.get_or_default(g, row + 1, col, Cell.Empty) == .Paper do n += 1
	if grid.get_or_default(g, row + 1, col + 1, Cell.Empty) == .Paper do n += 1
	return n
}
