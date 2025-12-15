package day7

import "core:c"
import "core:fmt"
import "core:os"
import "core:slice"

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

Cell :: enum (u8) {
	Empty    = '.',
	Start    = 'S',
	Splitter = '^',
	Beam     = '|',
}

Pos :: struct {
	row, col: int,
}

Input :: struct {
	manifold: grid.Grid(Cell),
	start:    Pos,
}

parse_input :: proc(text: string) -> (Input, bool) {
	text_grid, ok := grid.parse_ascii_grid(text)
	if !ok {
		return {}, false
	}

	start_pos: Pos = {-1, -1}
	for row in 0 ..< text_grid.rows {
		for col in 0 ..< text_grid.cols {
			c := grid.get(text_grid, row, col)
			switch c {
			case u8(Cell.Empty), u8(Cell.Splitter):
			case u8(Cell.Start):
				if start_pos.row != -1 {
					fmt.fprintfln(
						os.stderr,
						"multiple starts found: %v and %v",
						start_pos,
						Pos{row, col},
					)
					return {}, false
				}
				start_pos = {row, col}
			case:
				fmt.fprintfln(os.stderr, "invalid input cell: %c", c)
				return {}, false
			}
		}
	}
	return {manifold = transmute(grid.Grid(Cell))text_grid, start = start_pos}, true
}

part1 :: proc(input: Input) -> int {
	split_count := 0

	manifold_state := grid.clone(input.manifold)

	pending_beams := make([dynamic]Pos, 0, 256)
	append(&pending_beams, input.start)
	for len(pending_beams) > 0 {
		current_beam := pop(&pending_beams)
		// Scan down to find next splitter
		beam_loop: for row in current_beam.row + 1 ..< manifold_state.rows {
			switch grid.get(manifold_state, row, current_beam.col) {
			case .Empty:
				grid.set(manifold_state, row, current_beam.col, Cell.Beam)
			// Continue loop
			case .Splitter:
				split_count += 1
				if grid.get(manifold_state, row, current_beam.col - 1) == .Empty {
					grid.set(manifold_state, row, current_beam.col - 1, Cell.Beam)
					append(&pending_beams, Pos{row, current_beam.col - 1})
				}
				if grid.get(manifold_state, row, current_beam.col + 1) == .Empty {
					grid.set(manifold_state, row, current_beam.col + 1, Cell.Beam)
					append(&pending_beams, Pos{row, current_beam.col + 1})
				}
				break beam_loop
			case .Beam:
				// Already a beam there, so stop
				break beam_loop
			case .Start:
				fmt.fprintfln(os.stderr, "unexpected start at %v", Pos{row, current_beam.col})
				return 0
			}
		}
		// If no splitter found, the beam is done
	}

	return split_count
}

part2 :: proc(input: Input) -> u64 {
	// Number of branches each splitter leads to
	splitter_branch_counts := grid.make(u64, input.manifold.rows, input.manifold.cols)

	// Fill from the bottom-up so that "future" splitters will be known
	for row := input.manifold.rows - 1; row >= 0; row -= 1 {
		for cell, col in grid.get_row(input.manifold, row) {
			if cell != .Splitter {
				continue
			}

			left_branches := calc_beam_branches(
				input.manifold,
				splitter_branch_counts,
				Pos{row, col - 1},
			)
			right_branches := calc_beam_branches(
				input.manifold,
				splitter_branch_counts,
				Pos{row, col + 1},
			)
			grid.set(splitter_branch_counts, row, col, left_branches + right_branches)
		}
	}

	return calc_beam_branches(input.manifold, splitter_branch_counts, input.start)
}

calc_beam_branches :: proc(
	manifold: grid.Grid(Cell),
	branch_counts: grid.Grid(u64),
	start_pos: Pos,
) -> u64 {
	// Scan down to find next splitter
	for row in start_pos.row + 1 ..< manifold.rows {
		if grid.get(manifold, row, start_pos.col) == .Splitter {
			return grid.get(branch_counts, row, start_pos.col)
		}
	}
	// No splitters hit: 1 timeline
	return 1
}
