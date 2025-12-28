package day10

import "base:intrinsics"
import "core:fmt"
import "core:math"
import "core:math/linalg"
import "core:mem"
import "core:os"
import "core:slice"
import "core:strconv"
import "core:strings"

import "../common"
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

Machine_Schematics :: struct {
	light_Mask: Light_Mask,
	buttons:    []Button_Schematic,
	joltages:   []Joltage,
}

// Light pattern stored as a bitmask. Bit N corresponds to index N in the
// pattern, so it will appear "flipped" compared to the original:
// ##.. => 0011
Light_Mask :: u16

Button_Schematic :: distinct Light_Mask

Joltage :: distinct u8

Input :: []Machine_Schematics

parse_input :: proc(text: string) -> (Input, bool) {
	return common.parse_lines(text, parse_machine)
}

parse_machine :: proc(line: string) -> (machine: Machine_Schematics, ok: bool) {
	cursor := line
	machine.light_Mask, ok = parse_lights(&cursor)
	if !ok do return
	machine.buttons, ok = parse_buttons(&cursor)
	if !ok do return
	machine.joltages, ok = parse_joltages(&cursor)
	if !ok do return

	if len(strings.trim_left_space(cursor)) > 0 {
		return {}, false
	}

	return
}

expect_char :: proc(cursor: ^string, c: u8) -> bool {
	if len(cursor) > 0 && cursor[0] == c {
		cursor^ = cursor[1:]
		return true
	} else {
		return false
	}
}

parse_lights :: proc(cursor: ^string) -> (Light_Mask, bool) {
	if !expect_char(cursor, '[') {
		return {}, false
	}

	pattern_str, ok := strings.split_by_byte_iterator(cursor, ']')
	if !ok do return {}, false
	cursor^ = strings.trim_left_space(cursor^)

	if len(pattern_str) > type_info_of(Light_Mask).size * 8 {
		return {}, false
	}

	pattern_bits: Light_Mask
	for c, index in pattern_str {
		switch c {
		case '.':
		case '#':
			pattern_bits |= 1 << u16(index)
		case:
			return {}, false
		}
	}
	return pattern_bits, true
}

parse_buttons :: proc(cursor: ^string) -> ([]Button_Schematic, bool) {
	buttons: [dynamic]Button_Schematic
	for len(cursor) == 0 || cursor[0] == '(' {
		if button, ok := parse_button(cursor); ok {
			append(&buttons, button)
		} else {
			break
		}
	}
	return buttons[:], true
}

parse_button :: proc(cursor: ^string) -> (Button_Schematic, bool) {
	if !expect_char(cursor, '(') {
		return 0, false
	}

	in_parens, ok := strings.split_by_byte_iterator(cursor, ')')
	if !ok do return 0, false
	cursor^ = strings.trim_left_space(cursor^)

	toggle_mask: Button_Schematic
	for num_str in strings.split_by_byte_iterator(&in_parens, ',') {
		if index, ok := strconv.parse_u64(num_str, 10); ok {
			toggle_mask |= 1 << uint(index)
		} else {
			return 0, false
		}
	}

	return toggle_mask, true
}

parse_joltages :: proc(cursor: ^string) -> ([]Joltage, bool) {
	if !expect_char(cursor, '{') {
		return nil, false
	}

	in_brackets, ok := strings.split_by_byte_iterator(cursor, '}')
	if !ok do return nil, false
	cursor^ = strings.trim_left_space(cursor^)

	joltages: [dynamic]Joltage
	for num_str in strings.split_by_byte_iterator(&in_brackets, ',') {
		if n, ok := strconv.parse_int(num_str, 10); ok {
			append(&joltages, Joltage(n))
		} else {
			return {}, false
		}
	}

	return joltages[:], true
}

press_button :: proc(state: Light_Mask, button: Button_Schematic) -> Light_Mask {
	return state ~ Light_Mask(button)
}

find_minimal_presses :: proc(
	buttons: []Button_Schematic,
	target: Light_Mask,
	current: Light_Mask = 0,
) -> (
	presses: int,
	found: bool,
) {
	if current == target {
		return 0, true
	}

	if len(buttons) == 0 {
		return 0, false
	}

	with_next_button := press_button(current, buttons[0])
	rest_buttons := buttons[1:]
	presses_with, found_with := find_minimal_presses(rest_buttons, target, with_next_button)
	presses_with += 1
	presses_without, found_without := find_minimal_presses(rest_buttons, target, current)

	if found_with && found_without {
		return min(presses_with, presses_without), true
	} else if found_with {
		return presses_with, true
	} else if found_without {
		return presses_without, true
	} else {
		return 0, false
	}
}

part1 :: proc(input: Input) -> int {
	total: int
	for machine in input {
		if presses, found := find_minimal_presses(machine.buttons, machine.light_Mask); found {
			total += presses
		} else {
			fmt.fprintln(os.stderr, "failed to find correct buttons for:", machine)
		}
	}
	return total
}

gaussian_elimination :: proc(g: grid.Grid($T)) where intrinsics.type_is_integer(T) {
	pivot_row, pivot_col: int
	for pivot_row < g.rows && pivot_col < g.cols {
		// Find max pivot column
		// TODO: Does it matter if it's the maximum since all arithmetic is in integers?

		abs_pivot_col_val: T
		target_row: int
		for row in pivot_row ..< g.rows {
			v := abs(grid.get(g, row, pivot_col))
			if v > abs_pivot_col_val {
				abs_pivot_col_val = v
				target_row = row
			}
		}

		if abs_pivot_col_val == 0 {
			// All 0's in this column
			pivot_col += 1
		} else {
			if target_row != pivot_row {
				grid.row_swap(g, pivot_row, target_row)
				// fmt.printfln("### swap %v, %v", pivot_row, target_row)
				// print_matrix(g)
			}
			// Cancel out pivot_col remaining rows
			pivot_val := grid.get(g, pivot_row, pivot_col)

			assert(pivot_val != 0)
			for col in 0 ..< pivot_col {
				assert(grid.get(g, pivot_row, col) == 0)
			}

			for row in pivot_row + 1 ..< g.rows {
				// TODO: Reduce scalars by GCD
				cancel_val := grid.get(g, row, pivot_col)
				grid.row_scale_add_scale(g, row, pivot_val, pivot_row, -cancel_val)
				// fmt.printfln(
				// 	"### row[%v] = row[%v] * %v + row[%v] * %v",
				// 	row,
				// 	row,
				// 	pivot_val,
				// 	pivot_row,
				// 	-cancel_val,
				// )
				// print_matrix(g)
				assert(grid.get(g, row, pivot_col) == 0)
			}

			pivot_row += 1
			pivot_col += 1
		}
	}
}

make_joltage_matrix :: proc(
	machine: Machine_Schematics,
	allocator := context.allocator,
) -> grid.Grid(i64) {
	m := grid.make(i64, len(machine.joltages), len(machine.buttons) + 1, allocator)
	for button, col in machine.buttons {
		bit_iter := button
		row := 0
		for bit_iter > 0 {
			if bit_iter & 1 == 1 {
				grid.set(m, row, col, 1)
			}

			bit_iter >>= 1
			row += 1
		}
	}

	end_col := len(machine.buttons)
	for joltage, row in machine.joltages {
		grid.set(m, row, end_col, i64(joltage))
	}
	return m
}

// Find first non-zero button index
first_significant_col :: proc(m: grid.Grid(i64), row: int) -> (index: int, ok: bool) {
	return slice.linear_search_proc(
		grid.get_row(m, row)[:m.cols - 1], // Ignore last column since it doesn't correspond to a button
		proc(n: i64) -> bool {return n != 0},
	)
}

// Assumes `m` is in row-echelon form
search_min_solution :: proc(m: grid.Grid(i64), max_cell: i64) -> i64 {
	solution_buf := make([]i64, m.cols - 1, context.temp_allocator)
	best := make([]i64, len(solution_buf), context.temp_allocator)

	solve_rec(m, max_cell, best, solution_buf, m.rows - 1, m.cols - 2)
	return slice_sum(best)
}

slice_sum :: proc(arr: []i64) -> i64 {
	sum: i64
	for v in arr {
		sum += v
	}
	return sum
}

apply_solution_in_row :: proc(m: grid.Grid(i64), solution: []i64, row: int) -> i64 {
	sum: i64
	for col in 0 ..< m.cols - 1 {
		sum += grid.get(m, row, col) * solution[col]
	}
	return sum
}

solve_rec :: proc(
	m: grid.Grid(i64),
	max_cell: i64,
	best: []i64,
	solution: []i64,
	cur_row, cur_col: int,
) {
	if cur_row < 0 {
		// Found a solution
		best_score := slice_sum(best)
		this_score := slice_sum(solution)
		if best_score == 0 || this_score < best_score {
			copy(best, solution)
		}
		return
	}

	leading_col, ok := first_significant_col(m, cur_row)
	if !ok {
		// See if there's no solution (indicative of an error elsewhere)
		assert(grid.get(m, cur_row, len(solution)) == 0)
		solve_rec(m, max_cell, best, solution, cur_row - 1, cur_col)
		return
	}

	if leading_col < cur_col {
		// TODO: Find a better way to limit this loop?
		for solution[cur_col] = 0; solution[cur_col] <= max_cell; solution[cur_col] += 1 {
			solve_rec(m, max_cell, best, solution, cur_row, cur_col - 1)
		}
		solution[cur_col] = 0
	} else {
		// Leading cell can be computed from the rest of the solution
		// TODO: Do a partial apply instead of setting the cell
		solution[leading_col] = 0
		running_total := apply_solution_in_row(m, solution, cur_row)
		target := grid.get(m, cur_row, len(solution))
		remaining := target - running_total
		scalar := grid.get(m, cur_row, leading_col)
		col_solution := remaining / scalar
		// TODO: Is the 0 check necessary?
		if col_solution >= 0 && col_solution * scalar == remaining {
			solution[leading_col] = col_solution
			solve_rec(m, max_cell, best, solution, cur_row - 1, cur_col - 1)
		} else {
			// No solution with current guesses
		}
	}
}

print_matrix :: proc(m: grid.Grid(i64)) {
	for row in 0 ..< m.rows {
		for col in 0 ..< m.cols - 1 {
			fmt.printf("%v\t", grid.get(m, row, col))
		}
		fmt.printfln("| %v", grid.get(m, row, m.cols - 1))
	}
}

calc_jotlage_presses :: proc(machine: Machine_Schematics) -> i64 {
	free_all(context.temp_allocator)
	m := make_joltage_matrix(machine, context.temp_allocator)

	max_cell: i64
	for joltage in machine.joltages {
		max_cell = max(max_cell, i64(joltage))
	}

	gaussian_elimination(m)

	presses := search_min_solution(m, max_cell)
	return presses
}

part2 :: proc(input: Input) -> i64 {
	total: i64
	for machine in input {
		total += calc_jotlage_presses(machine)
	}
	return total
}
