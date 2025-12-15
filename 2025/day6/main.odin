package day6

import "core:crypto/x25519"
import "core:fmt"
import "core:os"
import "core:reflect"
import "core:strconv"
import "core:strings"
import "core:text/i18n"

import "../common/grid"

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

Operator :: enum u8 {
	Plus  = '+',
	Times = '*',
}

Worksheet :: struct {
	operands:  grid.Grid(u64),
	operators: []Operator,
}

part1 :: proc(input: string) -> u64 {
	problems, ok := part1_parse_input(input)
	if !ok {
		fmt.fprintfln(os.stderr, "failed to parse Part 1 input")
		return 0
	}

	total: u64
	for op, col in problems.operators {
		subtotal: u64
		switch op {
		case .Plus:
			subtotal = 0
			for row in 0 ..< problems.operands.rows {
				subtotal += grid.get(problems.operands, row, col)
			}
		case .Times:
			subtotal = 1
			for row in 0 ..< problems.operands.rows {
				subtotal *= grid.get(problems.operands, row, col)
			}
		}
		total += subtotal
	}
	return total
}

part1_parse_input :: proc(text: string) -> (input: Worksheet, ok: bool) {
	operands_builder: grid.Builder(u64)
	grid.builder_init(&operands_builder)

	line_iter := text
	for line in strings.split_lines_iterator(&line_iter) {
		if strings.contains_any(line, "+*") {
			input.operators, ok = parse_operators(line)
			if !ok {
				return {}, false
			}
			break
		}

		row: []u64
		row, ok = parse_numbers(line)
		if !ok {
			return {}, false
		}

		err := grid.append(&operands_builder, row)
		if err != nil {
			fmt.fprintfln(os.stderr, "invalid grid append: %v", err)
			return {}, false
		}
	}

	input.operands = grid.build(operands_builder)

	if input.operands.cols != len(input.operators) {
		fmt.fprintfln(
			os.stderr,
			"# operators [%d] != # operand columns [%d]",
			len(input.operators),
			input.operands.cols,
		)
		ok = false
	} else {
		ok = true
	}
	return
}

parse_numbers :: proc(line: string) -> ([]u64, bool) {
	row: [dynamic]u64
	remaining := strings.trim_space(line)
	for len(remaining) > 0 {
		next_space_index := strings.index_byte(remaining, ' ')
		next_number_str: string

		if next_space_index == -1 {
			next_number_str = remaining
			remaining = ""
		} else {
			next_number_str = remaining[:next_space_index]
			remaining = strings.trim_left_space(remaining[next_space_index + 1:])
		}

		next_number, ok := strconv.parse_u64(next_number_str, 10)
		if !ok {
			fmt.fprintfln(os.stderr, "invalid operand: %w", next_number_str)
			return nil, false
		}

		append(&row, next_number)
	}

	return row[:], true
}

parse_operators :: proc(line: string) -> ([]Operator, bool) {
	operators: [dynamic]Operator
	remaining := strings.trim_space(line)
	for len(remaining) > 0 {
		op_char := remaining[0]
		remaining = strings.trim_left_space(remaining[1:])
		switch op_char {
		case '+', '*':
			append(&operators, cast(Operator)op_char)
		case:
			fmt.fprintfln(os.stderr, "invalid operator: %w", op_char)
			return nil, false
		}
	}
	return operators[:], true
}

part2 :: proc(input: string) -> u64 {
	worksheet, ok := part2_parse_input(input)
	if !ok {
		fmt.fprintfln(os.stderr, "failed to parse Part 2 input")
		return 0
	}

	total: u64
	for op, row in worksheet.operators {
		subtotal: u64
		switch op {
		case .Plus:
			subtotal = 0
			for val in worksheet.operands[row] {
				subtotal += val
			}
		case .Times:
			subtotal = 1
			for val in worksheet.operands[row] {
				subtotal *= val
			}
		}
		total += subtotal
	}
	return total
}

Part2Worksheet :: struct {
	operands:  [][]u64,
	operators: []Operator,
}

part2_parse_input :: proc(text: string) -> (input: Part2Worksheet, ok: bool) {
	operators_index := strings.index_any(text, "+*")
	operands_text := text[:operators_index]
	operators_text := text[operators_index:]

	// 123 328  51 64
	//  45 64  387 23
	//   6 98  215 314
	orig_operands_text: grid.Grid(u8)
	orig_operands_text, ok = grid.parse_ascii_grid(operands_text)
	if !ok {
		fmt.fprintln(os.stderr, "operands not in a proper grid")
		return
	}

	// 1
	// 24
	// 356
	//
	// 396
	// 248
	// 8
	//
	// ...
	flipped_operands_text := grid.clone_transposed(orig_operands_text)

	// 1 24 356
	// 396 248 8
	// ...
	operands: [dynamic][]u64

	current_row: [dynamic]u64
	for row in 0 ..< flipped_operands_text.rows {
		line := string(grid.get_row(flipped_operands_text, row))
		trimmed := strings.trim_space(line)
		if trimmed == "" {
			append(&operands, current_row[:])
			current_row = make([dynamic]u64, 0, len(current_row))
		} else {
			operand, ok := strconv.parse_u64(trimmed)
			if !ok {
				fmt.fprintfln(os.stderr, "invalid operand: %w", trimmed)
				return {}, false
			}

			append(&current_row, operand)
		}
	}
	append(&operands, current_row[:])

	input.operands = operands[:]
	input.operators, ok = parse_operators(operators_text)
	if !ok {
		return {}, false
	}

	if len(input.operands) != len(input.operators) {
		fmt.fprintfln(
			os.stderr,
			"# operators [%d] != # operand rows [%d]",
			len(input.operators),
			len(input.operands),
		)
		ok = false
	} else {
		ok = true
	}
	return
}
