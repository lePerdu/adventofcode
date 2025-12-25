package day9

import "core:container/rbtree"
import "core:fmt"
import "core:os"
import "core:slice"
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

Pos :: common.Vec2(u64)

Input :: []Pos

parse_input :: proc(text: string) -> (Input, bool) {
	return common.parse_lines(text, proc(line: string) -> (Pos, bool) {
		return common.parse_vec2_u64(line, ',')
	})
}

part1 :: proc(input: Input) -> u64 {
	best_pair: [2]int
	best_area: u64 = 0
	for i in 0 ..< len(input) {
		for j in i + 1 ..< len(input) {
			area := rect_area(input[i], input[j])
			if area > best_area {
				best_pair = {i, j}
				best_area = area
			}
		}
	}
	return best_area
}

abs_diff :: #force_inline proc(a, b: u64) -> u64 {
	if a >= b {
		return a - b
	} else {
		return b - a
	}
}

rect_area :: proc(a, b: Pos) -> u64 {
	return (abs_diff(a.x, b.x) + 1) * (abs_diff(a.y, b.y) + 1)
}

SkipNode :: struct {
	skip_index: int,
}

SkipMapping :: struct {
	orig_to_skip: rbtree.Tree(u64, SkipNode),
	skip_to_orig: []u64,
}

Cell :: enum u8 {
	Unknown = '.',
	Border  = '#',
	Oob     = 'X',
}

part2 :: proc(input: Input) -> u64 {
	// Mappings from real -> skip coordinates
	unique_x_set, unique_y_set: rbtree.Tree(u64, SkipNode)
	rbtree.init(&unique_x_set)
	rbtree.init(&unique_y_set)
	for pos in input {
		rbtree.find_or_insert(&unique_x_set, pos.x, SkipNode{})
		rbtree.find_or_insert(&unique_y_set, pos.y, SkipNode{})
	}

	skip_x := create_skip_list_mappings(unique_x_set)
	skip_y := create_skip_list_mappings(unique_y_set)

	g := grid.make(Cell, len(skip_y.skip_to_orig), len(skip_x.skip_to_orig))
	grid.fill(g, Cell.Unknown)

	prev_skip_pos := skip_list_to_skip_pos(&skip_x, &skip_y, input[len(input) - 1])
	for pos in input {
		skip_pos := skip_list_to_skip_pos(&skip_x, &skip_y, pos)
		fill_line(g, prev_skip_pos, skip_pos)
		prev_skip_pos = skip_pos
	}

	flood_fill_from(g, 0, 0)
	flood_fill_from(g, 0, g.cols - 1)
	flood_fill_from(g, g.rows - 1, 0)
	flood_fill_from(g, g.rows - 1, g.cols - 1)

	best_pair: [2]int
	best_area: u64 = 0
	for i in 0 ..< len(input) {
		skip_i := skip_list_to_skip_pos(&skip_x, &skip_y, input[i])
		for j in i + 1 ..< len(input) {
			skip_j := skip_list_to_skip_pos(&skip_x, &skip_y, input[j])
			if perimeter_in_region(g, skip_i, skip_j) {
				area := rect_area(input[i], input[j])
				if area > best_area {
					best_pair = {i, j}
					best_area = area
				}
			}
		}
	}
	return best_area
}

perimeter_in_region :: proc(g: grid.Grid(Cell), corner1, corner2: Pos) -> bool {
	min_x := min(corner1.x, corner2.x)
	max_x := max(corner1.x, corner2.x)
	min_y := min(corner1.y, corner2.y)
	max_y := max(corner1.y, corner2.y)

	for x in min_x ..< max_x {
		if grid.get(g, int(min_y), int(x)) == Cell.Oob {
			return false
		}
	}
	for y in min_y ..< max_y {
		if grid.get(g, int(y), int(max_x)) == Cell.Oob {
			return false
		}
	}
	for x in min_x + 1 ..= max_x {
		if grid.get(g, int(max_y), int(x)) == Cell.Oob {
			return false
		}
	}
	for y in min_y + 1 ..= max_y {
		if grid.get(g, int(y), int(min_x)) == Cell.Oob {
			return false
		}
	}

	return true
}

fill_line :: proc(g: grid.Grid(Cell), a, b: Pos) {
	if a.x == b.x {
		for y in min(a.y, b.y) ..= max(a.y, b.y) {
			grid.set(g, int(y), int(a.x), Cell.Border)
		}
	} else if a.y == b.y {
		for x in min(a.x, b.x) ..= max(a.x, b.x) {
			grid.set(g, int(a.y), int(x), Cell.Border)
		}
	} else {
		assert(false)
	}
}

flood_fill_from :: proc(g: grid.Grid(Cell), start_row, start_col: int) {
	IntPos :: struct {
		row, col: int,
	}
	queue: [dynamic]IntPos

	try_fill_coord :: proc(g: grid.Grid(Cell), queue: ^[dynamic]IntPos, row, col: int) {
		if grid.get_or_default(g, row, col, Cell.Oob) == Cell.Unknown {
			grid.set(g, row, col, Cell.Oob)
			append(queue, IntPos{row, col})
		}
	}

	try_fill_coord(g, &queue, start_row, start_col)

	for len(queue) > 0 {
		pos := pop(&queue)
		try_fill_coord(g, &queue, pos.row, pos.col - 1)
		try_fill_coord(g, &queue, pos.row - 1, pos.col)
		try_fill_coord(g, &queue, pos.row, pos.col + 1)
		try_fill_coord(g, &queue, pos.row + 1, pos.col)
	}
}

print_grid :: proc(g: grid.Grid(Cell)) {
	for row in 0 ..< g.rows {
		fmt.printfln("%s", transmute([]u8)grid.get_row(g, row))
	}
}

create_skip_list_mappings :: proc(uniques: rbtree.Tree(u64, SkipNode)) -> SkipMapping {
	orig_to_skip := uniques
	skip_to_orig := make([dynamic]u64, 0, rbtree.len(&orig_to_skip))
	iter := rbtree.iterator(&orig_to_skip, .Forward)
	for node in rbtree.iterator_next(&iter) {
		node.value.skip_index = len(skip_to_orig)
		append(&skip_to_orig, node.key)
	}
	assert(slice.is_sorted(skip_to_orig[:]))
	assert(len(skip_to_orig) == rbtree.len(&orig_to_skip))
	return {orig_to_skip = orig_to_skip, skip_to_orig = skip_to_orig[:]}
}

skip_list_to_skip :: proc(sl: ^SkipMapping, orig: u64) -> u64 {
	node, ok := rbtree.find_value(&sl.orig_to_skip, orig)
	assert(ok)
	return u64(node.skip_index)
}

skip_list_to_orig :: proc(sl: ^SkipMapping, skip: u64) -> u64 {
	return sl.skip_to_orig[skip]
}

skip_list_to_skip_pos :: proc(sl_x, sl_y: ^SkipMapping, orig_pos: Pos) -> Pos {
	return {x = skip_list_to_skip(sl_x, orig_pos.x), y = skip_list_to_skip(sl_y, orig_pos.y)}
}
