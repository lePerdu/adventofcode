package day8

import "core:fmt"
import "core:mem"
import "core:os"
import "core:slice"
import "core:strconv"
import "core:strings"

PART1_N_JUNCTIONS :: 1000
PART1_N_LARGEST :: 3

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

Pos :: struct {
	x, y, z: i64,
}

square :: #force_inline proc(a: i64) -> u64 {
	return u64(a * a)
}

distance2 :: proc(a, b: Pos) -> u64 {
	return square(a.x - b.x) + square(a.y - b.y) + square(a.z - b.z)
}

Input :: []Pos

parse_input :: proc(text: string) -> (Input, bool) {
	positions: [dynamic]Pos
	line_iter := text
	for line in strings.split_lines_iterator(&line_iter) {
		pos, ok := parse_position(line)
		if !ok {
			return nil, false
		}
		append(&positions, pos)
	}
	return positions[:], true
}

parse_position :: proc(line: string) -> (Pos, bool) {
	pos: Pos
	comma_iter := line

	if x_str, ok := strings.split_by_byte_iterator(&comma_iter, ','); ok {
		pos.x, ok = strconv.parse_i64(x_str, 10)
		if !ok do return {}, false
	} else {
		return {}, false
	}
	if y_str, ok := strings.split_by_byte_iterator(&comma_iter, ','); ok {
		pos.y, ok = strconv.parse_i64(y_str, 10)
		if !ok do return {}, false
	} else {
		return {}, false
	}
	if z_str, ok := strings.split_by_byte_iterator(&comma_iter, ','); ok {
		pos.z, ok = strconv.parse_i64(z_str, 10)
		if !ok do return {}, false
	} else {
		return {}, false
	}

	if len(comma_iter) > 0 {
		return {}, false
	}
	return pos, true
}

part1 :: proc(input: Input) -> u64 {
	all_junctions := calc_shortest_junctions(input)

	disjoint_set := disjoint_set_make(len(input))
	for j in all_junctions[:PART1_N_JUNCTIONS] {
		disjoint_set_merge(disjoint_set, j.index1, j.index2)
	}

	set_sizes := make([]u32, len(input))
	for _, index in disjoint_set {
		rep := disjoint_set_find(disjoint_set, index)
		set_sizes[rep.parent] += 1
	}

	slice.sort(set_sizes)
	// Take 3 largest
	product: u64 = 1
	for i in 0 ..< PART1_N_LARGEST {
		size := set_sizes[len(set_sizes) - 1 - i]
		product *= u64(size)
	}

	return product
}

Junction :: struct {
	index1, index2: int,
	dist_metric:    u64,
}

calc_shortest_junctions :: proc(input: Input) -> []Junction {
	n_pairs := len(input) * (len(input) - 1) / 2
	all_junctions := make([dynamic]Junction, 0, n_pairs)
	for index1 := 0; index1 < len(input); index1 += 1 {
		for index2 := index1 + 1; index2 < len(input); index2 += 1 {
			append(
				&all_junctions,
				Junction {
					index1 = index1,
					index2 = index2,
					dist_metric = distance2(input[index1], input[index2]),
				},
			)
		}
	}

	slice.sort_by(all_junctions[:], proc(j1, j2: Junction) -> bool {
		return j1.dist_metric < j2.dist_metric
	})

	return all_junctions[:]
}

Id :: int

Node :: struct {
	parent: Id,
	size:   u32,
}

DisjointSet :: distinct []Node

disjoint_set_make :: proc(n: int) -> (DisjointSet, mem.Allocator_Error) #optional_allocator_error {
	set, err := make(DisjointSet, n)
	if err != nil {
		return nil, err
	}

	for &node, index in set {
		node = Node {
			parent = index,
			size   = 1,
		}
	}
	return set, nil
}

disjoint_set_find :: proc(set: DisjointSet, x: Id) -> ^Node {
	x := x
	for set[x].parent != x {
		x, set[x].parent = set[x].parent, set[set[x].parent].parent
		x = set[x].parent
	}
	return &set[x]
}

disjoint_set_merge :: proc(set: DisjointSet, x, y: Id) -> ^Node {
	x_rep := disjoint_set_find(set, x)
	y_rep := disjoint_set_find(set, y)
	if x_rep == y_rep {
		return x_rep
	}

	if x_rep.size >= y_rep.size {
		x_rep.size += y_rep.size
		y_rep.parent = x_rep.parent
		return x_rep
	} else {
		y_rep.size += x_rep.size
		x_rep.parent = y_rep.parent
		return y_rep
	}
}

part2 :: proc(input: Input) -> u64 {
	all_junctions := calc_shortest_junctions(input)

	disjoint_set := disjoint_set_make(len(input))
	for j in all_junctions {
		merged := disjoint_set_merge(disjoint_set, j.index1, j.index2)
		// All connected
		if int(merged.size) == len(input) {
			return u64(input[j.index1].x * input[j.index2].x)
		}
	}
	return 0
}
