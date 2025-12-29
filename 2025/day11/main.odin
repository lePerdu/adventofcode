package day11

import "core:fmt"
import "core:os"
import "core:slice"
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
	fmt.printfln("Part 2: %d", part2(input))
}

Input :: struct {
	links:       []Link,
	id_to_index: map[Id]int,
	index_to_id: []Id,
}

Link :: struct {
	src: int,
	dst: []int,
}

Id :: distinct [3]u8

parse_input :: proc(text: string) -> (Input, bool) {
	unordered_links := make([dynamic]Link)
	id_to_index := make(map[Id]int)
	index_to_id := make([dynamic]Id)

	line_iter := text
	for line in strings.split_lines_iterator(&line_iter) {
		cursor := line

		src_index: int
		if src_str, ok := strings.split_by_byte_iterator(&cursor, ':'); ok {
			if src_id, ok := parse_id(src_str); ok {
				src_index = get_or_add_id_index(&id_to_index, &index_to_id, src_id)
			} else {
				return {}, false
			}
		} else {
			fmt.fprintfln(os.stderr, "invalid link: '%s'", line)
			return {}, false
		}

		dsts := make([dynamic]int)
		for dst_str in strings.split_by_byte_iterator(&cursor, ' ') {
			// Allow flexible spacing
			if len(dst_str) == 0 do continue

			dst_index: int
			if dst_id, ok := parse_id(dst_str); ok {
				dst_index = get_or_add_id_index(&id_to_index, &index_to_id, dst_id)
			} else {
				return {}, false
			}

			append(&dsts, dst_index)
		}

		append(&unordered_links, Link{src = src_index, dst = dsts[:]})
	}

	// Links indexed by node index
	ordered_links := make([]Link, len(index_to_id))
	for link in unordered_links {
		ordered_links[link.src] = link
	}

	return {links = ordered_links, id_to_index = id_to_index, index_to_id = index_to_id[:]}, true
}

parse_id :: proc(s: string) -> (Id, bool) {
	if len(s) == 3 {
		return Id{s[0], s[1], s[2]}, true
	} else {
		fmt.fprintfln(os.stderr, "invalid ID: '%s'", s)
		return 0, false
	}
}

get_or_add_id_index :: proc(id_to_index: ^map[Id]int, index_to_id: ^[dynamic]Id, id: Id) -> int {
	if index, found := id_to_index[id]; found {
		return index
	} else {
		index := len(index_to_id)
		append(index_to_id, id)
		id_to_index[id] = index
		return index
	}
}

count_paths :: proc(graph: []Link, start, end: int, exclude: ..int) -> u64 {
	paths_to_end := make([]i64, len(graph))
	slice.fill(paths_to_end, -1)
	paths_to_end[end] = 1
	for node in exclude {
		paths_to_end[node] = 0
	}
	return u64(scan_paths(graph, paths_to_end, start, end))
}

scan_paths :: proc(graph: []Link, paths_to_end: []i64, start, end: int) -> i64 {
	if paths_to_end[start] >= 0 {
		return paths_to_end[start]
	}

	total: i64
	for dst in graph[start].dst {
		total += scan_paths(graph, paths_to_end, dst, end)
		paths_to_end[start] = total
	}
	return total
}

part1 :: proc(input: Input) -> u64 {
	you := input.id_to_index["you"]
	out := input.id_to_index["out"]
	return count_paths(input.links, you, out)
}

part2 :: proc(input: Input) -> u64 {
	svr := input.id_to_index["svr"]
	out := input.id_to_index["out"]
	dac := input.id_to_index["dac"]
	fft := input.id_to_index["fft"]

	srv_to_dac := count_paths(input.links, svr, dac, exclude = []int{fft, out})
	dac_to_fft := count_paths(input.links, dac, fft, exclude = []int{svr, out})
	fft_to_out := count_paths(input.links, fft, out, exclude = []int{svr, dac})
	dac_then_fft := srv_to_dac * dac_to_fft * fft_to_out

	srv_to_fft := count_paths(input.links, svr, fft, exclude = []int{dac, out})
	fft_to_dac := count_paths(input.links, fft, dac, exclude = []int{svr, out})
	dac_to_out := count_paths(input.links, dac, out, exclude = []int{svr, fft})
	fft_then_dac := srv_to_fft * fft_to_dac * dac_to_out

	return dac_then_fft + fft_then_dac
}
