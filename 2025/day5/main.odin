package day5

import "core:fmt"
import "core:os"
import "core:slice"
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
	fmt.printfln("Part 2: %d", part2(input))
}

Id :: u64

Range :: struct {
	start: Id,
	end:   Id,
}

range_includes :: #force_inline proc(r: Range, id: Id) -> bool {
	return r.start <= id && id <= r.end
}

Input :: struct {
	fresh_ranges:  []Range,
	available_ids: []Id,
}

parse_input :: proc(text: string) -> (Input, bool) {
	line_iter := text

	fresh_ranges: [dynamic]Range
	for line in strings.split_lines_iterator(&line_iter) {
		if line == "" {
			break
		}

		if range, ok := parse_range(line); ok {
			append(&fresh_ranges, range)
		} else {
			return {}, false
		}
	}

	available_ids: [dynamic]Id
	for line in strings.split_lines_iterator(&line_iter) {
		if id, ok := strconv.parse_u64(line, 10); ok {
			append(&available_ids, id)
		} else {
			return {}, false
		}
	}

	return {fresh_ranges = fresh_ranges[:], available_ids = available_ids[:]}, true
}

parse_range :: proc(text: string) -> (range: Range, ok: bool) {
	hyphen_index := strings.index_byte(text, '-')
	if hyphen_index == -1 {
		return
	}

	range.start, ok = strconv.parse_u64(text[:hyphen_index], 10)
	if !ok {
		return
	}
	range.end, ok = strconv.parse_u64(text[hyphen_index + 1:], 10)
	if !ok {
		return
	}
	return
}

part1 :: proc(input: Input) -> uint {
	n: uint
	for id in input.available_ids {
		if is_fresh(input, id) {
			n += 1
		}
	}
	return n
}

is_fresh :: proc(input: Input, id: Id) -> bool {
	for r in input.fresh_ranges {
		if range_includes(r, id) do return true
	}
	return false
}

part2 :: proc(input: Input) -> u64 {
	// Sorted, disjoint ranges
	disjoint_ranges: [dynamic]Range

	for new_range in input.fresh_ranges {
		// Merge with existing
		// TODO: Binary search?
		merged_index := -1
		for &existing, index in disjoint_ranges {
			if merged, ok := try_merge_ranges(existing, new_range); ok {
				existing = merged
				merged_index = index
				break
			}
		}

		if merged_index != -1 {
			disjoint_index: int
			for disjoint_index = merged_index + 1;
			    disjoint_index < len(disjoint_ranges);
			    disjoint_index += 1 {
				if merged, ok := try_merge_ranges(
					disjoint_ranges[merged_index],
					disjoint_ranges[disjoint_index],
				); ok {
					disjoint_ranges[merged_index] = merged
				} else {
					break
				}
			}

			remove_range(&disjoint_ranges, merged_index + 1, disjoint_index)
		} else {
			new_index, _ := slice.binary_search_by(
				disjoint_ranges[:],
				new_range.start,
				proc(r: Range, start: Id) -> slice.Ordering {
					if r.start < start {
						return .Less
					} else if r.start > start {
						return .Greater
					} else {
						return .Equal
					}
				},
			)
			inject_at(&disjoint_ranges, new_index, new_range)
		}
	}

	n: u64
	for r in disjoint_ranges {
		n += r.end - r.start + 1
	}
	return n
}

try_merge_ranges :: proc(r1, r2: Range) -> (Range, bool) {
	if r1.start <= r2.end && r2.start <= r1.end {
		return {start = min(r1.start, r2.start), end = max(r1.end, r2.end)}, true
	} else {
		return {}, false
	}
}
