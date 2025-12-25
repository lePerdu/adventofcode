package common

import "core:strconv"
import "core:strings"

Vec2 :: struct($T: typeid) {
	x, y: T,
}

parse_vec2_u64 :: proc(text: string, sep: u8) -> (vec: Vec2(u64), ok: bool) {
	comma := strings.index_byte(text, sep)
	if comma == -1 {
		return {}, false
	}

	vec.x, ok = strconv.parse_u64(text[:comma], 10)
	if !ok do return
	vec.y, ok = strconv.parse_u64(text[comma + 1:], 10)
	if !ok do return
	return
}

parse_lines :: proc(text: string, parser: proc(line: string) -> ($T, bool)) -> ([]T, bool) {
	result: [dynamic]Vec2(u64)
	iter := text
	for line in strings.split_lines_iterator(&iter) {
		vec, ok := parser(line)
		if !ok do return nil, false
		append(&result, vec)
	}
	return result[:], true
}
