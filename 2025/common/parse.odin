package common

import "base:intrinsics"
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
	result: [dynamic]T
	iter := text
	for line in strings.split_lines_iterator(&iter) {
		vec, ok := parser(line)
		if !ok do return nil, false
		append(&result, vec)
	}
	return result[:], true
}

parse_enum_value :: proc($E: typeid, val: $T) -> (E, bool) where intrinsics.type_is_enum(E) {
	for e in E {
		if T(e) == val {
			return e, true
		}
	}
	return {}, false
}

parse_enum_slice :: proc($E: typeid, s: []$T) -> ([]E, bool) where intrinsics.type_is_enum(E) {
	for v in s {
		if _, ok := parse_enum_value(E, v); !ok {
			return nil, false
		}
	}
	return transmute([]E)s, true
}

parse_enum_string :: proc($E: typeid, s: string) -> ([]E, bool) where intrinsics.type_is_enum(E) {
	return parse_enum_slice(E, transmute([]u8)s)
}

parse_enum :: proc {
	parse_enum_value,
	parse_enum_slice,
	parse_enum_string,
}
