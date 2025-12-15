package grid

import "core:slice"
import "core:strings"

// Parse a grid of ASCII and alias the string into a `Grid(u8)`. Each line must be of the same length.
//
// Note: The returned grid aliases the string, so it should not be modified.
parse_ascii_grid :: proc(text: string) -> (grid: Grid(u8), ok: bool) {
	// Check that all lines are of the same length
	line_len := -1
	line_count := 0
	line_iter := text
	for line in strings.split_lines_iterator(&line_iter) {
		if line_len == -1 {
			line_len = len(line)
		} else if len(line) != line_len {
			return {}, false
		}
		line_count += 1
	}
	if line_count == 0 {
		// Valid empty grid
		return {}, true
	}

	// Stride includes '\n' at each line
	return {data = raw_data(text), rows = line_count, cols = line_len, stride = line_len + 1}, true
}
