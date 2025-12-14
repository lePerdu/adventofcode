package grid

import "base:builtin"
import "core:mem"
import "core:slice"

Grid :: struct($T: typeid) {
	data: []T,
	rows: int,
	cols: int,
}

make :: proc(
	$T: typeid,
	rows: int,
	cols: int,
	allocator := context.allocator,
) -> (
	grid: Grid(T),
	err: mem.Allocator_Error,
) #optional_allocator_error {
	grid.data, err = builtin.make([]T, rows * cols, allocator)
	if err != nil {
		return
	}
	grid.rows = rows
	grid.cols = cols
	return
}

fill :: proc(grid: Grid($T), val: T) {
	for row in 0 ..< grid.rows {
		for col in 0 ..< grid.cols {
			set(grid, row, col, val)
		}
	}
}

get :: #force_inline proc(grid: Grid($T), row: int, col: int) -> T {
	assert(row < grid.rows)
	assert(col < grid.cols)
	return grid.data[row * grid.cols + col]
}

get_or_default :: proc(grid: Grid($T), row: int, col: int, default: T) -> T {
	if 0 <= row && row < grid.rows && 0 <= col && col < grid.cols {
		return get(grid, row, col)
	} else {
		return default
	}
}

set :: #force_inline proc(grid: Grid($T), row: int, col: int, val: T) {
	assert(row < grid.rows)
	assert(col < grid.cols)
	grid.data[row * grid.cols + col] = val
}

clone :: proc(grid: Grid($T), allocator := context.allocator) -> Grid(T) {
	return {data = slice.clone(grid.data, allocator), rows = grid.rows, cols = grid.cols}
}

Builder :: struct($T: typeid) {
	data: [dynamic]T,
	cols: int,
}

builder_init :: proc(builder: ^Builder($T), allocator := context.allocator) {
	builder.data = builtin.make([dynamic]T, 0, 0, allocator)
	builder.cols = 0
}

GridAppendError :: enum {
	None = 0,
	Invalid_Row_Len,
}

GridBuilderError :: union #shared_nil {
	mem.Allocator_Error,
	GridAppendError,
}

append :: proc(builder: ^Builder($T), row: []T) -> GridBuilderError {
	if builder.cols == 0 {
		builder.cols = len(row)
	} else if len(row) != builder.cols {
		return GridAppendError.Invalid_Row_Len
	}

	return append(&builder.data, ..row)
}

add_row :: proc(builder: ^Builder($T), cols: int) -> (new_row: []T, err: GridBuilderError) {
	if builder.cols == 0 {
		builder.cols = cols
	} else if cols != builder.cols {
		return nil, GridAppendError.Invalid_Row_Len
	}

	old_len := len(builder.data)
	if err := resize(&builder.data, old_len + cols); err != nil {
		return nil, err
	}

	return builder.data[old_len:], nil
}

build :: proc(builder: Builder($T)) -> Grid(T) {
	if len(builder.data) == 0 {
		return {}
	}
	assert(builder.cols > 0)
	assert(len(builder.data) % builder.cols == 0)
	return {data = builder.data[:], rows = len(builder.data) / builder.cols, cols = builder.cols}
}
