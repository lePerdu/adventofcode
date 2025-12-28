package grid

import "base:builtin"
import "base:intrinsics"
import "core:fmt"
import "core:mem"
import "core:slice"

Grid :: struct($T: typeid) {
	data:   [^]T,
	rows:   int,
	cols:   int,
	stride: int,
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
	grid.data, err = builtin.make([^]T, rows * cols, allocator)
	if err != nil {
		return
	}
	grid.rows = rows
	grid.cols = cols
	grid.stride = cols
	return
}

fill :: proc(grid: Grid($T), val: T) {
	for row in 0 ..< grid.rows {
		for col in 0 ..< grid.cols {
			set(grid, row, col, val)
		}
	}
}

in_bounds :: proc(grid: Grid($T), row: int, col: int) -> bool {
	return 0 <= row && row < grid.rows && 0 <= col && col < grid.cols
}

get :: #force_inline proc(grid: Grid($T), row: int, col: int) -> T {
	return get_ptr(grid, row, col)^
}

get_ptr :: #force_inline proc(grid: Grid($T), row: int, col: int) -> ^T {
	assert(row >= 0)
	assert(col >= 0)
	assert(row < grid.rows)
	assert(col < grid.cols)
	return &grid.data[row * grid.stride + col]
}

get_or_default :: proc(grid: Grid($T), row: int, col: int, default: T) -> T {
	if 0 <= row && row < grid.rows && 0 <= col && col < grid.cols {
		return get(grid, row, col)
	} else {
		return default
	}
}

get_row :: #force_inline proc(grid: Grid($T), row: int) -> []T {
	assert(row >= 0)
	assert(row < grid.rows)
	return grid.data[row * grid.stride:][:grid.cols]
}

set :: #force_inline proc(grid: Grid($T), row: int, col: int, val: T) {
	get_ptr(grid, row, col)^ = val
}

copy :: proc(dst, src: Grid($T)) {
	assert(dst.rows == src.rows)
	assert(dst.cols == src.cols)
	if dst.stride == dst.rows && src.stride == dst.rows {
		data_len := dst.rows * dst.cols
		builtin.copy(dst.data[:data_len], src.data[:data_len])
	} else {
		for row in 0 ..< dst.rows {
			builtin.copy(get_row(dst, row), get_row(src, row))
		}
	}
}

sub :: proc(grid: Grid($T), row_start, col_start, row_end, col_end: int) -> Grid(T) {
	assert(row_start >= 0)
	assert(col_start >= 0)
	assert(row_start <= row_end)
	assert(col_start <= col_end)
	assert(row_end <= grid.rows)
	assert(col_end <= grid.cols)

	rows := row_end - row_start
	cols := col_end - col_start
	flat_start := row_start * grid.stride + col_start
	return {data = grid.data[flat_start:], rows = rows, cols = cols, stride = grid.stride}
}

clone :: proc(
	grid: Grid($T),
	allocator := context.allocator,
) -> (
	cloned: Grid(T),
	err: mem.Allocator_Error,
) #optional_allocator_error {
	cloned, err = make(T, grid.rows, grid.cols)
	if err != nil {
		return
	}
	copy(cloned, grid)
	return
}

clone_transposed :: proc(
	grid: Grid($T),
	allocator := context.allocator,
) -> (
	transposed: Grid(T),
	err: mem.Allocator_Error,
) #optional_allocator_error {
	transposed, err = make(T, grid.cols, grid.rows, allocator)
	if err != nil do return

	for orig_row in 0 ..< grid.rows {
		for orig_col in 0 ..< grid.cols {
			set(transposed, orig_col, orig_row, get(grid, orig_row, orig_col))
		}
	}
	return
}

row_swap :: proc(grid: Grid($T), dst_row, src_row: int) {
	slice.swap_with_slice(get_row(grid, dst_row), get_row(grid, src_row))
}

row_scale :: proc(grid: Grid($T), row: int, scalar: T) where intrinsics.type_is_numeric(T) {
	for v in get_row(grid, row) {
		v *= scalar
	}
}

row_add_scale :: proc(
	grid: Grid($T),
	dst_row, src_row: int,
	scalar: T,
) where intrinsics.type_is_numeric(T) {
	row_scale_add_scale(grid, dst_row, 1, src_row, scalar)
}

row_scale_add_scale :: proc(
	grid: Grid($T),
	dst_row: int,
	dst_scalar: T,
	src_row: int,
	src_scalar: T,
) where intrinsics.type_is_numeric(T) {
	dst_row := get_row(grid, dst_row)
	src_row := get_row(grid, src_row)
	for col in 0 ..< grid.cols {
		dst_row[col] = dst_row[col] * dst_scalar + src_row[col] * src_scalar
	}
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

@(require_results)
append_row_slice :: proc(builder: ^Builder($T), row: []T) -> GridBuilderError {
	if builder.cols == 0 {
		builder.cols = len(row)
	} else if len(row) != builder.cols {
		return GridAppendError.Invalid_Row_Len
	}

	_, err := builtin.append(&builder.data, ..row)
	return err
}

/// Specialized version for appending a string as a row
@(require_results)
append_row_string :: proc(builder: ^Builder(u8), row: string) -> GridBuilderError {
	return append_row_slice(builder, transmute([]u8)row)
}

append :: proc {
	append_row_slice,
	append_row_string,
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
	return {
		data = raw_data(builder.data),
		rows = len(builder.data) / builder.cols,
		cols = builder.cols,
		stride = builder.cols,
	}
}
