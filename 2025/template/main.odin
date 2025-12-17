package dayN

import "core:fmt"
import "core:os"

main :: proc() {
	data, err := os.read_entire_file_or_err("example.txt")
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

Input :: string

parse_input :: proc(text: string) -> (Input, bool) {
	return {}, false
}

part1 :: proc(input: Input) -> u64 {
	return 0
}

part2 :: proc(input: Input) -> u64 {
	return 0
}
