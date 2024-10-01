let run input_file read_input part1 part2 =
  let input = read_input input_file in
  Printf.printf "Part1: %d\n%!" (part1 input);
  Printf.printf "Part2: %d\n%!" (part2 input)
