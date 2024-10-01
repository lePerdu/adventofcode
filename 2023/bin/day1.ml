let is_digit c = '0' <= c && c <= '9'

let int_of_digit digit =
  if is_digit digit then Char.code digit - Char.code '0' else raise Not_found

let int_of_digits digit1 digit2 =
  (int_of_digit digit1 * 10) + int_of_digit digit2

let string_find_index s pred =
  let len = String.length s in
  let rec loop index =
    if index < len then if pred s.[index] then Some index else loop (index + 1)
    else None
  in
  loop 0

let string_find_rindex s pred =
  let len = String.length s in
  let rec loop index =
    if index >= 0 then if pred s.[index] then Some index else loop (index - 1)
    else None
  in
  loop (len - 1)

let extract_calibration1 line =
  match (string_find_index line is_digit, string_find_rindex line is_digit) with
  | Some first, Some last -> int_of_digits line.[first] line.[last]
  | _ -> raise Not_found

let part1 input =
  input |> List.map extract_calibration1 |> List.fold_left ( + ) 0

let digit_names =
  [
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
  ]

let string_starts_with_at ~prefix ~offset s =
  String.length prefix <= String.length s - offset
  && String.to_seqi prefix
     |> Seq.for_all (fun (index, ch) -> ch == s.[index + offset])

let match_digit index str =
  if is_digit str.[index] then Some (int_of_digit str.[index])
  else
    List.find_map
      (fun dig ->
        let name, v = dig in
        if string_starts_with_at ~prefix:name ~offset:index str then Some v
        else None)
      digit_names

let string_first_digit str =
  String.to_seqi str |> Seq.find_map (fun (index, _) -> match_digit index str)

let string_last_digit str =
  let len = String.length str in
  Seq.init len (fun index -> len - 1 - index)
  |> Seq.find_map (fun index -> match_digit index str)

let extract_calibration2 line =
  match (string_first_digit line, string_last_digit line) with
  | Some a, Some b -> (10 * a) + b
  | _ -> raise Not_found

let read_input input_file =
  In_channel.with_open_text input_file (fun ch ->
      let rec loop () =
        match In_channel.input_line ch with
        | Some line -> line :: loop ()
        | None -> []
      in
      loop ())

let part2 input =
  input |> List.map extract_calibration2 |> List.fold_left ( + ) 0
;;

let input = read_input "input/day1.txt" in
Printf.printf "Part1: %d\n" (part1 input);
Printf.printf "Part2: %d\n" (part2 input)
