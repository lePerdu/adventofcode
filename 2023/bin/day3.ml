type schematic = { data : string Array.t }
type coord = { row : int; col : int }
type part_number = { _coord : coord; number : int }

let schematic_rows s = Array.length s.data
let schematic_cols s = String.length s.data.(0)
let schematic_get s coord = s.data.(coord.row).[coord.col]
let cell_is_empty c = c == '.'
let cell_is_digit c = '0' <= c && c <= '9'
let cell_is_symbol c = (not (cell_is_empty c)) && not (cell_is_digit c)
let cell_is_gear c = c == '*'

let read_input file =
  In_channel.with_open_text file (fun ch ->
      let rec loop opt_line_size =
        match In_channel.input_line ch with
        | Some next_line -> (
            match opt_line_size with
            | Some req_size ->
                if String.length next_line == req_size then
                  next_line :: loop opt_line_size
                else raise (Failure "Inconsistent line length")
            | None -> next_line :: loop (Some (String.length next_line)))
        | None -> []
      in
      let line_list = loop None in
      { data = Array.of_list line_list })

let string_scan_left s pred offset =
  let rec scan index =
    if index >= 0 && pred s.[index] then scan (index - 1) else index + 1
  in
  scan (offset - 1)

let string_scan_right s pred offset =
  let len = String.length s in
  let rec scan index =
    if index < len && pred s.[index] then scan (index + 1) else index
  in
  scan (offset + 1)

let string_scan_bounds s pred offset =
  (string_scan_left s pred offset, string_scan_right s pred offset)

let expand_part_number schema coord =
  if cell_is_digit (schematic_get schema coord) then
    let row = schema.data.(coord.row) in
    let start_col, end_col = string_scan_bounds row cell_is_digit coord.col in
    let num_len = end_col - start_col in
    let part_num = int_of_string (String.sub row start_col num_len) in
    Some { _coord = { coord with col = start_col }; number = part_num }
  else None

let schematic_in_bounds schema { row; col } =
  0 <= row
  && row < schematic_rows schema
  && 0 <= col
  && col < schematic_cols schema

let coords_around schema { row = r; col = c } =
  [
    { row = r; col = c + 1 };
    { row = r - 1; col = c + 1 };
    { row = r - 1; col = c };
    { row = r - 1; col = c - 1 };
    { row = r; col = c - 1 };
    { row = r + 1; col = c - 1 };
    { row = r + 1; col = c };
    { row = r + 1; col = c + 1 };
  ]
  |> List.filter (schematic_in_bounds schema)

let uniq_cons x xs =
  match xs with [] -> [ x ] | y :: _ -> if x = y then xs else x :: xs

let list_uniq xs = List.fold_right uniq_cons xs []

let scan_part_numbers_around schema coord =
  coords_around schema coord
  |> List.filter_map (expand_part_number schema)
  (* Dedup at the "around symbol" level, but it isn't necessary to do so
     globally. I.e. there are no part numbers surrounded by more than 1 symbol.
  *)
  |> list_uniq

let schematic_coords_seq schema =
  Seq.product
    (Seq.init (schematic_rows schema) Fun.id)
    (Seq.init (schematic_cols schema) Fun.id)
  |> Seq.map (fun (row, col) -> { row; col })

let schematic_symbol_coords_seq schema =
  schematic_coords_seq schema
  |> Seq.filter (fun c -> cell_is_symbol (schematic_get schema c))

let scan_part_numbers schema =
  schematic_symbol_coords_seq schema
  |> Seq.flat_map (fun coord ->
         scan_part_numbers_around schema coord |> List.to_seq)

let part1 schema =
  scan_part_numbers schema
  |> Seq.map (fun p -> p.number)
  |> Seq.fold_left ( + ) 0

let schematic_gear_symbols_seq schema =
  schematic_coords_seq schema
  |> Seq.filter (fun c -> cell_is_gear (schematic_get schema c))

type gear = { p1 : part_number; p2 : part_number }

let schematic_gears_seq schema =
  schematic_gear_symbols_seq schema
  |> Seq.map (scan_part_numbers_around schema)
  |> Seq.filter_map (function [ p1; p2 ] -> Some { p1; p2 } | _ -> None)

let gear_ratio { p1; p2 } = p1.number * p2.number

let part2 input =
  schematic_gears_seq input |> Seq.map gear_ratio |> Seq.fold_left ( + ) 0
;;

let input = read_input "input/day3.txt" in
Printf.printf "Part1: %d\n" (part1 input);
Printf.printf "Part2: %d\n" (part2 input)
