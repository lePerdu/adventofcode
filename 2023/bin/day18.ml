module Coord = Advent.Coord

type color = int
type dig_step = { dir : Coord.dir; dist : int; color : color }
type dig_plan = dig_step list

let dir_of_string : string -> Coord.dir = function
  | "U" -> Up
  | "D" -> Down
  | "L" -> Left
  | "R" -> Right
  | s -> failwith @@ Printf.sprintf "Invalid direction: %S" s

let color_regexp =
  let digit = "[0-9a-f]" in
  let pair = digit ^ digit in
  Str.regexp @@ "(#\\(" ^ pair ^ pair ^ pair ^ "\\))"

let int_of_hex_digit c =
  if '0' <= c && c <= '9' then Char.code c - Char.code '0'
  else if 'a' <= c && c <= 'f' then Char.code c - Char.code 'a' + 10
  else failwith @@ Printf.sprintf "Invalid hex digit: %C" c

let int_of_hex_string str =
  String.to_seq str |> Seq.map int_of_hex_digit
  |> Seq.fold_left (fun acc d -> (acc * 16) + d) 0

let parse_color color_desc =
  if Str.string_match color_regexp color_desc 0 then
    Str.matched_group 1 color_desc |> int_of_hex_string
  else failwith @@ Printf.sprintf "Invalid color: %S" color_desc

let parse_dig_step line =
  match String.split_on_char ' ' line with
  | [ dir_str; dist_str; color_str ] ->
      {
        dir = dir_of_string dir_str;
        dist = int_of_string dist_str;
        color = parse_color color_str;
      }
  | _ -> failwith @@ Printf.sprintf "Invalid dig step: %S" line

let read_input file_name =
  In_channel.with_open_text file_name (fun ch ->
      In_channel.input_lines ch |> List.map parse_dig_step)

type line_seg = { start : Coord.t; dir : Coord.dir; dist : int }

let line_seg_end { start; dir; dist } = Coord.add_dir_n start dir dist

let validate_path path =
  if
    not
      (Coord.equal
         (line_seg_end path.(Array.length path - 1))
         { row = 0; col = 0 })
  then failwith "Path does not end at origin"
  else path

let trench_path_segs plan =
  let rec gen pos (steps : dig_plan) () =
    match steps with
    | [] -> Seq.Nil
    | next :: rest ->
        let seg = { start = pos; dir = next.dir; dist = next.dist } in
        Seq.Cons (seg, gen (line_seg_end seg) rest)
  in
  let origin : Coord.t = { row = 0; col = 0 } in
  let path_segs = gen origin plan |> Array.of_seq in
  validate_path path_segs

let dir_cw_turn (a : Coord.dir) (b : Coord.dir) =
  match (a, b) with
  | Up, Left -> -1
  | Up, Right -> 1
  | Down, Left -> 1
  | Down, Right -> -1
  | Left, Up -> 1
  | Left, Down -> -1
  | Right, Up -> -1
  | Right, Down -> 1
  | _ -> if a == b then 0 else failwith "Can't handle reversing direction"

type path_dir = Clockwise | CounterClockwise

let trench_path_dir path =
  let seq = Array.to_seq path in
  let total_cw_turns =
    Seq.zip seq (seq |> Seq.cycle |> Seq.drop 1)
    |> Seq.map (fun (prev, cur) -> dir_cw_turn prev.dir cur.dir)
    |> Seq.fold_left ( + ) 0
  in
  match total_cw_turns with
  | 4 -> Clockwise
  | -4 -> CounterClockwise
  | _ -> failwith @@ Printf.sprintf "Invalid number of turns: %d" total_cw_turns

let inside_dir (path_dir : path_dir) (cur_dir : Coord.dir) : Coord.dir =
  match path_dir with
  | Clockwise -> (
      match cur_dir with
      | Up -> Right
      | Down -> Left
      | Left -> Up
      | Right -> Down)
  | CounterClockwise -> (
      match cur_dir with
      | Up -> Left
      | Down -> Right
      | Left -> Down
      | Right -> Up)

module RawGrid = Advent.Grid
module IntSet = Set.Make (Int)

let extract_skip_values coord_part path =
  Array.to_seq path
  |> Seq.flat_map (fun step -> List.to_seq [ step.start; line_seg_end step ])
  |> Seq.map coord_part
  (* Go through a set to sort and de-duplicate *)
  |> IntSet.of_seq
  |> IntSet.to_seq |> Array.of_seq

let trench_path_skip_rows = extract_skip_values (fun c -> c.row)
let trench_path_skip_cols = extract_skip_values (fun c -> c.col)

let binary_search ~compare x arr =
  let rec loop low high =
    if low == high then
      if compare x arr.(low) == 0 then low else raise Not_found
    else
      let mid = (high + low) / 2 in
      let cmp = compare x arr.(mid) in
      if cmp < 0 then loop low (mid - 1)
      else if cmp > 0 then loop (mid + 1) high
      else mid
  in
  loop 0 (Array.length arr - 1)

module DigGrid = struct
  type t = {
    data : bool RawGrid.t;
    row_offsets : int array;
    col_offsets : int array;
  }

  let make row_offsets col_offsets init =
    {
      data =
        RawGrid.make
          ((Array.length row_offsets * 2) - 1)
          ((Array.length col_offsets * 2) - 1)
          init;
      row_offsets;
      col_offsets;
    }

  let raw g = g.data

  let resolve_row g row =
    binary_search ~compare:Int.compare row g.row_offsets * 2

  let resolve_col g col =
    binary_search ~compare:Int.compare col g.col_offsets * 2

  let resolve_coord g (c : Coord.t) : Coord.t =
    { row = resolve_row g c.row; col = resolve_col g c.col }

  let compute_width offsets coord_val =
    if coord_val mod 2 == 0 then 1
    else
      let index = coord_val / 2 in
      offsets.(index + 1) - offsets.(index) - 1

  let row_width g = compute_width g.row_offsets
  let col_width g = compute_width g.col_offsets

  let coord_volume g ({ row; col } : Coord.t) =
    row_width g row * col_width g col

  let filled_volume g =
    RawGrid.to_seqi g.data
    |> Seq.filter (fun (_, v) -> v)
    |> Seq.map (fun (pos, _) -> coord_volume g pos)
    |> Seq.fold_left ( + ) 0

  let resolve_line_seq g line =
    let a = resolve_coord g line.start in
    let b = resolve_coord g (line_seg_end line) in
    match line.dir with
    | Up ->
        Seq.init (a.row - b.row) (fun drow : Coord.t ->
            { row = a.row - 1 - drow; col = a.col })
    | Down ->
        Seq.init (b.row - a.row) (fun drow : Coord.t ->
            { row = a.row + 1 + drow; col = a.col })
    | Left ->
        Seq.init (a.col - b.col) (fun dcol : Coord.t ->
            { row = a.row; col = a.col - 1 - dcol })
    | Right ->
        Seq.init (b.col - a.col) (fun dcol : Coord.t ->
            { row = a.row; col = a.col + 1 + dcol })

  let draw_line g line =
    resolve_line_seq g line |> Seq.iter (fun pos -> RawGrid.set g.data pos true)
end

let draw_trench grid path = Array.iter (DigGrid.draw_line grid) path

let expand_fill grid pos =
  let queue = Stack.create () in
  let try_push pos =
    if RawGrid.in_bounds grid pos && not (RawGrid.get grid pos) then (
      RawGrid.set grid pos true;
      Stack.push pos queue)
  in
  let rec loop () =
    match Stack.pop_opt queue with
    | Some next ->
        List.iter try_push (Coord.adjacent next);
        loop ()
    | None -> ()
  in
  try_push pos;
  loop ()

let fill_trench grid path =
  let path_dir = trench_path_dir path in
  Array.iter
    (fun seg ->
      DigGrid.resolve_line_seq grid seg
      |> Seq.iter (fun pos ->
             let inside_pos = Coord.add_dir pos (inside_dir path_dir seg.dir) in
             expand_fill (DigGrid.raw grid) inside_pos))
    path

let part1 input =
  let path = trench_path_segs input in
  let grid =
    DigGrid.make (trench_path_skip_rows path) (trench_path_skip_cols path) false
  in
  draw_trench grid path;
  fill_trench grid path;
  DigGrid.filled_volume grid

let dig_step_of_color color =
  let dist = color / 16 in
  let dir : Coord.dir =
    match color mod 16 with
    | 0 -> Right
    | 1 -> Down
    | 2 -> Left
    | 3 -> Up
    | _ -> failwith "Invalid direction from color"
  in
  { dir; dist; color }

let part2 input =
  let input' = List.map (fun s -> dig_step_of_color s.color) input in
  part1 input'

let () = Advent.Main.run "input/day18.txt" read_input part1 part2
