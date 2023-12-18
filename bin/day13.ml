type tile = Ash | Rocks

module Grid = Advent.Grid.Make (struct
  type t = tile

  let of_char = function '.' -> Some Ash | '#' -> Some Rocks | _ -> None
  let to_char = function Ash -> '.' | Rocks -> '#'
end)

let read_input file_name =
  let rec loop ch =
    try
      let next = Grid.input ch in
      next :: loop ch
    with End_of_file -> []
  in
  In_channel.with_open_text file_name loop

let row_diff grid row1 row2 =
  Seq.init (Grid.cols grid) Fun.id
  |> Seq.filter (fun col ->
         Grid.get grid { row = row1; col } != Grid.get grid { row = row2; col })
  |> Seq.length

let horiz_reflect_at ~precision grid row =
  let radius = min row (Grid.rows grid - row) in
  let total_diff =
    Seq.init radius (fun x -> (row - x - 1, row + x))
    |> Seq.map (fun (a, b) -> row_diff grid a b)
    |> Seq.fold_left ( + ) 0
  in
  total_diff == precision

let find_horiz_reflection ~precision grid =
  let found =
    Seq.ints 1
    |> Seq.take (Grid.rows grid - 1)
    |> Seq.filter (horiz_reflect_at ~precision grid)
    |> List.of_seq
  in
  match found with
  | [] -> None
  | [ single ] -> Some single
  | _ -> failwith "Multiple horizontal reflections found"

let col_diff grid col1 col2 =
  Seq.init (Grid.rows grid) Fun.id
  |> Seq.filter (fun row ->
         Grid.get grid { col = col1; row } != Grid.get grid { col = col2; row })
  |> Seq.length

let vert_reflect_at ~precision grid col =
  let radius = min col (Grid.cols grid - col) in
  let total_diff =
    Seq.init radius (fun x -> (col - x - 1, col + x))
    |> Seq.map (fun (a, b) -> col_diff grid a b)
    |> Seq.fold_left ( + ) 0
  in
  total_diff == precision

let find_vert_reflection ~precision grid =
  let found =
    Seq.ints 1
    |> Seq.take (Grid.cols grid - 1)
    |> Seq.filter (vert_reflect_at ~precision grid)
    |> List.of_seq
  in
  match found with
  | [] -> None
  | [ single ] -> Some single
  | _ -> failwith "Multiple vertical reflections found"

type reflection = Horiz of int | Vert of int

let find_reflection ~precision grid =
  match
    (find_horiz_reflection ~precision grid, find_vert_reflection ~precision grid)
  with
  | None, None -> failwith "No reflection found"
  | Some r, None -> Horiz r
  | None, Some c -> Vert c
  | Some _, Some _ -> failwith "Horizontal and vertical reflection found"

let sum_reflections ~precision input =
  List.to_seq input
  |> Seq.map (find_reflection ~precision)
  |> Seq.map (function Horiz r -> 100 * r | Vert c -> c)
  |> Seq.fold_left ( + ) 0

let part1 = sum_reflections ~precision:0
let part2 = sum_reflections ~precision:1
let () = Advent.Main.run "input/day13.txt" read_input part1 part2
