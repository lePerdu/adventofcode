type coord = Advent.Grid.coord
type cell = Space | Galaxy

module Grid = Advent.Grid.Make (struct
  type t = cell

  let of_char = function '.' -> Some Space | '#' -> Some Galaxy | _ -> None
  let to_char = function Space -> '.' | Galaxy -> '#'
end)

let read_input file_name = In_channel.with_open_text file_name Grid.input

let galaxy_coords grid =
  Grid.to_seqi grid
  |> Seq.filter (fun (_, cell) -> cell == Galaxy)
  |> Seq.map (fun (coord, _) -> coord)
  |> List.of_seq

let empty_rows grid =
  Seq.init (Grid.rows grid) Fun.id
  |> Seq.filter (fun row ->
         Seq.init (Grid.cols grid) Fun.id
         |> Seq.for_all (fun col -> Grid.get grid { row; col } == Space))
  |> List.of_seq

let empty_cols grid =
  Seq.init (Grid.cols grid) Fun.id
  |> Seq.filter (fun col ->
         Seq.init (Grid.rows grid) Fun.id
         |> Seq.for_all (fun row -> Grid.get grid { row; col } == Space))
  |> List.of_seq

let count_lt ordered_indices index =
  List.find_index (fun offset -> offset > index) ordered_indices
  |> Option.value ~default:(List.length ordered_indices)

let fixup_coord ~expand_factor ~expanded_rows ~expanded_cols
    ({ row; col } : coord) : coord =
  {
    row = row + ((expand_factor - 1) * count_lt expanded_rows row);
    col = col + ((expand_factor - 1) * count_lt expanded_cols col);
  }

let expand_glaxy ~expand_factor image =
  let empty_r = empty_rows image in
  let empty_c = empty_cols image in
  galaxy_coords image
  |> List.map
       (fixup_coord ~expand_factor ~expanded_rows:empty_r ~expanded_cols:empty_c)

let rec seq_pairs seq =
  match Seq.uncons seq with
  | Some (next, rest) ->
      let with_first = Seq.map (fun other -> (next, other)) rest in
      Seq.append with_first (seq_pairs rest)
  | None -> Seq.empty

let distance_sum galaxies =
  List.to_seq galaxies |> seq_pairs
  |> Seq.map (fun (c1, c2) -> Advent.Grid.coord_dist c1 c2)
  |> Seq.fold_left ( + ) 0

let dist_after_expand ~expand_factor image =
  expand_glaxy ~expand_factor image |> distance_sum

let part1 = dist_after_expand ~expand_factor:2
let part2 = dist_after_expand ~expand_factor:1_000_000
let () = Advent.Main.run "input/day11.txt" read_input part1 part2
