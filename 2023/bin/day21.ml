module Coord = Advent.Coord
module Grid = Advent.Grid

type input_tile = Plot | Rock | Start
type tile = Plot | Rock
type input = { grid : tile Grid.t; start : Coord.t }

let tile_of_char : char -> input_tile option = function
  | '.' -> Some Plot
  | '#' -> Some Rock
  | 'S' -> Some Start
  | _ -> None

let read_input file_name =
  let raw = Grid.input_file tile_of_char file_name in
  let start =
    Grid.to_seqi raw
    |> Seq.find (fun (_, t) -> t == Start)
    |> Option.map fst |> Option.get
  in
  let grid =
    Grid.map
      (fun (x : input_tile) ->
        match x with Plot | Start -> Plot | Rock -> Rock)
      raw
  in
  { start; grid }

let part1_steps = 64

module CoordSet = Set.Make (Coord)

type visit_state = Plot | Rock | Visited

let get_visited_after input ~steps =
  let start_parity = (input.start.row + input.start.col) mod 2 in
  let steps_parity = steps mod 2 in
  let correct_parity (c : Coord.t) =
    (c.row + c.col) mod 2 == start_parity lxor steps_parity
  in
  let visited = Grid.mapi (fun c _ -> c = input.start) input.grid in
  let rec loop steps frontier =
    if steps == 0 then ()
    else
      let new_front =
        CoordSet.to_seq frontier
        |> Seq.flat_map (fun c -> Coord.adjacent c |> List.to_seq)
        |> Seq.filter (fun c ->
               (not (Grid.get visited c)) && Grid.get input.grid c == Plot)
        |> Seq.map (fun c ->
               Grid.set visited c true;
               c)
        |> CoordSet.of_seq
      in
      loop (steps - 1) new_front
  in
  let _ = loop steps (CoordSet.singleton input.start) in
  Grid.mapi
    (fun c v ->
      if v && correct_parity c then Visited
      else match Grid.get input.grid c with Plot -> Plot | Rock -> Rock)
    visited

let part1 input =
  let start_parity = (input.start.row + input.start.col) mod 2 in
  let steps_parity = part1_steps mod 2 in
  let visited = Grid.mapi (fun c _ -> c = input.start) input.grid in
  let rec loop steps frontier =
    if steps == 0 then ()
    else
      let new_front =
        CoordSet.to_seq frontier
        |> Seq.flat_map (fun c -> Coord.adjacent c |> List.to_seq)
        |> Seq.filter (fun c ->
               (not (Grid.get visited c))
               && Grid.in_bounds input.grid c
               && Grid.get input.grid c == Plot)
        |> Seq.map (fun c ->
               Grid.set visited c true;
               c)
        |> CoordSet.of_seq
      in
      loop (steps - 1) new_front
  in
  let _ = loop part1_steps (CoordSet.singleton input.start) in
  Grid.to_seqi visited |> Seq.filter snd
  |> Seq.filter (fun (c, _) ->
         let parity = (c.Coord.row + c.col) mod 2 in
         start_parity lxor steps_parity == parity)
  |> Seq.length

let expand_grid orig init times =
  let dup = Grid.make (Grid.rows orig * times) (Grid.cols orig * times) init in
  Coord.iter_inside
    (fun outer ->
      let offset = Coord.mul outer (Grid.size orig) in
      Coord.iter_inside
        (fun inner ->
          Grid.set dup (Coord.add offset inner) (Grid.get orig inner))
        (Grid.size orig))
    { Coord.row = times; col = times };
  dup

let expand_input input times =
  {
    start =
      Coord.add (Coord.scale (times / 2) (Grid.size input.grid)) input.start;
    grid = expand_grid input.grid Plot times;
  }

let count_visited input ~times =
  let grid_size = Grid.rows input.grid in
  let expanded = expand_input input times in
  let steps = (grid_size / 2) + (grid_size * (times / 2)) in
  let visited = get_visited_after expanded ~steps in
  Grid.to_seqi visited
  |> Seq.filter (fun (c, v) ->
         v == Visited
         && (c.Coord.row + c.col) mod 2
            == (input.start.row + input.start.col) mod 2 lxor (steps mod 2))
  |> Seq.length

let part2_steps = 26501365

let part2 input =
  assert (Grid.rows input.grid == Grid.cols input.grid);
  let grid_size = Grid.rows input.grid in
  assert (grid_size mod 2 == 1);
  let a0 = count_visited input ~times:1 in
  let a1 = count_visited input ~times:3 in
  let a2 = count_visited input ~times:5 in
  let delta0 = a1 - a0 in
  let delta1 = a2 - a1 in
  (* secondary delta is constant *)
  let delta_delta = delta1 - delta0 in
  let chunks = (part2_steps - (grid_size / 2)) / grid_size in
  a0 + (chunks * (delta0 + delta0 + (delta_delta * (chunks - 1))) / 2)

let () = Advent.Main.run "input/day21.txt" read_input part1 part2
