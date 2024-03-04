module Coord = Advent.Coord

type tile = Empty | MirrorForward | MirrorBackward | SplitHoriz | SplitVert
type beam_state = { pos : Coord.t; dir : Coord.dir }

module Grid = Advent.Grid

let tile_of_char = function
  | '.' -> Some Empty
  | '/' -> Some MirrorForward
  | '\\' -> Some MirrorBackward
  | '-' -> Some SplitHoriz
  | '|' -> Some SplitVert
  | _ -> None

(* let tile_to_char = function *)
(*   | Empty -> '.' *)
(*   | MirrorForward -> '/' *)
(*   | MirrorBackward -> '\\' *)
(*   | SplitHoriz -> '-' *)
(*   | SplitVert -> '|' *)

let read_input file_name =
  In_channel.with_open_text file_name (Grid.input tile_of_char)

let coord_move ({ row; col } : Coord.t) (dir : Coord.dir) : Coord.t =
  match dir with
  | Up -> { row = row - 1; col }
  | Down -> { row = row + 1; col }
  | Left -> { row; col = col - 1 }
  | Right -> { row; col = col + 1 }

let step_beam grid { pos; dir } =
  let beam_move new_dir = { pos = coord_move pos new_dir; dir = new_dir } in
  match Grid.get grid pos with
  | Empty -> [ beam_move dir ]
  | MirrorForward ->
      let new_dir : Coord.dir =
        match dir with Up -> Right | Down -> Left | Left -> Down | Right -> Up
      in
      [ beam_move new_dir ]
  | MirrorBackward ->
      let new_dir : Coord.dir =
        match dir with Up -> Left | Down -> Right | Left -> Up | Right -> Down
      in
      [ beam_move new_dir ]
  | SplitHoriz -> (
      match dir with
      | Left | Right -> [ beam_move dir ]
      | Up | Down -> [ beam_move Left; beam_move Right ])
  | SplitVert -> (
      match dir with
      | Up | Down -> [ beam_move dir ]
      | Left | Right -> [ beam_move Up; beam_move Down ])

let trace_beam grid init_state =
  let seen = Hashtbl.create 16 in
  let queue = Stack.create () in
  let add_state s =
    Hashtbl.replace seen s ();
    Stack.push s queue
  in
  let rec loop () =
    match Stack.pop_opt queue with
    | None -> ()
    | Some state ->
        let next_states =
          step_beam grid state
          |> List.filter (fun s -> Grid.in_bounds grid s.pos)
          |> List.filter (fun s -> not (Hashtbl.mem seen s))
        in
        List.iter add_state next_states;
        loop ()
  in
  add_state init_state;
  loop ();
  Hashtbl.to_seq_keys seen

let count_energized ~init grid =
  let trace = trace_beam grid init in
  let energized_tiles = Hashtbl.create (Grid.rows grid * Grid.cols grid / 4) in
  Seq.iter (fun s -> Hashtbl.replace energized_tiles s.pos ()) trace;
  Hashtbl.length energized_tiles

let part1 = count_energized ~init:{ pos = { row = 0; col = 0 }; dir = Right }

let starts_seq grid =
  let rows = Grid.rows grid in
  let cols = Grid.cols grid in
  List.to_seq
    [
      Seq.init cols (fun col -> { pos = { row = 0; col }; dir = Down });
      Seq.init cols (fun col -> { pos = { row = rows - 1; col }; dir = Up });
      Seq.init rows (fun row -> { pos = { row; col = 0 }; dir = Right });
      Seq.init rows (fun row -> { pos = { row; col = cols - 1 }; dir = Left });
    ]
  |> Seq.concat

let part2 input =
  starts_seq input
  |> Seq.map (fun init -> count_energized ~init input)
  |> Seq.fold_left max 0

let () = Advent.Main.run "input/day16.txt" read_input part1 part2
