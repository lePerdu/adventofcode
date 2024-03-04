type cell = Empty | Cube | Round

module Grid = Advent.Grid

let cell_of_char = function
  | '.' -> Some Empty
  | '#' -> Some Cube
  | 'O' -> Some Round
  | _ -> None

(* let cell_to_char = function Empty -> '.' | Cube -> '#' | Round -> 'O' *)

type coord = Advent.Coord.t

let read_input file_name =
  In_channel.with_open_text file_name (Grid.input cell_of_char)

(* let find_north_stop_row platform (coord : coord) = *)
(*   Seq.init coord.row (fun x -> coord.row - x - 1) *)
(*   |> Seq.find (fun row -> Grid.get platform { coord with row } != Empty) *)
(*   |> Option.map succ |> Option.value ~default:0 *)

(* let roll_north platform coord = *)
(*   let stop_row = find_north_stop_row platform coord in *)
(*   Grid.set platform coord Empty; *)
(*   Grid.set platform { coord with row = stop_row } Round *)

(* let tilt_north platform = *)
(*   (\* Start at row 0 and work up from there *\) *)
(*   Grid.rows_seq platform *)
(*   |> Seq.flat_map (fun row -> *)
(*          Grid.cols_seq platform *)
(*          |> Seq.map (fun col : coord -> { row; col }) *)
(*          |> Seq.filter (fun c -> Grid.get platform c == Round)) *)
(*   |> Seq.iter (roll_north platform) *)

let do_tilt ~ordered_seq ~roll_seq ~prev_coord ~edge_coord platform =
  ordered_seq platform
  |> Seq.filter (fun coord -> Grid.get platform coord == Round)
  |> Seq.iter (fun coord ->
         let stop_coord =
           roll_seq platform coord
           |> Seq.find (fun c -> Grid.get platform c != Empty)
           |> Option.fold ~none:(edge_coord coord) ~some:prev_coord
         in
         Grid.set platform coord Empty;
         Grid.set platform stop_coord Round)

let rocks_seq_north platform =
  Grid.rows_seq platform
  |> Seq.flat_map (fun row ->
         Grid.cols_seq platform |> Seq.map (fun col : coord -> { row; col }))

let north_rows_seq _platform (coord : coord) =
  Seq.init coord.row (fun x -> coord.row - x - 1)
  |> Seq.map (fun row -> { coord with row })

let tilt_north platform =
  do_tilt ~ordered_seq:rocks_seq_north ~roll_seq:north_rows_seq
    ~prev_coord:(fun c -> { c with row = c.row + 1 })
    ~edge_coord:(fun c -> { c with row = 0 })
    platform

let rocks_seq_south platform =
  Grid.rows_seq platform
  |> Seq.map (fun row -> Grid.rows platform - row - 1)
  |> Seq.flat_map (fun row ->
         Grid.cols_seq platform |> Seq.map (fun col : coord -> { row; col }))

let south_rows_seq platform (coord : coord) =
  Seq.ints (coord.row + 1)
  |> Seq.take (Grid.rows platform - coord.row - 1)
  |> Seq.map (fun row -> { coord with row })

let tilt_south platform =
  do_tilt ~ordered_seq:rocks_seq_south ~roll_seq:south_rows_seq
    ~prev_coord:(fun c -> { c with row = c.row - 1 })
    ~edge_coord:(fun c -> { c with row = Grid.rows platform - 1 })
    platform

let rocks_seq_east platform =
  Grid.cols_seq platform
  |> Seq.map (fun col -> Grid.cols platform - col - 1)
  |> Seq.flat_map (fun col ->
         Grid.rows_seq platform |> Seq.map (fun row : coord -> { row; col }))

let east_cols_seq platform (coord : coord) =
  Seq.ints (coord.col + 1)
  |> Seq.take (Grid.cols platform - coord.col - 1)
  |> Seq.map (fun col -> { coord with col })

let tilt_east platform =
  do_tilt ~ordered_seq:rocks_seq_east ~roll_seq:east_cols_seq
    ~prev_coord:(fun c -> { c with col = c.col - 1 })
    ~edge_coord:(fun c -> { c with col = Grid.cols platform - 1 })
    platform

let rocks_seq_west platform =
  Grid.cols_seq platform
  |> Seq.flat_map (fun col ->
         Grid.rows_seq platform |> Seq.map (fun row : coord -> { row; col }))

let west_cols_seq _platform (coord : coord) =
  Seq.init coord.col (fun x -> coord.col - x - 1)
  |> Seq.map (fun col -> { coord with col })

let tilt_west platform =
  do_tilt ~ordered_seq:rocks_seq_west ~roll_seq:west_cols_seq
    ~prev_coord:(fun c -> { c with col = c.col + 1 })
    ~edge_coord:(fun c -> { c with col = 0 })
    platform

let round_rocks_seq platform =
  Grid.to_seqi platform
  |> Seq.filter (fun (_, cell) -> cell == Round)
  |> Seq.map (fun (coord, _) -> coord)

let total_load platform =
  round_rocks_seq platform
  |> Seq.map (fun (coord : coord) -> Grid.rows platform - coord.row)
  |> Seq.fold_left ( + ) 0

let part1 input =
  let input = Grid.copy input in
  tilt_north input;
  total_load input

let run_cycle platform =
  let platform = Grid.copy platform in
  tilt_north platform;
  tilt_west platform;
  tilt_south platform;
  tilt_east platform;
  platform

let rec run_cycles_direct n init =
  if n == 0 then init else run_cycles_direct (n - 1) (run_cycle init)

type repetition = {
  start_iteration : int;
  period : int;
  repeated_state : cell Grid.t;
}

let find_repeat_cycle init =
  let seen = Hashtbl.create 1024 in
  let rec loop state cycle_count =
    match Hashtbl.find_opt seen state with
    | Some prev_count ->
        {
          start_iteration = prev_count;
          period = cycle_count - prev_count;
          repeated_state = state;
        }
    | None ->
        Hashtbl.add seen state cycle_count;
        let next = run_cycle state in
        loop next (cycle_count + 1)
  in
  loop init 0

let run_cycles total init =
  let rep = find_repeat_cycle init in
  if rep.start_iteration > total then
    (* Fall back to this in case the repeated state happens too late *)
    run_cycles_direct total init
  else
    let left_over = (total - rep.start_iteration) mod rep.period in
    run_cycles_direct left_over rep.repeated_state

let part2 input = run_cycles 1000000000 input |> total_load
let () = Advent.Main.run "input/day14.txt" read_input part1 part2
