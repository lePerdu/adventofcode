module Coord = Advent.Coord
module Grid = Advent.Grid

let read_input file_name =
  let city =
    In_channel.with_open_text file_name (Grid.input Advent.Util.digit_of_char)
  in
  let all_gt_0 = Grid.to_seqi city |> Seq.for_all (fun (_, v) -> v > 0) in
  if not all_gt_0 then failwith "Grid cells not all > 0" else city

type state = { pos : Coord.t; straight_dir : Coord.dir; straight_count : int }
type queue_node = { state : state; path_dist : int; est_remaining : int }

let dummy_state =
  {
    state =
      { pos = { row = 0; col = 0 }; straight_dir = Right; straight_count = 0 };
    path_dist = 0;
    est_remaining = 0;
  }

let total_est_cost s = s.path_dist + s.est_remaining

module PriorityQ = Binary_heap.Make (struct
  type t = queue_node

  let compare a b = Int.compare (total_est_cost a) (total_est_cost b)
end)

let estimate_cost = Coord.dist

(* let reconstruct_path visited end_state = *)
(*   let rec loop state = *)
(*     match Hashtbl.find_opt visited state with *)
(*     | Some parent -> state :: loop parent *)
(*     | None -> [] *)
(*   in *)
(*   List.rev (loop end_state) *)

(* let print_path path = *)
(*   print_string "Path:"; *)
(*   let rec loop = function *)
(*     | [] -> () *)
(*     | next :: rest -> *)
(*         Printf.printf " [%a -> %s]" Coord.pp next.pos *)
(*           (Coord.dir_name next.straight_dir); *)
(*         loop rest *)
(*   in *)
(*   loop path; *)
(*   print_newline () *)

let find_min_path ~can_go_in_dir ~can_stop city start target =
  let possible_dirs city state =
    Coord.all_dirs
    |> List.filter (fun d ->
           can_go_in_dir state d
           && Grid.in_bounds city (Coord.add_dir state.pos d))
  in
  let make_adj_node from dir =
    let new_pos = Coord.add_dir from.state.pos dir in
    {
      state =
        {
          pos = new_pos;
          straight_dir = dir;
          straight_count =
            (if from.state.straight_dir == dir then
               from.state.straight_count + 1
             else 1);
        };
      path_dist = from.path_dist + Grid.get city new_pos;
      est_remaining = estimate_cost new_pos target;
    }
  in
  let est_size = (Grid.rows city + Grid.cols city) * 2 in
  let visited = Hashtbl.create est_size in
  let queue = PriorityQ.create ~dummy:dummy_state est_size in
  let rec loop () =
    if PriorityQ.is_empty queue then raise Not_found
    else
      let current = PriorityQ.pop_minimum queue in
      if Coord.equal current.state.pos target && can_stop current.state then
        current
      else (
        possible_dirs city current.state
        |> List.map (make_adj_node current)
        |> List.filter (fun node -> not (Hashtbl.mem visited node.state))
        |> List.iter (fun step ->
               Hashtbl.add visited step.state current.state;
               PriorityQ.add queue step);
        loop ())
  in
  (* Initial queue containing all of the possible starting directions *)
  let () =
    Coord.all_dirs |> List.to_seq
    |> Seq.filter (fun d -> Grid.in_bounds city (Coord.add_dir start d))
    |> Seq.map (fun d ->
           {
             state = { pos = start; straight_dir = d; straight_count = 0 };
             path_dist = 0;
             est_remaining = estimate_cost start target;
           })
    |> Seq.iter (PriorityQ.add queue)
  in
  let end_node = loop () in
  (* let path = reconstruct_path visited end_node.state in *)
  (* print_path path; *)
  end_node.path_dist

let part1_max_straight_count = 3

let part1 input =
  find_min_path input { row = 0; col = 0 } (Grid.max_coord input)
    ~can_go_in_dir:(fun state dir ->
      let op_dir = Coord.dir_opposite state.straight_dir in
      let can_go_straight = state.straight_count < part1_max_straight_count in
      dir != op_dir && (can_go_straight || dir != state.straight_dir))
    ~can_stop:(Fun.const true)

let part2_min_straight_to_turn = 4
let part2_max_straight = 10

let part2 input =
  find_min_path input { row = 0; col = 0 } (Grid.max_coord input)
    ~can_go_in_dir:(fun state dir ->
      let can_turn = state.straight_count >= part2_min_straight_to_turn in
      if can_turn then
        let op_dir = Coord.dir_opposite state.straight_dir in
        let can_go_straight = state.straight_count < part2_max_straight in
        dir != op_dir && (can_go_straight || dir != state.straight_dir)
      else dir == state.straight_dir)
    ~can_stop:(fun state -> state.straight_count >= part2_min_straight_to_turn)

let () = Advent.Main.run "input/day17.txt" read_input part1 part2
