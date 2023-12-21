module Coord = Advent.Coord

module Grid = Advent.Grid.Make (struct
  type t = int

  let of_char = Advent.Util.digit_of_char
  let to_char = Advent.Util.char_of_digit
end)

let read_input file_name =
  let city = In_channel.with_open_text file_name Grid.input in
  let all_gt_0 = Grid.to_seqi city |> Seq.for_all (fun (_, v) -> v > 0) in
  if not all_gt_0 then failwith "Grid cells not all > 0" else city

type state = { pos : Coord.t; straight_dir : Coord.dir; straight_count : int }
type queue_node = { state : state; path_dist : int; est_remaining : int }

let total_est_cost s = s.path_dist + s.est_remaining

module PriorityQ = Advent.Heap.Make (struct
  type t = queue_node

  let compare a b = Int.compare (total_est_cost a) (total_est_cost b)
end)

module VisitMap = Map.Make (struct
  type t = state

  let compare = compare
end)

let estimate_cost = Coord.dist

let reconstruct_path visited end_state =
  let rec loop state =
    match VisitMap.find_opt state visited with
    | Some parent -> state :: loop parent
    | None -> []
  in
  List.rev (loop end_state)

let print_path path =
  print_string "Path:";
  let rec loop = function
    | [] -> ()
    | next :: rest ->
        Printf.printf " [%a -> %s]" Coord.pp next.pos
          (Coord.dir_name next.straight_dir);
        loop rest
  in
  loop path;
  print_newline ()

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
  let rec loop visited queue =
    match PriorityQ.pop_min queue with
    | Some (current, rest) ->
        if current.state.pos = target && can_stop current.state then
          (current, visited)
        else
          let possible_steps =
            possible_dirs city current.state
            |> List.map (make_adj_node current)
            |> List.filter (fun node -> not (VisitMap.mem node.state visited))
          in
          loop
            (List.fold_left
               (fun acc step -> VisitMap.add step.state current.state acc)
               visited possible_steps)
            (List.fold_left (Fun.flip PriorityQ.add) rest possible_steps)
    | None -> raise Not_found
  in
  (* Initial queue containing all of the possible starting directions *)
  let init_queue =
    Coord.all_dirs |> List.to_seq
    |> Seq.filter (fun d -> Grid.in_bounds city (Coord.add_dir start d))
    |> Seq.map (fun d ->
           {
             state = { pos = start; straight_dir = d; straight_count = 0 };
             path_dist = 0;
             est_remaining = estimate_cost start target;
           })
    |> PriorityQ.of_seq
  in
  let end_node, visited = loop VisitMap.empty init_queue in
  let path = reconstruct_path visited end_node.state in
  print_path path;
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
