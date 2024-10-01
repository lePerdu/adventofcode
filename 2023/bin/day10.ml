type north_south = North | South
type east_west = East | West
type tile = Empty | Start | Vert | Horiz | Bend of (north_south * east_west)
type coord = { row : int; col : int }
type grid = { start : coord; data : tile array array }

let tile_of_char = function
  | '|' -> Vert
  | '-' -> Horiz
  | 'L' -> Bend (North, East)
  | 'J' -> Bend (North, West)
  | '7' -> Bend (South, West)
  | 'F' -> Bend (South, East)
  | '.' -> Empty
  | 'S' -> Start
  | c -> failwith @@ Printf.sprintf "Invalid tile character: %C" c

let char_of_tile = function
  | Vert -> '|'
  | Horiz -> '-'
  | Bend (North, East) -> 'L'
  | Bend (North, West) -> 'J'
  | Bend (South, West) -> '7'
  | Bend (South, East) -> 'F'
  | Empty -> '.'
  | Start -> 'S'

let read_grid cell_of_char ch =
  let read_row () =
    In_channel.input_line ch
    |> Option.map (fun line ->
           String.to_seq line |> Seq.map cell_of_char |> Array.of_seq)
  in
  let rec loop rows col_count =
    match read_row () with
    | Some next ->
        if Array.length next == col_count then loop (next :: rows) col_count
        else failwith "Rows of inconsistent length"
    | None -> rows
  in
  match read_row () with
  | Some first ->
      loop [ first ] (Array.length first) |> List.rev |> Array.of_list
  | None -> failwith "Missing first row"

let grid_of_matrix data =
  let start_coord =
    Array.to_seqi data
    |> Seq.flat_map (fun (row_index, row) ->
           Array.to_seqi row
           |> Seq.map (fun (col_index, cell) ->
                  ({ row = row_index; col = col_index }, cell)))
    |> Seq.find (fun (_coord, cell) -> cell == Start)
    |> Option.map (fun (coord, _cell) -> coord)
    |> Option.get
  in
  { start = start_coord; data }

let read_input file_name =
  In_channel.with_open_text file_name (fun ch ->
      grid_of_matrix (read_grid tile_of_char ch))

type pipe_graph = { adj : (coord, coord list) Hashtbl.t }

let grid_rows g = Array.length g.data
let grid_cols g = Array.length g.data.(0)

let grid_in_bounds g { row; col } =
  0 <= row && row < grid_rows g && 0 <= col && col < grid_cols g

let grid_get g pos = g.data.(pos.row).(pos.col)

let coord_ns { row; col } = function
  | North -> { row = row - 1; col }
  | South -> { row = row + 1; col }

let coord_ew { row; col } = function
  | East -> { row; col = col + 1 }
  | West -> { row; col = col - 1 }

let find_connections grid pos =
  match grid_get grid pos with
  | Vert -> [ coord_ns pos North; coord_ns pos South ]
  | Horiz -> [ coord_ew pos East; coord_ew pos West ]
  | Bend (ns, ew) -> [ coord_ns pos ns; coord_ew pos ew ]
  | Start | Empty -> []

let get_raw_adjacent pos =
  [
    coord_ns pos North; coord_ns pos South; coord_ew pos East; coord_ew pos West;
  ]

let find_connected_to grid pos =
  get_raw_adjacent pos
  |> List.filter (grid_in_bounds grid)
  |> List.filter (fun adj -> List.mem pos @@ find_connections grid adj)

let find_pipe_span grid =
  let adj = Hashtbl.create (grid_rows grid * grid_cols grid / 4) in
  let rec loop queue =
    match queue with
    | [] -> adj
    | next_pos :: rest ->
        if Hashtbl.mem adj next_pos then loop rest
        else
          let adjacent_coords =
            match grid_get grid next_pos with
            | Start -> find_connected_to grid next_pos
            | Empty ->
                failwith
                @@ Printf.sprintf "Pipe span is not a loop: (%d, %d)"
                     next_pos.row next_pos.col
            | _ -> find_connections grid next_pos
          in
          assert (List.for_all (grid_in_bounds grid) adjacent_coords);
          Hashtbl.replace adj next_pos adjacent_coords;
          loop (adjacent_coords @ rest)
  in
  { adj = loop [ grid.start ] }

let part1 input =
  let graph = find_pipe_span input in
  (* Maximum distance in a loop is just half the length *)
  let max_dist = Hashtbl.length graph.adj / 2 in
  max_dist

type path_dir = Up | Down | Left | Right
type path_node = { coord : coord; dir : path_dir }

(* Directions in the path, always going counter-clockwise around the loop *)
type path = { steps : path_node array }

let coord_move { row; col } = function
  | Up -> { row = row - 1; col }
  | Down -> { row = row + 1; col }
  | Left -> { row; col = col - 1 }
  | Right -> { row; col = col + 1 }

let dir_of_ns = function North -> Up | South -> Down
let dir_of_ew = function East -> Right | West -> Left

let string_of_dir = function
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"

type continue_result = Continue of path_node | Stop | AtStart of coord

let try_continue_path grid { coord = pos; dir } =
  let next_pos = coord_move pos dir in
  if not (grid_in_bounds grid next_pos) then Stop
  else
    match grid_get grid next_pos with
    | Empty -> Stop
    | Start -> AtStart next_pos
    | Vert -> (
        match dir with
        | Up | Down -> Continue { coord = next_pos; dir }
        | _ -> Stop)
    | Horiz -> (
        match dir with
        | Left | Right -> Continue { coord = next_pos; dir }
        | _ -> Stop)
    | Bend (ns, ew) -> (
        match (ns, ew, dir) with
        | North, out, Down -> Continue { coord = next_pos; dir = dir_of_ew out }
        | South, out, Up -> Continue { coord = next_pos; dir = dir_of_ew out }
        | out, East, Left -> Continue { coord = next_pos; dir = dir_of_ns out }
        | out, West, Right -> Continue { coord = next_pos; dir = dir_of_ns out }
        | _ -> Stop)

let compute_turns dir new_dir =
  match (dir, new_dir) with
  | Up, Left -> 1
  | Up, Right -> -1
  | Down, Left -> -1
  | Down, Right -> 1
  | Left, Down -> 1
  | Left, Up -> -1
  | Right, Down -> -1
  | Right, Up -> 1
  | _ -> 0

let dir_opposite = function
  | Up -> Down
  | Down -> Up
  | Left -> Right
  | Right -> Left

let reverse_nodes ~start nodes =
  let coords_seq =
    Seq.cons start (List.to_seq nodes |> Seq.map (fun n -> n.coord))
  in
  let dirs_seq = List.to_seq nodes |> Seq.map (fun n -> dir_opposite n.dir) in
  Seq.zip coords_seq dirs_seq
  |> Seq.map (fun (coord, dir) -> { coord; dir })
  |> Array.of_seq

let find_pipe_loop grid =
  let rec loop node steps cc_turns =
    match try_continue_path grid node with
    | Continue new_node ->
        loop new_node (node :: steps)
          (cc_turns + compute_turns node.dir new_node.dir)
    | AtStart start_pos ->
        if start_pos = grid.start then (node :: steps, cc_turns)
        else
          failwith
          @@ Printf.sprintf "Multiple starts found: (%d, %d) and (%d, %d)"
               grid.start.row grid.start.col start_pos.row start_pos.col
    | Stop ->
        failwith
        @@ Printf.sprintf "Path stops at (%d, %d) -> %s" node.coord.row
             node.coord.col (string_of_dir node.dir)
  in
  let init_dir =
    [ Up; Down; Left; Right ]
    |> List.find (fun dir ->
           match try_continue_path grid { coord = grid.start; dir } with
           | Continue _ -> true
           | _ -> false)
  in
  let steps, cc_turns = loop { coord = grid.start; dir = init_dir } [] 0 in
  let steps =
    if cc_turns < 0 then reverse_nodes ~start:grid.start steps
    else Array.of_list (List.rev steps)
  in
  { steps }

type tile_state = Loop | Inside | Outside

let make_visited_grid input =
  Array.make_matrix (grid_rows input) (grid_cols input) Outside

let visited_get v { row; col } = v.(row).(col)
let visited_set v { row; col } state = v.(row).(col) <- state

let visited_in_bounds v { row; col } =
  0 <= row && row < Array.length v && 0 <= col && col < Array.length v.(0)

let mark_along_path visited path =
  Array.iter (fun node -> visited_set visited node.coord Loop) path.steps

(* Coordinate inside the loop moving counter-clockwise in the direction *)
let coord_inside_loop_node { coord = { row; col }; dir } =
  match dir with
  | Up -> { row; col = col - 1 }
  | Down -> { row; col = col + 1 }
  | Left -> { row = row + 1; col }
  | Right -> { row = row - 1; col }

let print_visited_grid input visited =
  for r = 0 to grid_rows input - 1 do
    for c = 0 to grid_cols input - 1 do
      print_char
        (match visited.(r).(c) with
        | Inside -> 'I'
        | Outside -> '.'
        | Loop -> char_of_tile input.data.(r).(c))
    done;
    print_newline ()
  done

let adjacent_outside_tiles visited around_coord =
  get_raw_adjacent around_coord
  |> List.filter (visited_in_bounds visited)
  |> List.filter (fun c -> visited_get visited c = Outside)

let mark_and_expand_inside_region visited from_coord =
  let rec loop = function
    | [] -> ()
    | next :: rest ->
        let adjs = adjacent_outside_tiles visited next in
        List.iter (fun c -> visited_set visited c Inside) adjs;
        loop (adjs @ rest)
  in
  if
    visited_in_bounds visited from_coord
    && visited_get visited from_coord = Outside
  then (
    visited_set visited from_coord Inside;
    loop [ from_coord ])
  else ()

let mark_inside_path visited path =
  let len = Array.length path.steps in
  for i = 0 to len - 1 do
    let cur_node = path.steps.(i) in
    let prev_node = path.steps.((len + i - 1) mod len) in
    let inside1 = coord_inside_loop_node cur_node in
    (* Also mark using the previous direction to handle turns. *)
    (* TODO There's probably a better path representation which is
       cleaner here *)
    let inside2 =
      coord_inside_loop_node { cur_node with dir = prev_node.dir }
    in
    mark_and_expand_inside_region visited inside1;
    mark_and_expand_inside_region visited inside2
  done

let mark_path visited path =
  mark_along_path visited path;
  mark_inside_path visited path

let part2 input =
  let path = find_pipe_loop input in
  let visited_grid = make_visited_grid input in
  mark_path visited_grid path;
  print_visited_grid input visited_grid;
  Array.to_seq visited_grid
  |> Seq.map (fun row ->
         Array.to_seq row |> Seq.filter (fun t -> t = Inside) |> Seq.length)
  |> Seq.fold_left ( + ) 0

let () =
  let input = read_input "input/day10.txt" in
  Printf.printf "Part1: %d\n" (part1 input);
  Printf.printf "Part2: %d\n" (part2 input)
