module Grid = Advent.Grid
module Coord = Advent.Coord

type maze_tile = Path | Forest | SlopeU | SlopeR | SlopeD | SlopeL

let tile_of_char = function
  | '.' -> Some Path
  | '#' -> Some Forest
  | '^' -> Some SlopeU
  | '>' -> Some SlopeR
  | 'v' -> Some SlopeD
  | '<' -> Some SlopeL
  | _ -> None

type input = { maze : maze_tile Grid.t; start : Coord.t; finish : Coord.t }

let read_input file_name =
  let maze = Grid.input_file tile_of_char file_name in
  let start : Coord.t = { row = 0; col = 1 } in
  assert (Grid.get maze start == Path);
  let finish : Coord.t =
    { row = Grid.rows maze - 1; col = Grid.cols maze - 2 }
  in
  assert (Grid.get maze finish == Path);
  { maze; start; finish }

module CoordSet = Set.Make (Coord)
module CoordHash = Hashtbl.Make (Coord)
module IntSet = Set.Make (Int)
module IntHash = Hashtbl.Make (Int)

type node = int
type edge = { dst : node; len : int }
type node_desc = { edges : edge list }
type graph = { start : node; finish : node; nodes : node_desc array }

let maze_neighbors maze current visited =
  match Grid.get maze current with
  | Path ->
      Coord.all_dirs
      |> List.filter_map (fun dir ->
             let next = Coord.add_dir current dir in
             let can_step =
               Grid.in_bounds maze next
               &&
               match Grid.get maze next with
               | Path -> true
               | Forest -> false
               | SlopeU -> dir != Coord.Down
               | SlopeR -> dir != Coord.Left
               | SlopeD -> dir != Coord.Up
               | SlopeL -> dir != Coord.Right
             in
             let have_visited = CoordHash.mem visited next in
             if can_step && not have_visited then Some next else None)
  | Forest -> failwith "cannot find neighbors from forest tile"
  | SlopeU -> [ Coord.add_dir current Up ]
  | SlopeR -> [ Coord.add_dir current Right ]
  | SlopeD -> [ Coord.add_dir current Down ]
  | SlopeL -> [ Coord.add_dir current Left ]

let build_graph { maze; start; finish } =
  let nodes_by_coord = CoordHash.create 32 in
  let edges_by_node = IntHash.create 32 in
  let explore_set = IntHash.create 32 in
  let explore_queue = Queue.create () in
  let get_or_create_node coord =
    match CoordHash.find_opt nodes_by_coord coord with
    | Some found -> found
    | None ->
        let new_node = CoordHash.length nodes_by_coord in
        CoordHash.add nodes_by_coord coord new_node;
        new_node
  in
  let is_node coord =
    match Grid.get maze coord with
    | Path -> coord = start || coord = finish
    | Forest -> false
    | _ -> true
  in
  let add_edge src_node dst len =
    let dst_node = get_or_create_node dst in
    IntHash.add edges_by_node src_node { dst = dst_node; len };
    if not (IntHash.mem explore_set dst_node) then (
      Queue.add dst explore_queue;
      IntHash.add explore_set dst_node ())
  in
  let explore src =
    let src_node = get_or_create_node src in
    let visited = CoordHash.create 32 in
    let queue = Queue.create () in
    let visit node depth =
      Queue.add (node, depth) queue;
      CoordHash.add visited node ()
    in
    let rec loop () =
      match Queue.take_opt queue with
      | Some (current, depth) ->
          let neighbors = maze_neighbors maze current visited in
          List.iter
            (fun (next : Coord.t) ->
              if is_node next then add_edge src_node next (depth + 1)
              else visit next (depth + 1))
            neighbors;
          loop ()
      | None -> ()
    in
    visit src 0;
    loop ()
  in
  let rec loop () =
    match Queue.take_opt explore_queue with
    | Some current ->
        explore current;
        loop ()
    | None -> ()
  in
  let start_node = get_or_create_node start in
  let finish_node = get_or_create_node finish in
  let () = Queue.add start explore_queue in
  let () = IntHash.add explore_set start_node () in
  let () = IntHash.add explore_set finish_node () in
  loop ();
  let nodes =
    Array.init (CoordHash.length nodes_by_coord) (fun node ->
        { edges = IntHash.find_all edges_by_node node })
  in
  { start = start_node; finish = finish_node; nodes }

let find_all_path_lens graph =
  let rec loop current visited len =
    let recur edge =
      loop edge.dst (IntSet.add edge.dst visited) (len + edge.len)
    in
    if current == graph.finish then Seq.return len
    else
      let v = graph.nodes.(current) in
      let candidates =
        v.edges |> List.filter (fun edge -> not (IntSet.mem edge.dst visited))
      in
      match candidates with
      | [] -> Seq.empty
      | [ single ] -> recur single
      | multiple -> Seq.flat_map recur (List.to_seq multiple)
  in
  loop graph.start (IntSet.singleton graph.start) 0

let part1 input =
  build_graph input |> find_all_path_lens |> Seq.fold_left Int.max 0

let maze_neighbors_all maze current visited =
  Coord.all_dirs
  |> List.filter_map (fun dir ->
         let next = Coord.add_dir current dir in
         if
           Grid.in_bounds maze next
           && Grid.get maze next != Forest
           && not (CoordHash.mem visited next)
         then Some next
         else None)

let build_graph2 { maze; start; finish } =
  (* TODO: Share code with part1 *)
  let nodes_by_coord = CoordHash.create 32 in
  let edges_by_node = IntHash.create 32 in
  let explore_set = IntHash.create 32 in
  let explore_queue = Queue.create () in
  let get_or_create_node coord =
    match CoordHash.find_opt nodes_by_coord coord with
    | Some found -> found
    | None ->
        let new_node = CoordHash.length nodes_by_coord in
        CoordHash.add nodes_by_coord coord new_node;
        new_node
  in
  let add_edge src_node dst len =
    let dst_node = get_or_create_node dst in
    IntHash.add edges_by_node src_node { dst = dst_node; len };
    if not (IntHash.mem explore_set dst_node) then (
      Queue.add dst explore_queue;
      IntHash.add explore_set dst_node ())
  in
  let explore src =
    let src_node = get_or_create_node src in
    let visited = CoordHash.create 32 in
    let queue = Queue.create () in
    let visit node depth =
      Queue.add (node, depth) queue;
      CoordHash.add visited node ()
    in
    let rec loop () =
      match Queue.take_opt queue with
      | Some (current, depth) ->
          let neighbors = maze_neighbors_all maze current visited in
          (match neighbors with
          | [] -> ()
          | [ single ] ->
              if single = start || single = finish then
                add_edge src_node single (depth + 1)
              else visit single (depth + 1)
          | _multiple ->
              assert (current != src);
              add_edge src_node current depth);
          loop ()
      | None -> ()
    in
    CoordHash.add visited src ();
    maze_neighbors_all maze src visited
    |> List.iter (fun branch -> visit branch 1);
    loop ()
  in
  let rec loop () =
    match Queue.take_opt explore_queue with
    | Some current ->
        explore current;
        loop ()
    | None -> ()
  in
  let start_node = get_or_create_node start in
  let finish_node = get_or_create_node finish in
  let () = Queue.add start explore_queue in
  let () = IntHash.add explore_set start_node () in
  let () = IntHash.add explore_set finish_node () in
  loop ();
  let nodes =
    Array.init (CoordHash.length nodes_by_coord) (fun node ->
        { edges = IntHash.find_all edges_by_node node })
  in
  { start = start_node; finish = finish_node; nodes }

let part2 input =
  build_graph2 input |> find_all_path_lens |> Seq.fold_left Int.max 0

let () = Advent.Main.run "input/day23.txt" read_input part1 part2
