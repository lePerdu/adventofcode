type node = string
type direction = Left | Right
type branch = { from : node; to_left : node; to_right : node }
type map = { directions : direction list; branches : branch list }

let dir_of_char = function
  | 'L' -> Left
  | 'R' -> Right
  | c -> failwith @@ Printf.sprintf "Invalid direction character: %C" c

let read_directions ch =
  Scanf.bscanf ch "%s" (fun s ->
      String.to_seq s |> Seq.map dir_of_char |> List.of_seq)

let read_branch ch =
  Scanf.bscanf ch "%[A-Z0-9] = (%[A-Z0-9], %[A-Z0-9])\n"
    (fun from to_left to_right -> { from; to_left; to_right })

let rec read_branches ch =
  try
    let next = read_branch ch in
    next :: read_branches ch
  with End_of_file -> []

let read_input file_path =
  Scanf.Scanning.(
    let ch = open_in file_path in
    Fun.protect
      ~finally:(fun () -> close_in ch)
      (fun () ->
        let directions = read_directions ch in
        let () = Scanf.bscanf ch "\n\n" () in
        let branches = read_branches ch in
        { directions; branches }))

type map_graph = (node, branch) Hashtbl.t

let create_graph input =
  input.branches |> List.to_seq
  |> Seq.map (fun b -> (b.from, b))
  |> Hashtbl.of_seq

let map_lookup graph from dir =
  let node = Hashtbl.find graph from in
  match dir with Left -> node.to_left | Right -> node.to_right

let map_directions m = Seq.cycle (List.to_seq m.directions)

let steps_to_end ~start ~end_pred map_graph directions =
  directions
  |> Seq.scan (map_lookup map_graph) start
  |> Seq.mapi (fun index node -> (index, node))
  |> Seq.find (fun (_, node) -> end_pred node)
  |> Option.map (fun (index, _) -> index)
  |> Option.get

let part1 input =
  let graph = create_graph input in
  let dirs = map_directions input in
  steps_to_end ~start:"AAA" ~end_pred:(fun n -> n = "ZZZ") graph dirs

let nodes_with_suffix suffix map =
  map.branches
  |> List.map (fun b -> b.from)
  |> List.filter (fun node -> node.[String.length node - 1] == suffix)

let is_ending_node n = n.[String.length n - 1] == 'Z'
let rec gcd a b = if b == 0 then a else gcd b (a mod b)

let lcm a b =
  let g = gcd a b in
  a / g * b

let part2 input =
  let graph = create_graph input in
  let start_nodes = nodes_with_suffix 'A' input in
  start_nodes
  |> List.map (fun start ->
         steps_to_end ~start ~end_pred:is_ending_node graph
           (map_directions input))
  |> List.fold_left lcm 1

let () =
  let input = read_input "input/day8.txt" in
  Printf.printf "Part1: %d\n" (part1 input);
  Printf.printf "Part2: %d\n" (part2 input)
