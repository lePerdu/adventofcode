type name = string

module NameHash = Hashtbl.Make (String)

module Graph = struct
  type vertex = int
  type weight = int

  type t = {
    mutable size : int;
    vertex_by_name : vertex NameHash.t;
    (* Each vertex can map to multiple names after merging *)
    names_by_vertex : name list array;
    edge_matrix : weight array array;
  }

  let create vertex_by_name =
    let n = NameHash.length vertex_by_name in
    let names_by_vertex = Array.make n [] in
    let () =
      NameHash.iter
        (fun name index -> names_by_vertex.(index) <- [ name ])
        vertex_by_name
    in
    let () =
      assert (
        Array.for_all (fun names -> not (List.is_empty names)) names_by_vertex)
    in
    {
      size = n;
      vertex_by_name;
      names_by_vertex;
      edge_matrix = Array.make_matrix n n 0;
    }

  let size g = g.size
  let compound_size g = Array.length g.names_by_vertex
  let get_vertex_by_name g = NameHash.find g.vertex_by_name
  let get_vertex_names g v = g.names_by_vertex.(v)
  let get_vertex_compound_size g v = List.length (get_vertex_names g v)
  let get_edge_weights g u = g.edge_matrix.(u)

  let incr_weight g u v w_incr =
    g.edge_matrix.(u).(v) <- g.edge_matrix.(u).(v) + w_incr

  let add_edge g u v w =
    incr_weight g u v w;
    incr_weight g v u w

  let remove_edge g u v =
    g.edge_matrix.(u).(v) <- 0;
    g.edge_matrix.(v).(u) <- 0

  let edges_from_seq g v =
    Array.to_seqi g.edge_matrix.(v) |> Seq.filter (fun (_, w) -> w > 0)

  (* Merge edge v into u *)
  let merge_edges g u v =
    (* Remove uv first to avoid counting it *)
    remove_edge g u v;
    edges_from_seq g v
    |> Seq.iter (fun (x, w) ->
           add_edge g u x w;
           remove_edge g v x);
    g.names_by_vertex.(u) <-
      (* Order doesn't matter and rev_append is faster than append *)
      List.rev_append g.names_by_vertex.(u) g.names_by_vertex.(v);
    g.names_by_vertex.(v) <- [];
    List.iter
      (fun v_name -> NameHash.replace g.vertex_by_name v_name u)
      g.names_by_vertex.(v);
    g.size <- g.size - 1;
    ()
end

let parse_line line =
  match String.split_on_char ':' line with
  | [ c; conns ] -> (c, String.trim conns |> String.split_on_char ' ')
  | _ -> failwith "Invalid line"

let read_input file_name =
  In_channel.with_open_text file_name (fun ch ->
      let entries = In_channel.input_lines ch |> List.map parse_line in
      let vertex_by_name = NameHash.create (List.length entries) in
      let add_vertex name =
        match NameHash.find_opt vertex_by_name name with
        | Some _exists -> ()
        | None ->
            NameHash.add vertex_by_name name (NameHash.length vertex_by_name)
      in
      let () =
        List.iter
          (fun (base, conns) ->
            add_vertex base;
            List.iter add_vertex conns)
          entries
      in
      let graph = Graph.create vertex_by_name in
      let add_edge u_name v_name =
        Graph.add_edge graph
          (Graph.get_vertex_by_name graph u_name)
          (Graph.get_vertex_by_name graph v_name)
          1 (* Initial weight is always 1 *)
      in
      let () =
        List.iter
          (fun (base, conns) ->
            List.iter (fun conn -> add_edge base conn) conns)
          entries
      in
      graph)

type cut = {
  between : Graph.vertex * Graph.vertex;
  weight : int;
  partitions : int * int;
}

let find_min_cut graph init =
  assert (Graph.size graph > 1);
  let weigts_to_subset = Array.copy (Graph.get_edge_weights graph init) in
  let s, t = (ref init, ref init) in
  for _ = 2 to Graph.size graph do
    weigts_to_subset.(!t) <- Int.min_int;
    let tightest_vertex =
      Array.to_seqi weigts_to_subset
      |> Seq.fold_left
           (fun (max_v, max_w) (v, w) ->
             if w > max_w then (v, w) else (max_v, max_w))
           (!s, 0)
      |> fst
    in
    Graph.edges_from_seq graph tightest_vertex
    |> Seq.iter (fun (v', w) ->
           weigts_to_subset.(v') <- weigts_to_subset.(v') + w);
    s := !t;
    t := tightest_vertex
  done;
  let t_size = Graph.get_vertex_compound_size graph !t in
  let rest_size = Graph.compound_size graph - t_size in
  let weight =
    Graph.edges_from_seq graph !t |> Seq.fold_left (fun acc (_, w) -> acc + w) 0
  in
  { between = (!s, !t); weight; partitions = (t_size, rest_size) }

let find_global_min_cut graph =
  assert (Graph.compound_size graph > 1);
  let best_cut =
    ref
      {
        between = (0, 0);
        weight = Graph.compound_size graph;
        partitions = (0, 0);
      }
  in
  for i = 2 to Graph.compound_size graph do
    let local_cut = find_min_cut graph 0 in
    if local_cut.weight < !best_cut.weight then best_cut := local_cut;
    let s, t = local_cut.between in
    Printf.fprintf Out_channel.stderr "[%d] Merge %d, %d\n" i s t;
    Out_channel.flush stderr;
    Graph.merge_edges graph s t
  done;
  !best_cut

let part1 input =
  let cut = find_global_min_cut input in
  let a, b = cut.partitions in
  a * b

let () =
  let input = read_input "input/day25.txt" in
  Printf.printf "Part1: %d\n%!" (part1 input)
