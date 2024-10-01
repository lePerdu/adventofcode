module Grid = Advent.Grid

type v2 = { x : int; y : int }
type v3 = { x : int; y : int; z : int }
type brick_bounds = v3 * v3
type input = brick_bounds list

let parse_v3 s =
  let coords = String.split_on_char ',' s |> List.map int_of_string in
  assert (List.length coords == 3);
  let v =
    { x = List.nth coords 0; y = List.nth coords 1; z = List.nth coords 2 }
  in
  assert (v.x >= 0);
  assert (v.y >= 0);
  assert (v.z >= 1);
  v

let parse_brick line =
  let ends = String.split_on_char '~' line in
  assert (List.length ends == 2);
  let a, b = (parse_v3 (List.nth ends 0), parse_v3 (List.nth ends 1)) in
  assert (a.x == b.x || a.y == b.y);
  (* Coordinates must be ordered *)
  assert (a.x <= b.x && a.y <= b.y && a.z <= b.z);
  (a, b)

let read_input file_name : input =
  In_channel.with_open_text file_name (fun ch ->
      In_channel.input_lines ch
      |> List.filter (fun line -> line <> "")
      |> List.map parse_brick)

let max_bounds input =
  List.to_seq input
  |> Seq.flat_map (fun (a, b) -> Seq.cons a (Seq.return b))
  |> Seq.fold_left
       (fun acc end_coord ->
         {
           x = max acc.x end_coord.x;
           y = max acc.y end_coord.y;
           z = max acc.z end_coord.z;
         })
       { x = 0; y = 0; z = 0 }

let make_grid3 bounds init =
  Array.init (bounds.z + 1) (fun _ ->
      Grid.make (bounds.x + 1) (bounds.y + 1) init)

type brick = { head : v3; tail : v3; id : int }

let make_brick id (a, b) = { id; head = a; tail = b }

let brick_xy_vals b =
  if b.head.x == b.tail.x then
    let len = b.tail.y - b.head.y + 1 in
    Array.init len (fun dy -> { x = b.head.x; y = b.head.y + dy })
  else
    let len = b.tail.x - b.head.x + 1 in
    Array.init len (fun dx -> { x = b.head.x + dx; y = b.head.y })

let brick_coords_seq b =
  if b.head.x != b.tail.x then
    let len = b.tail.x - b.head.x + 1 in
    Seq.init len (fun dx -> { b.head with x = b.head.x + dx })
  else if b.head.y != b.tail.y then
    let len = b.tail.y - b.head.y + 1 in
    Seq.init len (fun dy -> { b.head with y = b.head.y + dy })
  else
    let len = b.tail.z - b.head.z + 1 in
    Seq.init len (fun dz -> { b.head with z = b.head.z + dz })

module IdSet = Set.Make (Int)

let supports_at_level level xys =
  Array.to_seq xys
  |> Seq.filter_map (fun (v : v2) ->
         let id_at = Grid.get level { row = v.x; col = v.y } in
         if id_at < 0 then None else Some id_at)
  |> IdSet.of_seq

let find_supports levels brick =
  let brick_xys = brick_xy_vals brick in
  let z_init = brick.head.z - 1 in
  let rec loop z =
    if z == 0 then (-0, IdSet.empty)
    else
      let supports = supports_at_level levels.(z) brick_xys in
      if IdSet.is_empty supports then loop (z - 1) else (z, supports)
  in
  loop z_init

let place_brick levels brick z_base =
  let z_offset = z_base - brick.head.z in
  brick_coords_seq brick
  |> Seq.iter (fun c ->
         let level = levels.(z_offset + c.z) in
         Grid.set level { row = c.x; col = c.y } brick.id)

(*let ex = read_input "input/day22-example.txt"*)
(*let input = read_input "input/day22.txt"*)

let to_sorted_bricks input =
  let sorted_bricks =
    List.mapi (fun id bounds -> make_brick id bounds) input |> Array.of_list
  in
  Array.sort (fun b1 b2 -> b1.head.z - b2.head.z) sorted_bricks;
  sorted_bricks

let place_all_bricks bounds sorted_bricks =
  let levels = make_grid3 bounds (-1) in
  let supports_by_id = Array.make (Array.length sorted_bricks) IdSet.empty in
  Array.iter
    (fun brick ->
      let z_base, supports = find_supports levels brick in
      place_brick levels brick (z_base + 1);
      supports_by_id.(brick.id) <- supports)
    sorted_bricks;
  (levels, supports_by_id)

module IdMap = Map.Make (Int)

let find_critical_bricks supports_by_id =
  Array.fold_left
    (fun critical_ids supports ->
      if IdSet.cardinal supports == 1 then
        let new_crit = IdSet.choose supports in
        IdSet.add new_crit critical_ids
      else critical_ids)
    IdSet.empty supports_by_id

let part1 input =
  let sorted_bricks = to_sorted_bricks input in
  let _, supports_by_id = place_all_bricks (max_bounds input) sorted_bricks in
  let critical_ids = find_critical_bricks supports_by_id in
  Array.length sorted_bricks - IdSet.cardinal critical_ids

let find_supporting_bricks supports_by_id =
  let supporting_by_id = Array.make (Array.length supports_by_id) [] in
  Array.iteri
    (fun id supports ->
      IdSet.iter
        (fun supporter ->
          supporting_by_id.(supporter) <- id :: supporting_by_id.(supporter))
        supports)
    supports_by_id;
  (* Convert to array for quicker accesses *)
  Array.map Array.of_list supporting_by_id

let count_chain_reaction supporters_by_id supporting_by_id id =
  let queue = Queue.create () in
  Queue.add id queue;
  let rec loop fallen =
    match Queue.take_opt queue with
    | Some next ->
        let candidates = supporting_by_id.(next) in
        let new_fallen =
          Array.fold_left
            (fun acc_fallen cand ->
              let supporters = supporters_by_id.(cand) in
              if IdSet.for_all (Fun.flip IdSet.mem fallen) supporters then
                let () = Queue.add cand queue in
                IdSet.add cand acc_fallen
              else acc_fallen)
            fallen candidates
        in
        loop new_fallen
    | None -> fallen
  in
  let fallen = loop (IdSet.singleton id) in
  (* Don't count the original one removed *)
  IdSet.cardinal fallen - 1

let part2 input =
  let sorted_bricks = to_sorted_bricks input in
  let _, supports_by_id = place_all_bricks (max_bounds input) sorted_bricks in
  let supporting_by_id = find_supporting_bricks supports_by_id in
  Seq.init (Array.length sorted_bricks) (fun id ->
      count_chain_reaction supports_by_id supporting_by_id id)
  |> Seq.fold_left ( + ) 0

let () = Advent.Main.run "input/day22.txt" read_input part1 part2
