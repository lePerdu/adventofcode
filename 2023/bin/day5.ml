type seed = int
type range_mapping = { dst : int; src : int; len : int }
type almanac_mapping = { _name : string; data : range_mapping list }
type almanac = { seeds : seed list; sections : almanac_mapping list }

let scan_seeds ch =
  Scanf.bscanf ch "seeds: " ();
  let rec loop () =
    Scanf.kscanf ch
      (fun _ch exn ->
        match exn with Scanf.Scan_failure _message -> [] | _ -> raise exn)
      "%d "
      (fun n -> n :: loop ())
  in
  let seeds = loop () in
  Scanf.bscanf ch " " ();
  seeds

let scan_mapping ch =
  Scanf.bscanf ch "%d %d %d\n" (fun dst src len -> { dst; src; len })

let rec scan_mappings ch =
  try
    let next = scan_mapping ch in
    next :: scan_mappings ch
  with
  | Scanf.Scan_failure _message ->
      Scanf.bscanf ch " " ();
      []
  | End_of_file -> []

let scan_section ch =
  let name = Scanf.bscanf ch "%s map:\n" Fun.id in
  let mappings = scan_mappings ch in
  { _name = name; data = mappings }

let rec scan_sections ch =
  try
    let next = scan_section ch in
    next :: scan_sections ch
  with End_of_file -> []

let scan_almanac ch =
  let seeds = scan_seeds ch in
  let sections = scan_sections ch in
  { seeds; sections }

let read_input file_name =
  Scanf.Scanning.(
    let ch = open_in file_name in
    Fun.protect ~finally:(fun () -> close_in ch) (fun () -> scan_almanac ch))

let range_contains r n = r.src <= n && n < r.src + r.len

let section_lookup section n =
  section.data
  |> List.find_map (fun range ->
         if range_contains range n then Some (range.dst - range.src + n)
         else None)
  |> Option.value ~default:n

let almanac_lookup almanac n =
  List.fold_left (fun n section -> section_lookup section n) n almanac.sections

let part1 input =
  input.seeds |> List.map (almanac_lookup input) |> List.fold_left min max_int

type seed_range = { start : int; len : int }

type part2_almanac = {
  seeds : seed_range list;
  sections : almanac_mapping list;
}

let rec split_pairs = function
  | [] -> []
  | x :: y :: rest -> (x, y) :: split_pairs rest
  | _ -> failwith "Odd number of elements"

let part2_almanac_of_part1 (original : almanac) =
  {
    seeds =
      original.seeds |> split_pairs
      |> List.map (fun (start, len) -> { start; len });
    sections = original.sections;
  }

type partial_mapping = {
  unmapped : seed_range list;
  mapped : seed_range option;
}

let make_range range_start range_end =
  { start = range_start; len = range_end - range_start }

let offset_range range offset = { range with start = range.start + offset }

let range_intersect map_range id_range =
  let { dst = dst_start; src = src_start; len = map_len } = map_range in
  let { start = id_start; len = id_len } = id_range in
  let src_end = src_start + map_len in
  let id_end = id_start + id_len in
  let offset = dst_start - src_start in
  let make_offset_range range_start range_end =
    offset_range (make_range range_start range_end) offset
  in
  if src_start <= id_start && id_end <= src_end then
    (* Map fully contains ID *)
    { unmapped = []; mapped = Some (offset_range id_range offset) }
  else if id_start < src_start && src_end < id_end then
    (* ID fully contains map *)
    {
      unmapped = [ make_range id_start src_start; make_range src_end id_end ];
      mapped = Some { start = dst_start; len = map_len };
    }
  else if id_start <= src_start && src_start < id_end then
    (* Low overlap *)
    {
      unmapped = [ make_range id_start src_start ];
      mapped = Some (make_offset_range src_start id_end);
    }
  else if id_start < src_end && src_end <= id_end then
    (* High overlap *)
    {
      unmapped = [ make_range src_end id_end ];
      mapped = Some (make_offset_range id_start src_end);
    }
  else (* No overlap *)
    { unmapped = [ id_range ]; mapped = None }

let opt_cons opt l = Option.fold ~none:l ~some:(fun x -> x :: l) opt

let range_intersect_all map_range ranges =
  ranges
  |> List.map (range_intersect map_range)
  |> List.fold_left
       (fun (unmapped, mapped) partial ->
         (partial.unmapped @ unmapped, opt_cons partial.mapped mapped))
       ([], [])

let section_lookup_range section id_range =
  let rec fold unmapped mapped mappings =
    match unmapped with
    | [] -> mapped
    | unmapped -> (
        match mappings with
        | [] ->
            (* All remaining ranges need no mapping *)
            unmapped @ mapped
        | next_map :: mappings' ->
            let new_unmapped, new_mapped =
              range_intersect_all next_map unmapped
            in
            fold new_unmapped (new_mapped @ mapped) mappings')
  in
  fold [ id_range ] [] section.data

let almanac_lookup_range (almanac : part2_almanac) id_range =
  List.fold_left
    (fun acc sect -> List.concat_map (section_lookup_range sect) acc)
    [ id_range ] almanac.sections

let part2 input =
  let input = part2_almanac_of_part1 input in
  input.seeds
  |> List.concat_map (almanac_lookup_range input)
  |> List.map (fun x -> x.start)
  |> List.fold_left min max_int

let () =
  let input = read_input "input/day5.txt" in
  Printf.printf "Part1: %d\n" (part1 input);
  Printf.printf "Part2: %d\n" (part2 input)
