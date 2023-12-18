type spring_status = Working | Damaged | Unknown

type condition_record = {
  statuses : spring_status list;
  damaged_groups : int list;
}

let status_of_char = function
  | '.' -> Working
  | '#' -> Damaged
  | '?' -> Unknown
  | c -> failwith @@ Printf.sprintf "Invalid spring status: %C" c

let parse_record line =
  match String.split_on_char ' ' line with
  | [ statuses; groups ] ->
      {
        statuses =
          String.to_seq statuses |> Seq.map status_of_char |> List.of_seq;
        damaged_groups =
          String.split_on_char ',' groups |> List.map int_of_string;
      }
  | _ -> failwith "Failed to parse record"

let read_input file_name =
  In_channel.with_open_text file_name (fun ch ->
      In_channel.input_lines ch |> List.map parse_record)

let fix_count r =
  let cache = Hashtbl.create (List.length r.statuses) in
  let rec helper r cur_group =
    let key = (r, cur_group) in
    match Hashtbl.find_opt cache key with
    | Some existing -> existing
    | None ->
        let resolved =
          match (r.statuses, r.damaged_groups, cur_group) with
          | [], [], 0 -> 1
          | [], [ expect_last ], _ -> if cur_group == expect_last then 1 else 0
          | [], _, _ -> 0 (* More expectations than groups *)
          | Working :: rest_statuses, _, 0 ->
              helper { r with statuses = rest_statuses } 0
          | Working :: _, [], _ -> 0
          | Working :: rest_statuses, expect_group :: rest_groups, _ ->
              if cur_group == expect_group then
                helper
                  { statuses = rest_statuses; damaged_groups = rest_groups }
                  0
              else 0
          | Damaged :: _, [], _ -> 0
          | Damaged :: rest_statuses, expect_group :: _, _ ->
              if cur_group < expect_group then
                helper { r with statuses = rest_statuses } (cur_group + 1)
              else 0
          | Unknown :: rest_statuses, _, _ ->
              let recur_with leading =
                helper { r with statuses = leading :: rest_statuses } cur_group
              in
              recur_with Working + recur_with Damaged
        in
        Hashtbl.add cache key resolved;
        resolved
  in
  helper r 0

let part1 input = input |> List.map fix_count |> List.fold_left ( + ) 0
let part2_repeat_count = 5

let expand_record { statuses; damaged_groups } =
  {
    statuses =
      Seq.interleave
        (Seq.init part2_repeat_count (fun _ -> List.to_seq statuses))
        (Seq.init (part2_repeat_count - 1) (fun _ -> Seq.return Unknown))
      |> Seq.flat_map Fun.id |> List.of_seq;
    damaged_groups =
      List.init part2_repeat_count (fun _ -> damaged_groups) |> List.concat;
  }

let part2 input =
  List.to_seq input |> Seq.map expand_record |> Seq.map fix_count
  |> Seq.fold_left ( + ) 0

let () = Advent.Main.run "input/day12.txt" read_input part1 part2
