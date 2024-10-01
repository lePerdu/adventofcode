type race = { time : int; record : int }

let rec read_ints ch =
  Scanf.kscanf ch (fun _ch _exn -> []) "%d " (fun i -> i :: read_ints ch)

let read_sec ch label =
  Scanf.bscanf ch (Scanf.format_from_string (label ^ ": ") "") ();
  read_ints ch

let read_input file_name =
  Scanf.Scanning.(
    let ch = open_in file_name in
    Fun.protect
      ~finally:(fun () -> close_in ch)
      (fun () ->
        let times = read_sec ch "Time" in
        let distances = read_sec ch "Distance" in
        if List.length times == List.length distances then
          List.combine times distances
          |> List.map (fun (t, d) -> { time = t; record = d })
        else failwith "Time/Distance count mismatch"))

let calc_distance ~race_time ~button_time =
  (race_time - button_time) * button_time

let count_better_than race =
  Seq.init race.time Fun.id
  |> Seq.map (fun t -> calc_distance ~race_time:race.time ~button_time:t)
  |> Seq.filter (fun dist -> dist > race.record)
  |> Seq.length

let part1 input = input |> List.map count_better_than |> List.fold_left ( * ) 1

let part2_combine_races races =
  let int_concat_map f =
    races
    |> List.map (fun r -> string_of_int (f r))
    |> String.concat "" |> int_of_string
  in
  {
    time = int_concat_map (fun r -> r.time);
    record = int_concat_map (fun r -> r.record);
  }

let get_float_bounds race =
  let t = float_of_int race.time in
  let d = float_of_int race.record in
  let discrim = sqrt ((t *. t) -. (4.0 *. d)) in
  ((t -. discrim) /. 2.0, (t +. discrim) /. 2.0)

let get_win_range race =
  let low, high = get_float_bounds race in
  let low = int_of_float (ceil low) in
  let high = int_of_float (floor high) in
  high - low + 1

let part2 input =
  let single_race = part2_combine_races input in
  get_win_range single_race

let () =
  let input = read_input "input/day6.txt" in
  Printf.printf "Part1: %d\n" (part1 input);
  Printf.printf "Part2: %d\n" (part2 input)
