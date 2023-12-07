type color = Red | Green | Blue
type game_id = int
type reveal_set = { red : int; green : int; blue : int }
type game = { id : game_id; sets : reveal_set list }

let color_of_string = function
  | "red" -> Red
  | "green" -> Green
  | "blue" -> Blue
  | _ -> raise (Failure "invalid color")

let parse_single_reveal str =
  match str |> String.trim |> String.split_on_char ' ' with
  | [ n_str; color_str ] -> (color_of_string color_str, int_of_string n_str)
  | _ -> raise (Failure "invalid reveal pair")

let parse_reveal_set str =
  let groups = String.split_on_char ',' str in
  let pairs = List.map parse_single_reveal groups in
  let count_color c = List.assoc_opt c pairs |> Option.value ~default:0 in
  { red = count_color Red; green = count_color Green; blue = count_color Blue }

let parse_game line =
  match String.split_on_char ':' line with
  | [ game_desc; sets_str ] -> (
      match String.split_on_char ' ' game_desc with
      | [ "Game"; id_str ] ->
          let id = int_of_string id_str in
          {
            id;
            sets = List.map parse_reveal_set (String.split_on_char ';' sets_str);
          }
      | _ -> raise (Failure "Invalid game descriptor"))
  | _ -> raise (Failure "Missing game descriptor")

let read_input file =
  In_channel.with_open_text file (fun ch ->
      let rec loop () =
        match In_channel.input_line ch with
        | Some line -> parse_game line :: loop ()
        | None -> []
      in
      loop ())

let part1_target = { red = 12; green = 13; blue = 14 }

let reveal_set_possible ~target s =
  target.red >= s.red && target.green >= s.green && target.blue >= s.blue

let game_possible ~target game =
  List.for_all (fun s -> reveal_set_possible ~target s) game.sets

let part1 input =
  input
  |> List.filter (game_possible ~target:part1_target)
  |> List.map (fun g -> g.id)
  |> List.fold_left ( + ) 0

let game_lowest_counts game =
  let max_extracting f = game.sets |> List.map f |> List.fold_left max 0 in
  {
    red = max_extracting (fun s -> s.red);
    green = max_extracting (fun s -> s.green);
    blue = max_extracting (fun s -> s.blue);
  }

let reveal_set_power s = s.red * s.green * s.blue

let part2 input =
  input
  |> List.map (fun g -> reveal_set_power (game_lowest_counts g))
  |> List.fold_left ( + ) 0
;;

let input = read_input "input/day2.txt" in
Printf.printf "Part1: %d\n" (part1 input);
Printf.printf "Part2: %d\n" (part2 input)
