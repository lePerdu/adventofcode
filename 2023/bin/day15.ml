let read_input file_name =
  In_channel.with_open_text file_name In_channel.input_all
  |> String.trim (* Remove newline at the end in case it's present *)
  |> String.split_on_char ','

let hash_alg s =
  let hash = String.fold_left (fun acc ch -> (acc + Char.code ch) * 17) 0 s in
  hash mod 256

let part1 input = input |> List.map hash_alg |> List.fold_left ( + ) 0

type hashmap_op = Insert of (string * int) | Remove of string

let hashmap_op_of_string s =
  if String.ends_with ~suffix:"-" s then
    Remove (String.sub s 0 (String.length s - 1))
  else
    match String.split_on_char '=' s with
    | [ label; focal_str ] -> Insert (label, int_of_string focal_str)
    | _ -> failwith @@ Printf.sprintf "Invalid hashmap operation: %S" s

type hashmap_lens = { label : string; mutable focal_length : int }
type hashmap_box = { mutable lenses : hashmap_lens list }
type hahsmap = { boxes : hashmap_box array }

let hashmap_create () = { boxes = Array.init 256 (fun _ -> { lenses = [] }) }

let hashmap_box_insert box label focal_length =
  match List.find_opt (fun l -> l.label = label) box.lenses with
  | Some found -> found.focal_length <- focal_length
  | None -> box.lenses <- { label; focal_length } :: box.lenses

let hashmap_box_remove box label =
  box.lenses <- List.filter (fun l -> not (l.label = label)) box.lenses

let hashmap_insert hm label focal_length =
  let index = hash_alg label in
  hashmap_box_insert hm.boxes.(index) label focal_length

let hashmap_remove hm label =
  let index = hash_alg label in
  hashmap_box_remove hm.boxes.(index) label

let box_focusing_power box =
  let _, total =
    List.fold_right
      (fun lens (slot, acc) -> (slot + 1, acc + (lens.focal_length * slot)))
      box.lenses (1, 0)
  in
  total

let hashmap_focusing_power hm =
  Array.to_seq hm.boxes
  |> Seq.mapi (fun index box -> box_focusing_power box * (index + 1))
  |> Seq.fold_left ( + ) 0

let part2 input =
  let ops = List.map hashmap_op_of_string input in
  let hm = hashmap_create () in
  List.iter
    (function
      | Insert (label, foc) -> hashmap_insert hm label foc
      | Remove label -> hashmap_remove hm label)
    ops;
  hashmap_focusing_power hm

let () = Advent.Main.run "input/day15.txt" read_input part1 part2
