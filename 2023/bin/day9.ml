let hist_deltas hist =
  let hist = Array.to_seq hist in
  Seq.zip (Seq.drop 1 hist) hist
  |> Seq.map (fun (next, prev) -> next - prev)
  |> Array.of_seq

let all_0 = Array.for_all (fun x -> x == 0)

let rec hist_predict_next hist =
  if all_0 hist then 0
  else
    let deltas = hist_deltas hist in
    let next_delta = hist_predict_next deltas in
    hist.(Array.length hist - 1) + next_delta

let rec hist_predict_prev hist =
  if all_0 hist then 0
  else
    let deltas = hist_deltas hist in
    let prev_delta = hist_predict_prev deltas in
    hist.(0) - prev_delta

let part1 input = input |> List.map hist_predict_next |> List.fold_left ( + ) 0
let part2 input = input |> List.map hist_predict_prev |> List.fold_left ( + ) 0

let read_input file_name =
  let rec loop ch =
    match In_channel.input_line ch with
    | Some line ->
        let next_hist =
          line |> String.split_on_char ' ' |> List.map int_of_string
          |> Array.of_list
        in
        next_hist :: loop ch
    | None -> []
  in
  In_channel.with_open_text file_name loop

let () =
  let input = read_input "input/day9.txt" in
  Printf.printf "Part1: %d\n" (part1 input);
  Printf.printf "Part2: %d\n" (part2 input)
