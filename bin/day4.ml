type card = { id : int; winning : int list; given : int list }

let parse_numbers str =
  str |> String.trim |> String.split_on_char ' '
  |> List.filter (fun s -> String.length s > 0)
  |> List.map int_of_string

let read_input file_name =
  Scanf.Scanning.(
    let ch = open_in file_name in
    Fun.protect
      ~finally:(fun () -> close_in ch)
      (fun () ->
        let rec loop () =
          if end_of_input ch then []
          else
            let next =
              Scanf.bscanf ch "Card %d: %[0-9 ] | %[0-9 ]\n"
                (fun id winning_str given_str ->
                  {
                    id;
                    winning = parse_numbers winning_str;
                    given = parse_numbers given_str;
                  })
            in
            next :: loop ()
        in
        loop ()))

let card_matches card =
  List.filter (fun given -> List.memq given card.winning) card.given

let card_match_count card = List.length (card_matches card)

let card_score card =
  let n = card_match_count card in
  if n <= 0 then 0 else Int.shift_left 1 (n - 1)

let part1 input = input |> List.map card_score |> List.fold_left ( + ) 0

let part2 input =
  let n_cards = List.length input in
  let card_counts = Array.make n_cards 1 in
  List.iter
    (fun card ->
      let winning = card_match_count card in
      let card_index = card.id - 1 in
      let copies = card_counts.(card_index) in
      for i = card_index + 1 to min (card_index + winning) (n_cards - 1) do
        card_counts.(i) <- card_counts.(i) + copies
      done)
    input;
  Array.fold_left ( + ) 0 card_counts

let input = read_input "input/day4.txt";;

Printf.printf "Part1: %d\n" (part1 input);
Printf.printf "Part2: %d\n" (part2 input)
