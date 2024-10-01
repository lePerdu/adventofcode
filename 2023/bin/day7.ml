type card =
  | C2
  | C3
  | C4
  | C5
  | C6
  | C7
  | C8
  | C9
  | C10
  | Jack
  | Queen
  | King
  | Ace

type 'card hand_type =
  | HighCard
  | OnePair of 'card
  | TwoPair of ('card * 'card)
  | ThreeKind of 'card
  | FullHouse of ('card * 'card)
  | FourKind of 'card
  | FiveKind of 'card

let card_of_char = function
  | '2' -> C2
  | '3' -> C3
  | '4' -> C4
  | '5' -> C5
  | '6' -> C6
  | '7' -> C7
  | '8' -> C8
  | '9' -> C9
  | 'T' -> C10
  | 'J' -> Jack
  | 'Q' -> Queen
  | 'K' -> King
  | 'A' -> Ace
  | invalid ->
      raise
      @@ Invalid_argument (Printf.sprintf "Invalid card character: %C" invalid)

let card_counts cards =
  cards |> List.fast_sort compare |> List.to_seq |> Seq.group ( == )
  |> Seq.map (fun group ->
         let c, rest = Option.get (Seq.uncons group) in
         (c, Seq.length rest + 1))
  |> List.of_seq
  |> List.fast_sort (fun (_, a) (_, b) -> compare b a)

let match_hand_type = function
  | [ (c, 5) ] -> FiveKind c
  | [ (c, 4); _ ] -> FourKind c
  | [ (c3, 3); (c2, 2) ] -> FullHouse (c3, c2)
  | [ (c, 3); _; _ ] -> ThreeKind c
  | [ (a, 2); (b, 2); _ ] -> TwoPair (a, b)
  | [ (c, 2); _; _; _ ] -> OnePair c
  | _ -> HighCard

let get_hand_type cards = match_hand_type @@ card_counts cards

let hand_type_rank = function
  | HighCard -> 0
  | OnePair _ -> 1
  | TwoPair _ -> 2
  | ThreeKind _ -> 3
  | FullHouse _ -> 4
  | FourKind _ -> 5
  | FiveKind _ -> 6

type 'card typed_hand = {
  cards : 'card list;
  hand_type : 'card hand_type;
  bid_amount : int;
}

let hand_compare h1 h2 =
  let make h = (hand_type_rank h.hand_type, h.cards) in
  compare (make h1) (make h2)

type bid = { cards : card list; amount : int }

let read_bid ch =
  Scanf.bscanf ch "%[2-9TJQKA] %d\n" (fun cards amount ->
      assert (String.length cards == 5);
      {
        cards = cards |> String.to_seq |> Seq.map card_of_char |> List.of_seq;
        amount;
      })

let read_input file_name =
  Scanf.Scanning.(
    let ch = open_in file_name in
    Fun.protect
      ~finally:(fun () -> close_in ch)
      (fun () ->
        let rec loop () =
          try
            let next = read_bid ch in
            next :: loop ()
          with End_of_file -> []
        in
        loop ()))

let to_typed_hand bid =
  {
    cards = bid.cards;
    hand_type = get_hand_type bid.cards;
    bid_amount = bid.amount;
  }

let part1 input =
  input |> List.map to_typed_hand
  |> List.fast_sort hand_compare
  |> List.mapi (fun index hand ->
         let rank = index + 1 in
         rank * hand.bid_amount)
  |> List.fold_left ( + ) 0

type part2_card =
  | Joker
  | C2
  | C3
  | C4
  | C5
  | C6
  | C7
  | C8
  | C9
  | C10
  | Queen
  | King
  | Ace

let part2_card_of_part1 : card -> part2_card = function
  | C2 -> C2
  | C3 -> C3
  | C4 -> C4
  | C5 -> C5
  | C6 -> C6
  | C7 -> C7
  | C8 -> C8
  | C9 -> C9
  | C10 -> C10
  | Jack -> Joker
  | Queen -> Queen
  | King -> King
  | Ace -> Ace

let get_part2_hand_type cards =
  let counted = card_counts cards in
  let jokers, rest = List.partition (fun (c, _n) -> c == Joker) counted in
  let joker_count =
    jokers |> List.map (fun (_, n) -> n) |> List.fold_left ( + ) 0
  in
  (* Matching the jokers with the most common card will lead to the best hand *)
  let with_wilds =
    match rest with
    | [] -> [ (Joker, joker_count) ]
    | (top_card, top_count) :: others ->
        (top_card, top_count + joker_count) :: others
  in
  match_hand_type with_wilds

let to_part2_typed_hand bid =
  let part2_cards = List.map part2_card_of_part1 bid.cards in
  {
    cards = part2_cards;
    hand_type = get_part2_hand_type part2_cards;
    bid_amount = bid.amount;
  }

let part2 input =
  input
  |> List.map to_part2_typed_hand
  |> List.fast_sort hand_compare
  |> List.mapi (fun index hand ->
         let rank = index + 1 in
         rank * hand.bid_amount)
  |> List.fold_left ( + ) 0

let () =
  let input = read_input "input/day7.txt" in
  Printf.printf "Part1: %d\n" (part1 input);
  Printf.printf "Part2: %d\n" (part2 input)
