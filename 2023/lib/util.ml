type digit = int

let digit_of_char c : digit option =
  if '0' <= c && c <= '9' then Some (Char.code c - Char.code '0') else None

let char_of_digit (n : digit) : char =
  if 0 <= n && n <= 9 then Char.chr (n + Char.code '0')
  else failwith @@ Printf.sprintf "Invalid digit: %d" n
