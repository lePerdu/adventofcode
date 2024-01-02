type t = { row : int; col : int }

let dist c1 c2 = Int.abs (c1.row - c2.row) + Int.abs (c1.col - c2.col)
let map f c = { row = f c.row; col = f c.col }
let map2 f a b = { row = f a.row b.row; col = f a.col b.col }
let add = map2 ( + )
let sub = map2 ( - )
let neg = map Int.neg
let scale n = map (Int.mul n)
let min_coord = { row = Int.min_int; col = Int.min_int }
let max_coord = { row = Int.max_int; col = Int.max_int }
let bound_max = map2 Int.max
let bound_min = map2 Int.min
let equal c1 c2 = c1.row == c2.row && c1.col == c2.col

let hash { row; col } =
  let r = Int.hash row in
  let c = Int.hash col in
  (r * 17) + c

let pp ch { row; col } = Printf.fprintf ch "(%d, %d)" row col
let show () { row; col } = Printf.sprintf "(%d, %d)" row col

type dir = Up | Down | Left | Right

let dir_equal = ( == )
let dir_hash d = d |> Obj.repr |> Obj.tag |> Int.hash
let all_dirs = [ Up; Down; Left; Right ]

let adjacent { row; col } =
  [
    { row = row - 1; col };
    { row = row + 1; col };
    { row; col = col - 1 };
    { row; col = col + 1 };
  ]

let add_dir_n { row; col } d n =
  match d with
  | Up -> { row = row - n; col }
  | Down -> { row = row + n; col }
  | Left -> { row; col = col - n }
  | Right -> { row; col = col + n }

let add_dir c d = add_dir_n c d 1

let dir_opposite = function
  | Up -> Down
  | Down -> Up
  | Left -> Right
  | Right -> Left

let dir_between a b =
  if a.col == b.col then
    if a.row + 1 == b.row then Some Up
    else if a.row - 1 == b.row then Some Down
    else None
  else if a.row == b.row then
    if a.col + 1 == b.col then Some Left
    else if a.col - 1 == b.col then Some Right
    else None
  else None

let dir_name = function
  | Up -> "up"
  | Down -> "down"
  | Left -> "left"
  | Right -> "right"
