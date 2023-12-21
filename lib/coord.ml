type t = { row : int; col : int }

let dist c1 c2 = Int.abs (c1.row - c2.row) + Int.abs (c1.col - c2.col)
let add c1 c2 = { row = c1.row + c2.row; col = c1.col + c2.col }
let sub c1 c2 = { row = c1.row - c2.row; col = c1.col - c2.col }
let scale c n = { row = c.row * n; col = c.col * n }
let equal c1 c2 = c1.row == c2.row && c1.col == c2.col

let hash { row; col } =
  let r = Int.hash row in
  let c = Int.hash col in
  (r * 17) + c

let pp ch { row; col } = Printf.fprintf ch "(%d, %d)" row col

type dir = Up | Down | Left | Right

let dir_equal = ( == )
let dir_hash d = d |> Obj.repr |> Obj.tag |> Int.hash
let all_dirs = [ Up; Down; Left; Right ]

let adjacent_dirs { row; col } =
  [
    { row = row - 1; col };
    { row = row + 1; col };
    { row; col = col - 1 };
    { row; col = col + 1 };
  ]

let add_dir { row; col } = function
  | Up -> { row = row - 1; col }
  | Down -> { row = row + 1; col }
  | Left -> { row; col = col - 1 }
  | Right -> { row; col = col + 1 }

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
