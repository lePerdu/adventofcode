type coord = { row : int; col : int }

let coord_dist c1 c2 = Int.abs (c1.row - c2.row) + Int.abs (c1.col - c2.col)

module type CellType = sig
  type t

  val of_char : char -> t option
  val to_char : t -> char
end

module type G = sig
  type cell
  type t

  val make : int -> int -> cell -> t
  val copy : t -> t
  val rows : t -> int
  val cols : t -> int
  val get : t -> coord -> cell
  val set : t -> coord -> cell -> unit
  val to_seqi : t -> (coord * cell) Seq.t
  val rows_seq : t -> int Seq.t
  val cols_seq : t -> int Seq.t
  val input : in_channel -> t
  val print : t -> unit
end

module Make (Cell : CellType) : G with type cell = Cell.t = struct
  type cell = Cell.t
  type t = { data : cell array array }

  let make row_count col_count fill =
    { data = Array.make_matrix row_count col_count fill }

  let copy g = { data = Array.map Array.copy g.data }
  let rows g = Array.length g.data
  let cols g = Array.length g.data.(0)
  let get g { row; col } = g.data.(row).(col)
  let set g { row; col } v = g.data.(row).(col) <- v

  let to_seqi g =
    Array.to_seqi g.data
    |> Seq.flat_map (fun (row_index, row) ->
           Array.to_seqi row
           |> Seq.map (fun (col_index, cell) ->
                  ({ row = row_index; col = col_index }, cell)))

  let rows_seq g = Seq.init (rows g) Fun.id
  let cols_seq g = Seq.init (cols g) Fun.id

  let input ch =
    let convert_char c =
      match Cell.of_char c with
      | Some cell -> cell
      | None -> failwith @@ Printf.sprintf "Invalid cell character: %C" c
    in
    let input_row () =
      match In_channel.input_line ch with
      | Some "" | None -> None
      | Some line ->
          String.to_seq line |> Seq.map convert_char |> Array.of_seq
          |> Option.some
    in
    let rec loop rows col_count =
      match input_row () with
      | Some next ->
          if Array.length next == col_count then loop (next :: rows) col_count
          else failwith "Rows of inconsistent length"
      | None -> rows
    in
    match input_row () with
    | Some first ->
        let rows =
          loop [ first ] (Array.length first) |> List.rev |> Array.of_list
        in
        { data = rows }
    | None -> (* Only raise if the first line is missing *) raise End_of_file

  let print g =
    for row = 0 to rows g - 1 do
      for col = 0 to cols g - 1 do
        print_char (Cell.to_char (get g { row; col }))
      done;
      print_newline ()
    done
end
