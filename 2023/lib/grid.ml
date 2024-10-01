type 'a t = { data : 'a array array }

let make row_count col_count fill =
  { data = Array.make_matrix row_count col_count fill }

let map f g = { data = Array.map (Array.map f) g.data }

let mapi f g =
  {
    data =
      Array.mapi
        (fun rowi ->
          Array.mapi (fun coli cell -> f { Coord.row = rowi; col = coli } cell))
        g.data;
  }

let copy g = map Fun.id g
let rows g = Array.length g.data
let cols g = Array.length g.data.(0)

let in_bounds g ({ row; col } : Coord.t) =
  0 <= row && row < rows g && 0 <= col && col < cols g

let size g : Coord.t = { row = rows g; col = cols g }
let max_coord g : Coord.t = { row = rows g - 1; col = cols g - 1 }
let get g ({ row; col } : Coord.t) = g.data.(row).(col)
let set g ({ row; col } : Coord.t) v = g.data.(row).(col) <- v

let to_seqi g =
  Array.to_seqi g.data
  |> Seq.flat_map (fun (row_index, row) ->
         Array.to_seqi row
         |> Seq.map (fun (col_index, cell) ->
                ({ Coord.row = row_index; col = col_index }, cell)))

let rows_seq g = Seq.init (rows g) Fun.id
let cols_seq g = Seq.init (cols g) Fun.id

let input of_char ch =
  let convert_char c =
    match of_char c with
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

let input_file of_char path = In_channel.with_open_text path (input of_char)

let output to_char ch g =
  for row = 0 to rows g - 1 do
    for col = 0 to cols g - 1 do
      Out_channel.output_char ch (to_char (get g { row; col }))
    done;
    Out_channel.output_char ch '\n'
  done

let print to_char = output to_char stdout
