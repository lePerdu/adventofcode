type 'a t

val make : int -> int -> 'a -> 'a t
val copy : 'a t -> 'a t
val mapi : (Coord.t -> 'a -> 'b) -> 'a t -> 'b t
val map : ('a -> 'b) -> 'a t -> 'b t
val rows : 'a t -> int
val cols : 'a t -> int
val in_bounds : 'a t -> Coord.t -> bool
val size : 'a t -> Coord.t
val max_coord : 'a t -> Coord.t
val get : 'a t -> Coord.t -> 'a
val set : 'a t -> Coord.t -> 'a -> unit
val to_seqi : 'a t -> (Coord.t * 'a) Seq.t
val rows_seq : 'a t -> int Seq.t
val cols_seq : 'a t -> int Seq.t
val input : (char -> 'a option) -> in_channel -> 'a t
val input_file : (char -> 'a option) -> string -> 'a t
val output : ('a -> char) -> out_channel -> 'a t -> unit
val print : ('a -> char) -> 'a t -> unit
