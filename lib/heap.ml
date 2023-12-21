module type OrderedType = Set.OrderedType

module type S = sig
  type elt
  type t

  val empty : t
  val singleton : elt -> t
  val is_empty : t -> bool
  val add : elt -> t -> t
  val min_elt : t -> elt option
  val remove_min : t -> t
  val pop_min : t -> (elt * t) option
  val of_seq : elt Seq.t -> t
  val to_seq : t -> elt Seq.t
end

module Make (Ord : OrderedType) : S with type elt = Ord.t = struct
  type elt = Ord.t
  type t = Empty | Node of elt * t * t

  let empty = Empty
  let singleton x = Node (x, Empty, Empty)
  let is_empty h = h == Empty

  let rec add x = function
    | Empty -> Node (x, Empty, Empty)
    | Node (top, left, right) ->
        let new_parent, new_child =
          if Ord.compare x top < 0 then (x, top) else (top, x)
        in
        if Random.bool () then Node (new_parent, add new_child left, right)
        else Node (new_parent, left, add new_child right)

  let min_elt = function Empty -> None | Node (top, _, _) -> Some top

  let rec remove_min = function
    | Empty -> raise Not_found
    | Node (_, Empty, right) -> right
    | Node (_, left, Empty) -> left
    | Node (_, (Node (x, _, _) as left), (Node (y, _, _) as right)) ->
        if Ord.compare x y < 0 then Node (x, remove_min left, right)
        else Node (y, left, remove_min right)

  let pop_min h = min_elt h |> Option.map (fun top -> (top, remove_min h))
  let of_seq = Seq.fold_left (Fun.flip add) empty

  let rec to_seq h () =
    match pop_min h with
    | Some (x, rest) -> Seq.Cons (x, to_seq rest)
    | None -> Seq.Nil
end
