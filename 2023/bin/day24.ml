type 'a v3 = { x : 'a; y : 'a; z : 'a }
type 'a v2 = { x : 'a; y : 'a }
type state3 = { pos : int v3; vel : int v3 }

let parse_v3 s =
  match
    String.split_on_char ',' s
    |> List.map (fun part -> String.trim part |> int_of_string)
  with
  | [ x; y; z ] -> { x; y; z }
  | _ -> failwith "invalid v3"

let parse_state3 line =
  match String.split_on_char '@' line with
  | [ pos_str; vel_str ] -> { pos = parse_v3 pos_str; vel = parse_v3 vel_str }
  | _ -> failwith "invalid state3"

let read_input file_name =
  In_channel.with_open_text file_name (fun ch ->
      In_channel.input_lines ch |> List.map parse_state3)
  |> Array.of_list

type intersect_2d = { pos : float v2; t1 : float; t2 : float }

let find_xy_intersect (h1 : state3) (h2 : state3) : intersect_2d option =
  let t1 =
    float_of_int
      ((h2.vel.y * (h1.pos.x - h2.pos.x)) - (h2.vel.x * (h1.pos.y - h2.pos.y)))
    /. float_of_int ((h1.vel.y * h2.vel.x) - (h1.vel.x * h2.vel.y))
  in
  let t2 =
    float_of_int
      ((h1.vel.y * (h2.pos.x - h1.pos.x)) - (h1.vel.x * (h2.pos.y - h1.pos.y)))
    /. float_of_int ((h2.vel.y * h1.vel.x) - (h2.vel.x * h1.vel.y))
  in
  let pos =
    {
      x = float_of_int h1.pos.x +. (float_of_int h1.vel.x *. t1);
      y = float_of_int h1.pos.y +. (float_of_int h1.vel.y *. t1);
    }
  in
  if Float.is_finite t1 then Some { pos; t1; t2 } else None

let part1_min = 200000000000000.0
let part1_max = 400000000000000.0

let in_part1_range pos =
  part1_min <= pos.x && pos.x <= part1_max && part1_min <= pos.y
  && pos.y <= part1_max

let part1 input =
  Seq.init (Array.length input) Fun.id
  |> Seq.flat_map (fun i -> Seq.init i (fun j -> (input.(i), input.(j))))
  |> Seq.filter_map (fun (h1, h2) -> find_xy_intersect h1 h2)
  |> Seq.filter (fun intersect -> intersect.t1 >= 0.0 && intersect.t2 >= 0.0)
  |> Seq.filter (fun intersect -> in_part1_range intersect.pos)
  |> Seq.length

let vmap f (a : 'a v3) = { x = f a.x; y = f a.y; z = f a.z }

let vzip f (a : 'a v3) (b : 'a v3) =
  { x = f a.x b.x; y = f a.y b.y; z = f a.z b.z }

let vadd = vzip ( +. )
let vsub = vzip ( -. )
let vscale s = vmap (Float.mul s)
let vinv_scale s = vmap (fun v -> v /. s)

let vdot (a : float v3) (b : float v3) =
  (a.x *. b.x) +. (a.y *. b.y) +. (a.z *. b.z)

let vcross (a : float v3) (b : float v3) =
  {
    x = (a.y *. b.z) -. (a.z *. b.y);
    y = (a.z *. b.x) -. (a.x *. b.z);
    z = (a.x *. b.y) -. (a.y *. b.x);
  }

let v_to_float = vmap float_of_int
let v_round = vmap (fun v -> int_of_float (Float.round v))

let find_intersect_times (h0 : state3) (h1 : state3) (h2 : state3) =
  let v1 = vsub (v_to_float h1.vel) (v_to_float h0.vel) in
  let v2 = vsub (v_to_float h2.vel) (v_to_float h0.vel) in
  let d1 = vsub (v_to_float h1.pos) (v_to_float h0.pos) in
  let d2 = vsub (v_to_float h2.pos) (v_to_float h0.pos) in
  let d2x1 = vcross d2 d1 in
  let t1 = vdot d2x1 v2 /. vdot (vcross v1 d2) v2 in
  let t2 = vdot d2x1 v1 /. vdot (vcross d1 v2) v1 in
  (t1, t2)

let project (h : state3) (t : float) =
  vadd (v_to_float h.pos) (vscale t (v_to_float h.vel))

let find_rock_state (h0 : state3) (h1 : state3) (h2 : state3) =
  let t1, t2 = find_intersect_times h0 h1 h2 in
  let p1 = project h1 t1 in
  let p2 = project h2 t2 in
  let vr = vinv_scale (t2 -. t1) (vsub p2 p1) in
  let vp = vsub p1 (vscale t1 vr) in
  { pos = v_round vp; vel = v_round vr }

let part2 input =
  let rock = find_rock_state input.(0) input.(1) input.(2) in
  rock.pos.x + rock.pos.y + rock.pos.z

let () = Advent.Main.run "input/day24.txt" read_input part1 part2
