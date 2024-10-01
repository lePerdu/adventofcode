type node_id = string
type module_type = Broadcast | FlipFlop | Conjunction

type module_desc = {
  module_type : module_type;
  id : node_id;
  outputs : node_id list;
}

let button_ident = "button"
let broadcaster_id = "broadcaster"
let rx_id = "rx"

let split_module_type id_str =
  let get_rest_id () = String.sub id_str 1 (String.length id_str - 1) in
  if id_str.[0] == '%' then (FlipFlop, get_rest_id ())
  else if id_str.[0] == '&' then (Conjunction, get_rest_id ())
  else if String.equal id_str broadcaster_id then (Broadcast, id_str)
  else failwith "Invalid module type/ID"

let split_node_ids ids = String.split_on_char ',' ids |> List.map String.trim
let module_desc_regexp = Str.regexp {|^\(.[a-z]+\) *-> *\(.*\)$|}

let module_desc_of_string s =
  if Str.string_match module_desc_regexp s 0 then
    let full_id = Str.matched_group 1 s in
    let outputs_str = Str.matched_group 2 s in
    let module_type, id = split_module_type full_id in
    let outputs = split_node_ids outputs_str in
    { module_type; id; outputs }
  else failwith "Invalid module description"

let read_input file_name =
  In_channel.with_open_text file_name (fun ch ->
      In_channel.input_lines ch |> List.map module_desc_of_string)

type pulse = Low | High

module NodeHashtbl = Hashtbl.Make (String)

type flip_flop_state = On | Off
type conjunction_state = pulse NodeHashtbl.t

type module_state =
  | Broadcast
  | FlipFlop of flip_flop_state ref
  | Conjunction of conjunction_state

type module_node = {
  id : node_id;
  outputs : node_id list;
  state : module_state;
}

type pending_pulse = { src : node_id; dst : node_id; value : pulse }

type system_state = {
  nodes : module_node NodeHashtbl.t;
  pulses : pending_pulse Queue.t;
  pulse_handler : pending_pulse -> unit;
}

let make_module_state (typ : module_type) module_inputs : module_state =
  match typ with
  | Broadcast -> Broadcast
  | FlipFlop -> FlipFlop (ref Off)
  | Conjunction ->
      let init_states =
        List.to_seq module_inputs
        |> Seq.map (fun input_id -> (input_id, Low))
        |> NodeHashtbl.of_seq
      in
      Conjunction init_states

let make_module_node ~id ~module_type ~module_inputs ~outputs =
  { id; outputs; state = make_module_state module_type module_inputs }

let get_inputs_table (descs : module_desc list) =
  let inputs_table = NodeHashtbl.create (List.length descs) in
  List.iter
    (fun (desc : module_desc) ->
      List.iter
        (fun output_id -> NodeHashtbl.add inputs_table output_id desc.id)
        desc.outputs)
    descs;
  inputs_table

let make_system (descs : module_desc list) ~pulse_handler =
  let inputs_table = get_inputs_table descs in
  let system_table = NodeHashtbl.create (NodeHashtbl.length inputs_table) in
  List.iter
    (fun (desc : module_desc) ->
      let module_inputs = NodeHashtbl.find_all inputs_table desc.id in
      let node =
        make_module_node ~id:desc.id ~module_type:desc.module_type
          ~module_inputs ~outputs:desc.outputs
      in
      NodeHashtbl.replace system_table desc.id node)
    descs;
  { nodes = system_table; pulses = Queue.create (); pulse_handler }

let flip_state = function On -> Off | Off -> On
let flip_flop_pulse = function On -> High | Off -> Low

let update_module (node_state : module_state) source_id pulse =
  match node_state with
  | Broadcast -> Some pulse
  | FlipFlop state -> (
      match pulse with
      | Low ->
          let new_state = flip_state !state in
          state := new_state;
          Some (flip_flop_pulse new_state)
      | High -> None)
  | Conjunction states ->
      NodeHashtbl.replace states source_id pulse;
      let all_high =
        NodeHashtbl.to_seq_values states |> Seq.for_all (fun p -> p == High)
      in
      Some (if all_high then Low else High)

let system_stopped system = Queue.is_empty system.pulses

let system_step system =
  let next_pulse = Queue.pop system.pulses in
  system.pulse_handler next_pulse;
  match NodeHashtbl.find_opt system.nodes next_pulse.dst with
  | Some target_module -> (
      match
        update_module target_module.state next_pulse.src next_pulse.value
      with
      | Some response ->
          List.iter
            (fun output_id ->
              Queue.push
                { src = target_module.id; dst = output_id; value = response }
                system.pulses)
            target_module.outputs
      | None -> ())
  (* Ignore unknown module, but still count the pulse *)
  | None -> ()

let press_button system =
  Queue.push
    { src = button_ident; dst = broadcaster_id; value = Low }
    system.pulses

let rec run_until_stopped system =
  if system_stopped system then ()
  else (
    system_step system;
    run_until_stopped system)

let run_button_press system =
  press_button system;
  run_until_stopped system

type part1_stats = { mutable low : int; mutable high : int }

let update_stats stats pulse =
  match pulse.value with
  | Low -> stats.low <- stats.low + 1
  | High -> stats.high <- stats.high + 1

let part1_score stats = stats.low * stats.high
let part1_count = 1000

let part1 input =
  let stats = { low = 0; high = 0 } in
  let system = make_system input ~pulse_handler:(update_stats stats) in
  for _i = 1 to part1_count do
    run_button_press system
  done;
  part1_score stats

(* Sketch for part 2:

   - Find A, the dependency of `rx' (should be a conjunction)
   - Find dependencies of A
   - For each dependency: assume the behavior is cyclical (verified manually), so find the first pulse and assume the following pulses are at the same interval
   - Find LCM of the dependencies' intervals
*)

type system_summary = {
  modules : module_desc NodeHashtbl.t;
  module_inputs : node_id NodeHashtbl.t;
}

let make_sys_summary (descs : module_desc list) : system_summary =
  let module_inputs = get_inputs_table descs in
  let modules =
    descs |> List.to_seq
    |> Seq.map (fun (m : module_desc) -> (m.id, m))
    |> NodeHashtbl.of_seq
  in
  { modules; module_inputs }

let sys_inputs sys id = NodeHashtbl.find_all sys.module_inputs id

module NodeSet = Set.Make (String)

let sys_depends_subset sys id =
  let rec loop subset queue =
    match queue with
    | [] -> subset
    | next :: rest ->
        if NodeSet.mem next subset then loop subset rest
        else loop (NodeSet.add next subset) (sys_inputs sys next @ rest)
  in
  loop NodeSet.empty [ id ] |> NodeSet.to_list

let make_system_subset sys ids ~pulse_handler =
  ids
  |> List.map (fun id -> NodeHashtbl.find sys.modules id)
  |> make_system ~pulse_handler

let first_pulse_for sys id =
  let n_presses = ref 0 in
  let found_high = ref false in
  let pulse_handler pulse =
    if pulse.src = id && pulse.value == High then found_high := true
  in
  let system =
    make_system_subset sys (sys_depends_subset sys id) ~pulse_handler
  in
  let rec loop () =
    incr n_presses;
    run_button_press system;
    if !found_high then !n_presses else loop ()
  in
  loop ()

let rec gcd a b = if b == 0 then a else gcd b (a mod b)

let lcm a b =
  let g = gcd a b in
  a / g * b

let part2 input =
  let sys = make_sys_summary input in
  let rx_conj =
    match sys_inputs sys rx_id with
    | [ t ] ->
        let m = NodeHashtbl.find sys.modules t in
        if m.module_type == Conjunction then t
        else failwith "`rx' trigger must be a conjunction"
    | [] -> failwith "No triggers for `rx'"
    | _ -> failwith "Multiple triggers for `rx'"
  in
  sys_inputs sys rx_conj |> List.to_seq
  |> Seq.map (first_pulse_for sys)
  |> Seq.fold_left lcm 1

let () = Advent.Main.run "input/day20.txt" read_input part1 part2
