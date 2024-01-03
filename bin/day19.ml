type category = Extreme | Musical | Aerodynamic | Shiny
type comparison = Less | Greater
type return_action = Accepted | Rejected | Next of string
type matcher = { category : category; operator : comparison; threshold : int }
type rule = { matcher : matcher; action : return_action }
type workflow = { name : string; rules : rule list; fallback : return_action }

type 'a generic_part = {
  extreme : 'a;
  musical : 'a;
  aerodynamic : 'a;
  shiny : 'a;
}

type part = int generic_part
type input = { workflows : workflow list; parts : part list }

let category_of_string = function
  | "x" -> Extreme
  | "m" -> Musical
  | "a" -> Aerodynamic
  | "s" -> Shiny
  | _ -> raise Not_found

let comparison_of_string = function
  | "<" -> Less
  | ">" -> Greater
  | _ -> raise Not_found

let action_of_string = function
  | "A" -> Accepted
  | "R" -> Rejected
  | s -> Next s

let action_pattern = {|\([a-z]+\|[AR]\)|}

let rule_regexp =
  Str.regexp @@ {|^\([xmas]\)\([<>]\)\([0-9]+\):|} ^ action_pattern ^ "$"

let rule_of_string s =
  if Str.string_match rule_regexp s 0 then
    let g n = Str.matched_group n s in
    {
      matcher =
        {
          category = category_of_string (g 1);
          operator = comparison_of_string (g 2);
          threshold = int_of_string (g 3);
        };
      action = action_of_string (g 4);
    }
  else failwith "Invalid rule"

let fallack_regexp = Str.regexp @@ "^" ^ action_pattern ^ "$"
let workflow_regexp = Str.regexp {|^\([a-z]+\){\(.*\)}$|}

let fallback_of_string s =
  if Str.string_match fallack_regexp s 0 then action_of_string s
  else failwith "Invalid fallback rule"

let rec map_last_special ~first ~last = function
  | [] -> failwith "Empty list"
  | [ x ] -> ([], last x)
  | x :: rest ->
      let ys, z = map_last_special ~first ~last rest in
      (first x :: ys, z)

let workflow_of_string s =
  if Str.string_match workflow_regexp s 0 then
    let name = Str.matched_group 1 s in
    let rules_str = Str.matched_group 2 s in
    let rules, fallback =
      String.split_on_char ',' rules_str
      |> map_last_special ~first:rule_of_string ~last:fallback_of_string
    in
    { name; rules; fallback }
  else failwith "Invalid workflow"

let part_regexp =
  let num = {|\([0-9]+\)|} in
  let field label = String.make 1 label ^ "=" ^ num in
  Str.regexp @@ "{"
  ^ String.concat "," (List.map field [ 'x'; 'm'; 'a'; 's' ])
  ^ "}"

let part_of_string s =
  if Str.string_match part_regexp s 0 then
    let g n = Str.matched_group n s |> int_of_string in
    { extreme = g 1; musical = g 2; aerodynamic = g 3; shiny = g 4 }
  else failwith "Invalid part"

let read_input file_name =
  let rec input_workflows ch =
    match In_channel.input_line ch with
    | None -> []
    | Some "" -> []
    | Some line ->
        let w = workflow_of_string line in
        w :: input_workflows ch
  in
  let rec input_parts ch =
    match In_channel.input_line ch with
    | None -> []
    | Some line ->
        let p = part_of_string line in
        p :: input_parts ch
  in
  In_channel.with_open_text file_name (fun ch ->
      let workflows = input_workflows ch in
      let parts = input_parts ch in
      { workflows; parts })

let part_category p = function
  | Extreme -> p.extreme
  | Musical -> p.musical
  | Aerodynamic -> p.aerodynamic
  | Shiny -> p.shiny

let part_matches part matcher =
  let cat = part_category part matcher.category in
  match matcher.operator with
  | Less -> cat < matcher.threshold
  | Greater -> cat > matcher.threshold

let part_matches_rule part rule = part_matches part rule.matcher

let run_workflow part workflow =
  List.find_opt (part_matches_rule part) workflow.rules
  |> Option.fold ~none:workflow.fallback ~some:(fun r -> r.action)

let rec is_part_accepted part workflow_id workflows =
  let w = Hashtbl.find workflows workflow_id in
  match run_workflow part w with
  | Accepted -> true
  | Rejected -> false
  | Next next_id -> is_part_accepted part next_id workflows

let make_workflow_map workflow_list =
  List.to_seq workflow_list |> Seq.map (fun w -> (w.name, w)) |> Hashtbl.of_seq

let part_rating p = p.extreme + p.musical + p.aerodynamic + p.shiny
let initial_workflow = "in"

let part1 input =
  let workflows = make_workflow_map input.workflows in
  List.to_seq input.parts
  |> Seq.filter (fun p -> is_part_accepted p initial_workflow workflows)
  |> Seq.map part_rating |> Seq.fold_left ( + ) 0

type int_range = int * int
type part_ranges = int_range generic_part

let initial_category_range = (1, 4000)

let initial_part_ranges =
  {
    extreme = initial_category_range;
    musical = initial_category_range;
    aerodynamic = initial_category_range;
    shiny = initial_category_range;
  }

let invert_matcher m =
  match m.operator with
  | Less -> { m with operator = Greater; threshold = m.threshold - 1 }
  | Greater -> { m with operator = Less; threshold = m.threshold + 1 }

let restrict_part_range matcher (ranges : part_ranges) : part_ranges =
  let restrict (lower, upper) =
    match matcher.operator with
    | Less -> (lower, min (matcher.threshold - 1) upper)
    | Greater -> (max (matcher.threshold + 1) lower, upper)
  in
  match matcher.category with
  | Extreme -> { ranges with extreme = restrict ranges.extreme }
  | Musical -> { ranges with musical = restrict ranges.musical }
  | Aerodynamic -> { ranges with aerodynamic = restrict ranges.aerodynamic }
  | Shiny -> { ranges with shiny = restrict ranges.shiny }

let apply_workflow workflow ranges =
  let rec loop rules ranges =
    match rules with
    | [] -> [ (workflow.fallback, ranges) ]
    | rule :: rest ->
        (rule.action, restrict_part_range rule.matcher ranges)
        :: loop rest (restrict_part_range (invert_matcher rule.matcher) ranges)
  in
  loop workflow.rules ranges

let rec apply_workflows workflows workflow_id ranges =
  apply_workflow (Hashtbl.find workflows workflow_id) ranges
  |> List.to_seq
  |> Seq.flat_map (fun (action, new_ranges) ->
         match action with
         | Accepted -> Seq.return new_ranges
         | Rejected -> Seq.empty
         | Next next_id -> apply_workflows workflows next_id new_ranges)

let range_count (lower, upper) = if upper >= lower then upper - lower + 1 else 0

let part_range_count p =
  range_count p.extreme * range_count p.musical * range_count p.aerodynamic
  * range_count p.shiny

let part2 input =
  let workflow_map = make_workflow_map input.workflows in
  apply_workflows workflow_map initial_workflow initial_part_ranges
  |> Seq.map part_range_count |> Seq.fold_left ( + ) 0

let () = Advent.Main.run "input/day19-example.txt" read_input part1 part2
