input =: > ([: ". each ':' cut ]) each LF cut freads 'input.txt'

NB. Part 1
cart_power =: {{ ((y # $x) #: i. ($x) ^ y) { x }}
all_ops =: {{ +`* cart_power (<: y) }}

apply_ops =: {{ x/ |. y }}
check_ops =: {{ (0{::y) = x apply_ops 1{::y }}

check_solutions =: {{ y check_ops"1~ all_ops (# 1{::y) }}

NB. has_solution =: +./ @ check_solutions
NB. This is a bit faster since it can return early once it's found 1 solution
has_solution =: {{
 for_ops. all_ops (# 1{::y) do.
  if. ops check_ops y do. 1 return. end.
 end.
 0
}}

NB. Debugging tools:
NB. get_solution_ops =: {{ < |."1 (check_solutions y) # (all_ops # 1{::y) }}
NB. make_solution_table =: ,. get_solution_ops"1

NB. Part1 solution can be used to speed up part 2
part1_solvable =: has_solution"1 input
part1_ans =: +/ part1_solvable * > {."1 input
echo part1_ans

NB. Part 2
NB. x: is required to keep things integral with larger numbers in the input
next_pow_10 =: {{ 10 ^ x: >. 10 ^. >: y }}
concat =: {{ y + x * next_pow_10 y }}
tacnoc =: concat~
NB. TODO: Only generate operator sequences that use concat, since part1 already
NB. checked ones that don't
all_ops =: {{ +`*`tacnoc cart_power (<: y) }}

NB. Don't re-check equations solvable in Part 1
part2_candidates =: (-. part1_solvable) # input
echo part1_ans + +/ (has_solution * >@{.)"1 part2_candidates
