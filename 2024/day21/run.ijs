input =: LF cut freads 'input.txt'
PART1_N_ROBOTS =: 2
PART2_N_ROBOTS =: 25

np_keys =: '789','456','123',:' 0A'
np_order =: '0123456789A'
np_index =: np_order&i.
np_coords =: ($ #: np_order i.~ ,) np_keys

'UP LEFT DOWN RIGHT ACTIVATE' =: i.5
dp_order =: UP,LEFT,DOWN,RIGHT,ACTIVATE
dp_keys =: (_1,UP,ACTIVATE),:(LEFT,DOWN,RIGHT)
dp_coords =: ($ #: dp_order i.~ ,) dp_keys

delta_perm_count =: */ @: |
delta_key_counts =: 0 >. -,]
delta_keys =: (i.4) #~ delta_key_counts
unique_perms =: [: ~. i.@!@# A. ]
filtered_coord_perms =: {{ (x,:y) u unique_perms delta_keys y - x }}
move_sequences =: {{ u filtered_coord_perms & ({&n) "0 }}

NB. path_filter moves_table coord_mapping: table where cell i,j is all
NB. possible move sequences to go from button i to j
moves_table =: {{
 <@(u move_sequences y)/~ i.#y
}}

NB. Return whether x->y passes through the bad corner on a DR path
np_bad_corner_dr =: {{ (3 = {.y) *. (0 = {:x) }}
np_bad_corner_lu =: np_bad_corner_dr~
np_perm_filter =: {{
 if. np_bad_corner_dr/ x do. }.y
 elseif. np_bad_corner_lu/ x do. }:y
 else. y
 end.
}}
np_moves_table =: np_perm_filter moves_table np_coords

dp_bad_corner_ur =: {{ (0 = {.y) *. (0 = {:x) }}
dp_bad_corner_ld =: dp_bad_corner_ur~
dp_perm_filter =: {{ if. (dp_bad_corner_ld/ x) +. (dp_bad_corner_ur/ x) do. }.y else. y end. }}
dp_moves_table =: dp_perm_filter moves_table dp_coords

dp_in_seq =: {{ ACTIVATE&, u"0 ,&ACTIVATE }}

NB. (<i j){rN_action_steps: # human button presses to make robot N do action j
NB. when robot N-1 is in position i
NB. Human is robot -1: all actions are a single button press
r0_action_steps =: 1 $~ ,~#dp_order

lookup =: {{ y{x{m }}
path_steps =: {{ +/ (x lookup) dp_in_seq y }}"_ 1
min_path_steps =: {{ <./ @: (x&path_steps) @ > y }}

rn_action_steps =: {{
 steps =. r0_action_steps
 for. i.y do. steps =. steps min_path_steps dp_moves_table end.
}}

NB. Human button presses to go between each numpad button through y
NB. intermediate robots
np_action_steps =: {{ (rn_action_steps y) min_path_steps np_moves_table }}

NB. Numpad codes already have a trailing 'A'
np_in_seq =: {{ 10&,@}: u"0 ] }}
np_path_steps =: {{ [: +/ (m lookup) np_in_seq }}"1

code_steps =: {{ x np_path_steps np_index y }}
code_num =: ".@}:
code_complexity =: code_num@] * code_steps
total_complexity =: {{ +/ (np_action_steps x)&code_complexity@> y }}

echo PART1_N_ROBOTS total_complexity input
echo PART2_N_ROBOTS total_complexity input
