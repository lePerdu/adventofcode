input =: 'm' freads 'input.txt'

frequencies =: a. {~ (48+i.10) , (97+i.26) , (65+i.26)
freq_index =: frequencies & i.

NB. Part 1
antenna_coords =: ($ #: [: I. '.' ~: ,) input
antenna_types =: input {~ <antenna_coords
antenna_type_indices =: freq_index antenna_types
antennas_of_type =: antenna_coords #~ antenna_types&=
antennas_by_type =: <@antennas_of_type "0 frequencies

cartesian_prod =: > @ { @ (,&<)
NB. Flattened list of pairs of i.y
cartesian_pairs =: [: ,/ cartesian_prod~
NB. Excluding the "diagonal" where a = b
non_diag_indices =: [: , [: -. [: = i.
ordered_pairs =: non_diag_indices@# # cartesian_pairs

NB. All ordered pairs of distinct items of y, for rank N>0 items
NB. Going through indices seems simpler than boxing/unboxing
ordered_pairs_1 =: {{
 NB. Special case for 0, 1 since I couldn't figure out how to make
 NB. the cartesian production functions generate the right shape
 NB. automatically: they all "degenerate" into flat arrays instead of
 NB. keeping the nested shape
 if. 1 < #y
  do. (ordered_pairs@i.@# { ]) y
  else. (0 2 , }.$y) $ 0
 end.
}}

right_annode =: -~ +:

all_type_annodes =: [: right_annode/"2 ordered_pairs_1

collect_unique =: 1 : '[: ~. [: > [: ,each/ u each'
all_annodes =: all_type_annodes collect_unique antennas_by_type

in_bounds =: {{ *./ (0 0 <: y) *. (y < $ input) }}

NB. TODO: Would it be better to work with coordinate lists transposed?
NB. That was, this could drop the "1
echo +/ in_bounds"1 all_annodes

NB. Part 2

ext_right_annodes =: 4 : 0
 v =. y - x
 NB. Limit towards 0 edge
 n_towards_0 =. (v < 0) * <. (-y)%v
 NB. Limit towards outer edge
 n_towards_edge =. (v > 0) * <: >. (($input)-y)%v
 NB. Fallback in case the points are axis-aligned
 n_inf =. _ * v = 0
 n =. <./ n_towards_0 + n_towards_edge + n_inf
 |: y + v *"0 1 i. >: n
)

all_type_annodes_2 =: [: ,/ [: ext_right_annodes/"2 ordered_pairs_1

all_annodes_2 =: all_type_annodes_2 collect_unique antennas_by_type

NB. TODO: Would it be better to work with coordinate lists transposed?
NB. That was, this could drop the "1
echo +/ in_bounds"1 all_annodes_2
