input =: 'm' freads 'input.txt'

frequencies =: a. {~ (48+i.10) , (97+i.26) , (65+i.26)
freq_index =: frequencies & i.

NB. Part 1
antenna_coords =: ($ #: [: I. '.' ~: ,) input
antenna_types =: input {~ <antenna_coords
antenna_type_indices =: freq_index antenna_types
antennas_of_type =: antenna_coords #~ antenna_types&=
antennas_by_type =: <@antennas_of_type "0 frequencies

NB. Generate all combinations of x items of i.y
NB. From the J dictionary entry for "!"
NB. Doesn't seem to work... it just gives a "valence error"
NB. seed =: [: i.@(,&0)&.> <:@- {. 1:
NB. cf =: i.@# ,.&.> ,&.>/\.@:(>:&.>)
NB. comb =: [: ; [ cf@[&0 seed

NB. From the Wiki (actually works)
comb =: {{
 d =. y - x
 if. d < 0 do. (0,x) $ 0 return. end.
 k =. i. >: d
 z =. (d $ < i.0 0) , < i.1 0
 for. i.x do. z =. k ,.&.> ,&.>/\. >:&.> z end.
 ; z
}}

pairwise =: {~ 2 comb #

right_annode =: -~ +:
annodes_between =: right_annode ,: right_annode~

all_type_annodes =: [: ,/ [: annodes_between/"2 pairwise

all_annodes =: ~. > ,each/ all_type_annodes each antennas_by_type

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
ext_annodes =: ext_right_annodes , ext_right_annodes~

all_type_annodes =: [: ,/ [: ext_annodes/"2 pairwise

all_annodes =: ~. > ,each/ all_type_annodes each antennas_by_type

NB. TODO: Would it be better to work with coordinate lists transposed?
NB. That was, this could drop the "1
echo +/ in_bounds"1 all_annodes
