input =: 'm' freads 'input.txt'

coord_in =: {{ ($ #: x i.~ ,) y }}
'start end' =: 'SE' coord_in input

NB. Part 1

'NORTH EAST SOUTH WEST' =: i.4
steps =: _1 0, 0 1, 1 0,: 0 _1
neighbors =: +"1 & steps

scan_maze =: {{
 end =. x
 NB. Shortest path from each coord to the end, _1 if unknown
 dist =. 0 (<end)} _1 $~ $y
 queue =. ,:end
 while. #queue do.
  pos =. {.queue
  queue =. }.queue
  cur_dist =. (<pos){dist
  for_adj. neighbors pos do.
   if. ('#' ~: (<adj){y) *. (_1 = (<adj){dist) do.
    dist =. (>: cur_dist) (<adj)}dist
    queue =. queue,adj
   end.
  end.
 end.
 dist
}}

distance_map =: end scan_maze input

NB. Make sure there is only 1 path: All non-wall tiles have distinct, sequential distances to the end
assert (i. >: (<start){distance_map) -: /:~ ('#' ~: ,input) # ,distance_map

shift_grid =: {{ ({:x) |.!._1"1 ({.x) |.!._1 y }}

savings_for_shift =: {{
 shifted =. x shift_grid y
 cheat_dist =. +/ |x
 NB. start_open isn't necessary since those "savings" are already negative
 NB. start_open =. _1 < y
 end_open =. _1 < shifted
 saved =. 0 >. y - cheat_dist + shifted
 saved * end_open
}}

count_over =: [: +/ (<: ,)
count_cheats_over =: {{ +/ y x&count_over@:savings_for_shift"1 _ distance_map }}

part1_shifts =: _ 2 $ 2 0  _2 0  0 2  0 _2  1 1  1 _1  _1 1  _1 _1
echo 100 count_cheats_over part1_shifts

NB. Part 2

NB. Generate points (/shifts) at a specific manhattan distance
NB. Start with positive quadrant (excluding x-axis)
at_dist_in_quad =: {{ axis ,. y - axis=.>:i.y }}
NB. Rotate/flip quadrant to make in the rest
at_dist =: {{ (_1 1 *"1 |."1 q), (1 _1 *"1 |."1 q), (-q), q =. at_dist_in_quad y }}

NB. part1_shifts =: at_dist 2
part2_shifts =: ; <@at_dist"0 ] 2+i.19
echo 100 count_cheats_over part2_shifts
