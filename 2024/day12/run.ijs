input =: 'm' freads 'example2.txt'
pad =: {{ x , x ,~ x ,. x ,.~ y }}
unpad =: {{ (<;/ >: i."0 <:<: $y) { y }}
repad =: pad unpad

map =: '.' pad input

NB. Part 1

NB. up, right, down, left
dirs =: 4 2 $ _1 0  0 1  1 0  0 _1
neighbors =: dirs +"1 ]

same_up =: 0 repad {{ y = _1|.y }} map
same_right =: 0 repad {{ y = 1|."1 y }} map
same_down =: 0 repad {{ y = 1|.y }} map
same_left =: 0 repad {{ y = _1|."1 y }} map
same_in_dir =: same_up,same_right,same_down,:same_left

n_edges =: 4 - +/same_in_dir

expand_region =: {{
 visited =. 1 (<x)} 0$~ $y
 queue =. ,:x
 while. 0 < #queue do.
  pos =. {:queue
  queue =. }:queue
  for_dir. i.4 do.
   adj =. pos + dir{dirs
   if. ((<pos){dir{same_in_dir) *. (-. (<adj){visited) do.
    queue =. queue,adj
    visited =. 1 (<adj)}visited
   end.
  end.
 end.
 visited
}}

region_cost_part1 =: {{ (+/+/ n_edges * y) * +/+/y }}

coord_where =: $ #: [: I. ,
mask_out =: {{ '.' (<coord_where x)}y }}
any_plot =: [: {. [: coord_where '.' ~: ]

find_regions =: {{
 regions =. ''
 map =. y
 while. '.' ~: (<any_plot map){map do.
  r =. (any_plot map) expand_region map
  regions =. regions , <r
  map =. r mask_out map
 end.
 regions
}}
all_regions =: find_regions map

total_cost =: {{ [: +/ [: > u each }}

echo region_cost_part1 total_cost all_regions

NB. Part 2

corner_ul =: -. same_up +. same_left
corner_ur =: -. same_up +. same_right
corner_dr =: -. same_down +. same_right
corner_dl =: -. same_down +. same_left
n_corners =: corner_ul + corner_ur + corner_dr + corner_dl

minis =: {{ 3 3 & (u;._3) }}
NB. Classify the pattern
NB. x 0 x
NB. x 1 x
NB. x x x
classify =: {{ ((<1 1){y) *. -. (<0 1){y }}
id =: ]
rl =: |.@:|:
rr =: rl^:_1
flip =: |. NB. Technically, rl^:2, but this simple version works here

classify_indent =: {{ (classify@u minis@[) *. (classify@v minis@]) }}
indent_ul =: same_left id classify_indent rr same_up
indent_ur =: same_right id classify_indent rl same_up
indent_dr =: same_right flip classify_indent rl same_down
indent_dl =: same_left flip classify_indent rr same_down

n_indents =: 0 pad indent_ul + indent_ur + indent_dr + indent_dl

NB. # sides contributed by each tile in its region = # corners
n_region_sides =: n_corners + n_indents

region_cost_part2 =: {{ (+/+/ n_region_sides * y) * +/+/y }}

echo region_cost_part2 total_cost all_regions
