load 'regex'
pat =: rxcomp 'p=(\d+),(\d+) v=(-?\d+),(-?\d+)'

NB. Table of PX PY VX VY
input =: > ". each (rxfrom~ [: }."2 pat&rxmatches) freads 'input.txt'
rxfree pat
NB. room_size =: 7 11
room_size =: 103 101
t =: 100

NB. Flip order to Y X (row col)
pos =: (<a:;1 0){input
vel =: (<a:;3 2){input

pos_t =: {{ room_size |"1 pos + vel*y }}
robot_in_room =: {{ 1 (<y)} room_size $ 0 }}
room_t =: [: +/ [: robot_in_room"1 pos_t

quad_size =: <. -: room_size
NB. Only works on odd-size rooms, but that's fine here
quad_indices =: {{ quad_size (<@:+ i.)"0~ y + quad_size * y }}
select_quad =: {{ y {~ <quad_indices x }}
count_quad =: [: +/ [: +/ select_quad

end_room =: room_t 100
echo */ count_quad&end_room"1 #: i.4

NB. Part 2

NB. Helpers for visualizing arrangements
render_room =: {{ '#' (<y)} room_size $ '.' }}
render_t =: render_room @ pos_t

mean =: +/ % #
dist =: [: +/ [: *: -

NB. Score based on average distance to the mean of all robots
NB. Basically: find the most dense arrangement
score =: {{ mean dist&(mean y)"1 y }}

NB. Look for the best in the top 1000
echo {. /: score@pos_t"0 i.10000
