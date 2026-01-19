input =: > ". each LF cut freads 'input.txt'
size =: 2$71

NB. Part 1

pad =: {{ x , x ,~ x ,. x ,.~ y }}
neighbors =: +"1 & (4 2 $ _1 0 0 1 1 0 0 _1)

part1_count =: 1024
part1_map =: '#' pad '#' (part1_count{.input)} size $ '.'
NB. start/end adjusted for padding
start =: 2$1
end =: size

bfs =: {{
 visited_dist =. 0 (<start)} _1 $~ $y
 queue =. ,:start
 while. #queue do.
  pos =. {.queue
  queue =. }.queue
  dist =. (<pos){visited_dist
  next_dist =. >:dist
  for_next. neighbors pos do.
   if. next -: end do.
    next_dist return.
   elseif. 0 <: (<next){visited_dist do.
    NB. Skip if already visited
   elseif. '#' ~: (<next){y do.
    visited_dist =. next_dist (<next)} visited_dist
    queue =. queue, next
   end.
  end.
 end.
 _1
}}

echo bfs part1_map

NB. Part 2

can_exit =: 0 < bfs

first_blocking_coord =: {{
 map =. y
 for_pos. x do.
  NB. Increment to adjust for padding
  map =. '#' (< >:pos)}map
  if. -. can_exit map do.
   pos return.
  end.
 end.
 _1 _1
}}

part2_input =: part1_count}.input
join_comma =: {{ (":x), ',', (":y) }}
echo join_comma/ part2_input first_blocking_coord part1_map
