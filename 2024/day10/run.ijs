input =: "."0 'm' freads 'input.txt'

NB. Part 1

pad =: {{ x , x ,~ x ,. x ,.~ y }}
map =: 255 pad input
NB. All positions at height y
at_height =: {{ ($ #: [: I. y=,) map }}
trail_heads =: at_height 0
trail_ends =: at_height 9

steps =: 4 2 $ _1 0  0 1  1 0  0 _1
neighbors =: steps +"1 ]

height =: map {~ <
is_gradual_upward =: {{ (>: height x) = height y }}
trail_steps =: {{ (#~ y&is_gradual_upward) neighbors y }}

search_trail =: {{
 frontier =. ,: y
 visited =. 0$~ $map
 n_reached =. 0
 while. 0 < #frontier do.
  pos =. {:frontier
  frontier =. }:frontier
  for_next. trail_steps pos do.
   if. (<next){visited do. continue. end.
   visited =. 1 (<next)}visited
   if. 9 = height next
    do. n_reached =. >: n_reached
    else. frontier =. frontier , next
   end.
  end.
 end.
 n_reached
}}

echo +/ search_trail"1 trail_heads

NB. Part 2

sum_neighbors =: {{ (+/ (<trail_steps x){y) (<x)}y }}
NB. Update #trails in y for all positions at height x
calc_level =: ([ F.. sum_neighbors at_height)~

init_counts =: 1 (<trail_ends) } 0$~ $map
trail_counts =: init_counts [ F.: calc_level i.9
echo +/ (<trail_heads) { trail_counts
