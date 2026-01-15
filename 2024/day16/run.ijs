input =: 'm' freads 'input.txt'

'NORTH EAST SOUTH WEST' =: i.4
steps =: _1 0, 0 1, 1 0,: 0 _1
neighbors =: +"1 & steps

coord_in =: {{ ($ #: x i.~ ,) y }}
start =: 'S' coord_in input
end =: 'E' coord_in input
init_dir =: EAST

NB. Part 1

search =: {{
 'start end' =. x
 NB. Each item is SCORE DIR POS
 frontier =. ,: 0;init_dir;start
 scores =. 0 (<start)} ($y) $ _
 while. 0 < #frontier do.
  'score dir pos' =. {.frontier
  frontier =. }.frontier
  adj =. neighbors pos
  for_step_dir. I. '#' ~: (<adj){y do.
   new_pos =. step_dir{adj
   new_score =. score + >: 1000 * dir ~: step_dir
   if. new_score < (<new_pos){scores do.
    scores =. new_score (<new_pos)}scores
    frontier =. frontier , new_score;step_dir;new_pos
   end.
  end.

  NB. Keep sorted
  frontier =. /:~ frontier
 end.
 scores
}}

maze_scores =: (start;end) search input
echo (<end) { maze_scores

NB. Part 2

mark_best_paths =: {{
 'start end' =. x
 scores =. y

 NB. Includes an "effective score" for correctly handling corners
 frontier =. ,: ((<end){scores);end
 on_path =. 1 (start; end)} 0 $~ $scores

 while. 0 < #frontier do.
  'eff_score pos' =. {.frontier
  score =. (<pos){scores
  frontier =. }.frontier
  for_adj. neighbors pos do.
   prev_score =. (<adj){scores
   if. prev_score e. <: score,eff_score do.
    frontier =. frontier, prev_score;adj
    on_path =. 1 (<adj)}on_path
   NB. Only consider "true score" here to avoid propagating effective scores
   NB. beyond the corners
   elseif. prev_score = score - 1001 do.
    NB. Include the turn in the effective score
    frontier =. frontier, (<:score);adj
    on_path =. 1 (<adj)}on_path
   end.
  end.
 end.
 on_path
}}

echo +/+/ (start;end) mark_best_paths maze_scores
