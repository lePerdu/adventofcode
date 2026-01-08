input =: 'm' freads 'input.txt'
start =: ($ #: '^' i.~ ,) input
NB. Add boarder to make bounds checking easier
map =: '*' , '*' ,~ '*' ,. '*' ,.~ input
NB. Adjust for boarder
start =: start + 1

NB. Part 1
dirs =: 4 2 $ _1 0 0 1 1 0 0 _1
step_in_dir =: {{ x + y { dirs }}

walk =: {{
 visited =. map
 pos =. y
 dir =. 0
 while. '*' ~: (<pos){map do.
  visited =. 'X' (<pos) } visited
  next =. pos step_in_dir dir
  if. '#' = (<next){map do.
   dir =. 4 | dir + 1
  else.
   pos =. next
  end.
 end.
 visited
}}

visited =: walk start
echo +/+/ 'X' = visited

NB. Part 2
NB. TODO: This part is a bit slow. How can it be sped up?
NB. - Maybe converting it into a graph representation would be faster?
NB.   That way, the whole map doesn't have to be traversed each time
NB. - Does mutating bumped_in_dir create a new copy each time?

NB. Only have to check positions that are on the guard's path from Part 1
candidates =: ($ #: [: I. 'X' = ,) visited

is_loop =: {{
 pos =. x
 map =. y
 bumped_in_dir =. (4,$map) $ 0
 dir =. 0
 while. '*' ~: (<pos){map do.
  next =. pos step_in_dir dir
  if. '#' = (<next){map do.
   if. (<dir,next){bumped_in_dir do.
    1 return.
   else.
    bumped_in_dir =. 1 (<dir,next)}bumped_in_dir
   end.
   dir =. 4 | dir + 1
  else.
   pos =. next
  end.
 end.
 0
}}

is_loop_with_extra =: {{ start is_loop '#' (<y)}map }}
echo +/ is_loop_with_extra"1 candidates
