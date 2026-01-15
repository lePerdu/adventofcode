raw =: freads 'input.txt'

lines =: <;._2 raw,LF

sep_index =: lines i. <''

map =: > sep_index {. lines
movements =: ; sep_index }. lines

NB. Part 1

'UP RIGHT DOWN LEFT' =: i.4
moves =: '^>v<'
movements =: moves i. movements

steps =: 4 2 $ _1 0 0 1 1 0 0 _1

move_cell =: {{
 'p delta' =. x
 ('.', (<p){y) (p ; p + delta)} y
}}

try_move_cell =: {{
 'p delta' =. x
 if. '.' = (<p + delta){y
  do. x move_cell y
  else. throw.
 end.
}}

try_move_lr =: {{
 'p delta' =. x
 pp =. p + delta
 select. (<pp){y
  case. '#' do. throw.
  case. '.' do. x move_cell y
  fcase. 'O' do.
  fcase. '[' do.
  case. ']' do. x try_move_cell (pp;delta) try_move_lr y
 end.
}}

try_move_ud =: {{
 'p delta' =. x
 pp =. p + delta
 select. (<pp){y
  case. '#' do. throw.
  case. '.' do. x move_cell y
  case. 'O' do.
   x try_move_cell (pp;delta) try_move_ud y
  case. '[' do.
   x try_move_cell (delta ;~ pp + 0 1) try_move_ud (pp;delta) try_move_ud y
  case. ']' do.
   x try_move_cell (delta ;~ pp + 0 _1) try_move_ud (pp;delta) try_move_ud y
 end.
}}

try_move_dir =: {{
 'p dir' =. x
 delta =. dir{steps
 NB. even=UD, odd=LR
 if. 2 | dir
  do. (p;delta) try_move_lr y
  else. (p;delta) try_move_ud y
 end.
}}

do_move =: {{
 'p dir' =. x
 try.
  y =. x try_move_dir y
  p =. p + dir{steps
 catcht.
  NB. No change to map or position
 end.
 NB. For debugging:
 NB. echo '--- ', (dir{moves), ':'
 NB. echo y
 p; y
}}

do_moves =: {{
 'robot map' =. x
 for_move. y do.
  'robot map' =. (robot; move) do_move map
 end.
 map
}}

robot_coord =: $ #: '@' i.~ ,
box_coords =: $ #: [: I. '[O' e.~ ,

gps_coord =: [: +/ 100 1 * ]
gps_sum =: [: +/ [: gps_coord"1 box_coords

split_state =: {{
 robot =. robot_coord y
 robot; '.' (<robot)} y
}}

score =: {{ gps_sum (split_state y) do_moves movements }}

echo score map

NB. Part 2
NB. Part 1 functions were updated to work with both parts

expand_cell =: {{
 select. y
  case. '.' do. '..'
  case. '#' do. '##'
  case. 'O' do. '[]'
  case. '@' do. '@.'
 end.
}}
expand_map =: [: ,"2 expand_cell"0

echo score expand_map map
