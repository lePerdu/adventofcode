input =: freads 'input.txt'

load 'regex'

pattern =: '^Register A: (\d+)',LF,'Register B: (\d+)',LF,'Register C: (\d+)',LF,LF,'Program: (\d+(,\d+)*)'
NB. A;B;C;PROGRAM
match =: {{ y rxfrom~ (pattern; 1 2 3 4) rxmatch y }}
parse_init_state =: {{ (". each 3{.y) , 0 ; '' }}
parse_program =: {{ ". > ',' cut >{:y }}

matched =: match input
init_state =: parse_init_state matched
program =: parse_program matched

NB. Part 1

'A B C IP OUT' =: i.5

NB. x / 2^y
LSHIFT =: 33 b.~
RSHIFT =: 33 b.~ -
MOD8 =: 7&AND

rshift =: RSHIFT
xor =: XOR
mod8 =: MOD8

get =: {::
set =: {{ <@[ u} ] }}

combo =: {{
 if. x < 4 do.
  x
 elseif. x < 7 do.
  (x-4) get y
 else.
  echo 'invalid combo value: 7'
  throw.
 end.
}}

out_buf =: ''

adv =: {{ y A set~ (A get y) rshift x combo y }}
bxl =: {{ y B set~ (B get y) xor x }}
bst =: {{ y B set~ mod8 x combo y }}
jnz =: {{ if. A get y do. y IP set~ x combo y else. y end. }}
bxc =: {{ y B set~ (B get y) xor (C get y) [ x }} NB. Have to use 'x' to avoid valence error
out =: {{ y OUT set~ (OUT get y) , mod8 x combo y }}
bdv =: {{ y B set~ (A get y) rshift x combo y }}
cdv =: {{ y C set~ (A get y) rshift x combo y }}

opcodes =: adv`bxl`bst`jnz`bxc`out`bdv`cdv

run =: {{
 state =. x
 program =. y
 end =. <: #program
 while. end > IP get state do.
  ip =. IP get state
  opcode =. ip{program
  operand =. (>:ip){program
  state =. (2+ip) IP set state
  state =. operand opcodes@.opcode state
 end.
 state
}}

comma_join =: , ',' , ]
join_output =: comma_join&":/

echo join_output OUT get init_state run program

NB. Part 2

rshift =: {{ '(', (":x) , ') RSHIFT (', (":y), ')' }}
xor =: {{ '(', (":x) , ') XOR (', (":y), ')' }}
mod8 =: {{ 'MOD8 (', (":y), ')' }}

out =: {{ y OUT set~ (OUT get y) , < mod8 x combo y }}
jnz =: {{ y OUT set~ (OUT get y) , < 'loop ', ":x }}

NB. B and C set to indeterminate to make sure their initial values aren't used
'a_expr b_expr c_expr ip_end output_exprs' =: ('y';_.;_.;0;'') run program
assert 2 = #output_exprs
assert 'loop 0' -: >{:output_exprs
output_expr =: >{.output_exprs

a_update =: 3 : a_expr
output =: 3 : output_expr

a_sequence =: {{
 seq =. ''
 whilst. y do.
  seq =. seq,y
  y =. a_update y
 end.
 seq
}}

min_a =: 45 (33 b.) 1
max_a =: 48 (33 b.) 1

run_with_a =: output@a_sequence
nth_a =: {{ a_update^:x y }}

bit_chunk =: i. 1 LSHIFT 11
chunk_outputs =: output bit_chunk
chunks_output_n =: {{ I. y = chunk_outputs }}
shifted_chunks_for_index =: {{ (3*y) LSHIFT~ chunks_output_n y{program }}

check_a =: {{ program -: run_with_a y }}
check_a_range =: {{
 while. x < y do.
  if. check_a x do. a return. end.
  x =. >:x
 end.
 _1
}}

n_threads =: <: {. 8 T. ''
NB. {{0 T.0}}^:] n_threads

check_a_range_par =: {{
 per_thread =. >. y%x
 (per_thread * i.x) (check_a_range t. 0)"0 (per_thread * >: i.x)
}}
