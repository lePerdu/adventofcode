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

NB. Build a J expression that represents the program
NB. This assumes the program is of the form:
NB.  ...
NB.  out X
NB.  adv 3
NB.  jnz 0
NB. Where X only depends on A and doesn't modify A

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

NB. TODO: Take this from the program instead of assuming it's always 3
shift_bits =: 3
assert a_expr = '(y) RSHIFT (', (":shift_bits), ')'

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

run_with_a =: output@a_sequence
run_nth =: {{ output a_update^:x y }}

NB. Low 10 bits affect the first output
init_bits =: I. ({.program) = output i. 1 LSHIFT 8 + shift_bits
NB. Upper bits that affect the xth output
upper_bits =: {{ (7 + shift_bits * y) LSHIFT~ i.8 }}

extend_chunk =: {{
 target =. x{program
 NB. TODO: Use array operations
 res =. 0$0
 for_upper. upper_bits x do.
  for_lower. y do.
   guess =. upper OR lower
   if. target = x run_nth guess do.
    res =. res, guess
   end.
  end.
 end.
 res
}}

echo {. init_bits [ F.. extend_chunk }. i. #program
