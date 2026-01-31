input =: freads 'input.txt'
break_pos =: I. (LF,LF) E. input

init_lines =: LF cut break_pos{.input
init_names =: 0 1 2 & { @ > init_lines
init_values =: '1' = 5 & { @ > init_lines

gate_lines =: LF cut break_pos}.input
operators =: *.`+.`~:
op_names =: 'AND';'OR';'XOR'
op_index =: op_names i. <
apply =: {{ operators @. m }}

NB. 'A OP B -> Z' -> [A OP B C]
parse_gate =: 0 1 2 4 { ' ' cut ]
gates =: parse_gate @ > gate_lines

names =: /:~ ~. init_names , _ 3 $ ; (<a:;0 2 3){gates
name_index =: names & i.
index_name =: { & names

prefix_names =: (= {."1) # ]
x_names =: 'x' prefix_names names
y_names =: 'y' prefix_names names
z_names =: 'z' prefix_names names
assert x_names (= & #) y_names
assert (#x_names) = <: #z_names

NB. Part 1

NB. [A OP B C] -> C_INDEX OP_INDEX A_INDEX B_INDEX
convert_gate =: {{
 'a op b c' =. y
 (name_index c), (op_index op), (name_index a), name_index b
}}

index_gates =: convert_gate"1 gates

run_gate =: {{
 'c op a b' =. x
 if. +./ _1 = (a,b){y do. y
 elseif. _1 ~: c{y do. y
 else. (op apply/ (a,b){y) c}y
 end.
}}

run_gates =: {{
 NB. _1 means indeterminate
 state =. init_values (name_index init_names)} _1 $~ #names
 for. i.#y do.
  for_gate. y do.
   state =. gate run_gate state
  end.
 end.
 state
}}

echo #. |. (name_index z_names) { run_gates index_gates

NB. Part 2

NB. https://code.jsoftware.com/wiki/Phrases/Statistics
comb=: [:;(,.&.><@:>:@;\.)^:(i.@>:@-~`[`(1:<@i.@{.~<:@-,2:))

cols =: {{ (< a: ; x) { y }}

gate_output =: {{ (< y;3) { (gates,'';'';'';'') }}

half_adder =: {{
 'xn yn' =. y
 exprs =. 0 1 2 cols gates
 z_expr =. (exprs i. xn;'XOR';yn) <. (exprs i. yn;'XOR';xn)
 c_expr =. (exprs i. xn;'AND';yn) <. (exprs i. yn;'AND';xn)
 xn;yn;gate_output z_expr,c_expr
}}

full_adder =: {{
 'xn yn cn' =. y
 'an bn' =. 2 }. half_adder xn;yn
 'zn c_and_n' =. 2}. half_adder an;cn
 exprs =. 0 1 2 cols gates
 cn =. gate_output (exprs i. bn;'OR';c_and_n) <. (exprs i. c_and_n;'OR';bn)
 xn;yn;an;bn;zn;c_and_n;cn
}}

find_full_adder =: {{ full_adder (x{x_names);(x{y_names); {: y }}

swap =: {{ ((< (x,y);3){gates) (< (y,x);3)} gates }}
result_index =: {{ (3 cols gates) i. <y }}
swap_names =: swap & result_index

echo (half_adder 'x00';'y00') [ F:. find_full_adder }.i.#x_names
