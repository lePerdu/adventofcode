raw =: freads 'input.txt'

load 'regex'

xy =: 'X.(\d+), Y.(\d+)' NB. Accepts '+' (for buttons) or '=' (for prize)
pattern =: rxcomp 'Button A: ',xy,LF,'Button B: ',xy,LF,'Prize: ',xy,LF

NB. Table of: (AX AY) (BX BY) (PX PY)
input =: > ". each (}."2 pat rxmatches raw) rxfrom raw
rxfree pattern

NB. Part 1

NB. (A B) presses for a row of input y (just a change of basis)
presses =: {{ (4 5{y) %. (0 1{y),.(2 3{y) }}

tokens =: [: +/ 3 1 * ]
to_int_or_0 =: ] (] * -:) <.
total_tokens =: [: +/ tokens@to_int_or_0@presses"1

echo total_tokens input

NB. Part 2 (easy since I used linear algebra for Part 1)
fixed_input =: 0 0 0 0 10000000000000 10000000000000 +"1 input
echo total_tokens fixed_input
