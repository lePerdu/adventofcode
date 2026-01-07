load 'regex'

input =: freads 'input.txt'

NB. Part 1
mul_matches =: 'mul\((\d+),(\d+)\)' rxmatches input
mul_values =: ". > input rxfrom~ }."2 mul_matches
echo +/ */"1 mul_values

NB. Part 2
do_indices =: ;{."1 'do\(\)' rxmatches input
dont_indices =: ;{."1 'don''t\(\)' rxmatches input
mul_indices =: {."1 {."2 mul_matches
nearest_before =: {{ (x I. y) { 0,x }}
is_enabled =: (do_indices & nearest_before) >: (dont_indices & nearest_before)
echo +/ */"1 (is_enabled mul_indices) # mul_values
