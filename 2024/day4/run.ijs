input =: 'm' freads 'input.txt'

NB. Part 1
xmas_h =: ,:'XMAS'
xmas_diag =: 4 4 $ 'X????M????A????S'
pattern_match =: {{ *./ *./ (x = y) +. (x = '?') }}
matches_in =: {{ ($x) (x&pattern_match);._3 y }}
count_matches =: [: +/ [: +/ matches_in
all_flips =: {{ y; (|. |: y); (|."1 |: y); (|. |."1 y) }}
all_rotations =: (all_flips xmas_h) , (all_flips xmas_diag)
NB. total_matches =: {{ +/ > count_matches&input each y }}
total_matches =: [: +/ [: > count_matches&input each
echo total_matches all_rotations

NB. Part 2
x_mas =: 3 3 $ 'M?S?A?M?S'
echo total_matches all_flips x_mas
