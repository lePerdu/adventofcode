input =: ".@> LF cut freads 'input.txt'
N_SECRET_NUMBERS =: 2000

NB. Part 1

int_div =: [: <. %
mix =: XOR
prune =: 16777216 & |
prune_mix =: {{ [: prune (mix u) }}

next_secret_num =: [: (2048&* prune_mix) [: (int_div&32 prune_mix) (64&* prune_mix) : [:

echo +/ next_secret_num^:N_SECRET_NUMBERS input

NB. Part 2

price =: 10 & |
prices =: {{ (price y), y price F:. (next_secret_num@]) i.N_SECRET_NUMBERS }}
deltas =: 2 -~/\ ]

pattern_index =: 19 #. 9 + ]
max_pattern =: 19^4

find_pattern_prices =: {{
 p =. prices y
 indices =. 4 pattern_index\ deltas p
 NB. Drop first 4 to align with pattern indices (and they can't be picked anyway)
 shifted_prices =. 4}.p
 NB. Reverse lists so that the first one "wins"
 (|.shifted_prices) (|.indices) } max_pattern $ 0
}}"0

echo >./ +/ find_pattern_prices input
