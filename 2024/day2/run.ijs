NB. Rows aren't the same length, so they have to be boxed
input =: (". @ > @ cutopen) each cutopen freads 'input.txt'

NB. Part 1
is_increasing =: -: /:~
is_decreasing =: -: \:~
deltas =: | @ (}: - }.)
in_gradual_range =: 1&<: *. <:&3
gradual =: *./ @ in_gradual_range @ deltas
safe =: gradual *. is_increasing +. is_decreasing
echo +/ > safe each input

NB. Part 2
not_ident =: -. @ = @ i.  NB. "not" of the identity matrix
one_removed =: #~ not_ident @ #  NB. Each row picks all indices except the i'th
safe_with_one_removed =: [: +./ safe"1 @ one_removed  NB. Whether any are safe
echo +/ > safe_with_one_removed each input
