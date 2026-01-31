schematics =: [;._1 '     ', 'm' freads 'input.txt'

is_lock =: ('#####' -: {.)"2
heights =: [: <: [: +/"2 '#' = ]

locks =: heights (#~ is_lock) schematics
keys =: heights (#~ -.@is_lock) schematics

fits =: [: *./ 5 >: +
pairwise =: [: ,/ ,:"1/

echo +/ fits/"2 locks pairwise keys
