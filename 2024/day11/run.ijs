input =: ". LF delstring freads 'input.txt'

NB. Returns 0 for 0, but that's OK since 0 is covered by a special case
n_digits =: [: >. 10 ^. >:

NB. # stones from y after x blinks
after_x_blinks =: {{
 if. 0 = x do. 1 return. end.
 rec =. (<:x)&after_x_blinks
 if. 0 = y do. rec 1 return. end.
 digits =. n_digits y
 if. -. 2 | digits do.
  NB. <. required to keep things integers
  divisor =. <. 10 ^ -:digits
  (<. y%divisor) +&rec (divisor|y)
 else.
  rec 2024*y
 end.
 NB. Memoization required for part 2
}} M.

total_x_blinks =: [: +/ after_x_blinks"0

NB. Part 1
echo 25 total_x_blinks input
NB. Part 2
echo 75 total_x_blinks input
