raw_lines =: LF cut freads 'input.txt'

towels =: dlb each ',' cut >{.raw_lines
designs =: }.raw_lines

NB. Part 1

prefixes =: {{ y {~ I. y = (x {.~ #) each y }}

is_possible =: {{
 if. 0 = #x do. 1 return. end.
 for_prefix. y do.
  p =. >prefix
  NB. Negative length to take from the end, which is apparently much more
  NB. efficient than taking prefixes
  len =. - #p
  if. -. p -: len{.x do. continue. end.
  if. (len}.x) is_possible y do.
   1 return.
  end.
 end.
 0
}}

echo +/ is_possible&towels@> designs

NB. Part 2

NB. Split up patterns by length to shorten searches
NB. (probably not necessary anymore, but doesn't hurt)
pattern_lengths =: /:~ ~. # @> towels
max_length =: >./ pattern_lengths
filter_length =: [: > ] #~ (= #@>)
towels_by_length =: <@/:~@filter_length&towels"0 i.>:max_length

NB. TODO: Try pre-binding e.&patterns for built-in hashing
NB. TODO: Try binary search lookup
matches_length_pattern =: {{ y e. > x{towels_by_length }}
has_length_prefix =: [ matches_length_pattern {.

count_possible_with_cache =: {{
 cached =. (#y){x
 if. 0 <: cached do. cached return. end.
 total =. 0
 for_len. >: i. max_length <. #y do.
  if. len has_length_prefix y do.
   total =. total + x count_possible_with_cache len}.y
  end.
 end.
 total
}}

NB. TODO: Figure out why M. doesn't work here
NB. Is it because the input is non-numeric / non-scalar?
count_possible =: {{
 NB. Explicitly pre-compute each suffix of the input until the full input
 NB. i'th entry is for (-i){.y
 NB. i.e. for the suffix of length i
 cache =. 1 (0}) (>:#y) $ _1
 for_i. >: i. #y do.
  cache =. cache i}~ cache count_possible_with_cache (-i){.y
 end.
 {:cache
}}

NB. This would work for Part 1 because count_possible is fast enough
NB. is_possible =: 0 < count_possible

echo +/ count_possible@> designs
