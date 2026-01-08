input =: freads 'input.txt'
LFLF =: LF,LF
ordering_rules =: > ([: ". [: > '|'&cut) each LFLF cutopen cuts 1 input
page_lists =: ([: ". [: > ','&cut) each LFLF cutopen cuts _2 input
NB. Make sure the connection matrix has bounded size
NB. Otherwise, an index transformation would be needed
assert *./ 100 > ,ordering_rules
assert *./ *./ 100 > ;page_lists

NB. Part 1
conn_matrix =: #. e.~ [: i. [ , [
ordering_matrix =: 100 conn_matrix ordering_rules

subseq_valid =: {{ -. +./ (<(}.y) ; {.y) { ordering_matrix }}
page_valid =: {{ *./ subseq_valid\. y }}
valid_page_indices =: > page_valid each page_lists

middle_page =: {{ (<. 2 %~ #y) { y }}
mid_pages =: > middle_page each page_lists

echo +/ valid_page_indices * mid_pages

NB. Part 2
invalid_pages =: (-.valid_page_indices) # page_lists

NB. Seems like I have to write a custom sort algorithm in order to use a custom
NB. sort function
NB. TODO: Is there a better way?

before =: {{ (<x;y) { ordering_matrix }}
after =: before~
dontcare =: before +: after

sel =: 1 : 'u # ['

sort_pages =: {{
 if. 1 >: #y do. y
 else.
  pivot =. y{~?#y
  (sort_pages y before sel pivot),(y dontcare sel pivot),(sort_pages y after sel pivot)
 end.
}}

echo +/ > (middle_page @ sort_pages) each invalid_pages
