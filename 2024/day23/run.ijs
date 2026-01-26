NB. Connections by name
input =: _ 2 2 $ , (<a:;0 1 3 4) { 'm' freads 'input.txt'

nodes =: /:~ ~. ,/input
node_count =: #nodes
node_index =: nodes&i.
node_name =: {&nodes

NB. Connections by index
connections =: node_index"1 input

graph =: (+. |:) 1 connections} 0 $~ ,~ node_count
imfam=: [ +. +./ . *.

NB. Part 1

NB. https://code.jsoftware.com/wiki/Phrases/Statistics
comb=: [:;(,.&.><@:>:@;\.)^:(i.@>:@-~`[`(1:<@i.@{.~<:@-,2:))

t_nodes =: I. 't' = {."1 nodes
pairs =: {~ 2 comb #
filter_connected =: #~ graph {~ <
connected_pairs =: [: filter_connected [: pairs I.
join_sort_box =: [: < [: /:~"1 ,.
unique_triples_containing =: [: ~. [: ; [ (join_sort_box connected_pairs)"0 1 {

echo # t_nodes unique_triples_containing graph

NB. Part 2

collect_islands =: {{
 marked =. 0 $~ #y
 islands =. ''
 for_row. y do.
  if. row_index{marked do. continue. end.
  marked =. 1 row_index}marked
  cur =. row_index = i.#marked
  for_other. I. row do.
   if. other{marked do. continue. end.
   if. cur ([ -: *.) other{y do.
    marked =. 1 other}marked
    cur =. 1 other}cur
   end.
  end.
  islands =. islands, <I.cur
 end.
 islands
}}

max_index =: [: {. \:
join_comma =: , ',' , ]
echo join_comma/ /:~ node_name > ({~ [: max_index #@>) collect_islands graph
