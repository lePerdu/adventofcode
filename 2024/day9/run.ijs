NB. TODO: More idiomatic way to trim trailing line breaks?
input =: "."0 LF delstring freads 'input.txt'

NB. Part 1
evens =: +: @ i. @ >. @ -:
NB. odds =: >: @ +: @ i. @ <. @ -:
NB. pick =: 1 : '{~ u@#'

NB. Alternating file IDs and _1 (for empty space): 0 _1 1 _1 2 _1 ...
disk_map_ids =: {{ (i. >. -: y) (evens y) } y $ _1 }} #input
NB. Each block tracks the file ID or _1 if empty
blocks =: input # disk_map_ids

NB. free/used indices
free_blocks =: I. _1 = blocks
used_blocks =: I. _1 ~: blocks
total_used =: # used_blocks
NB. Only care about free blocks in the total usage bounds
target_blocks =: (#~ <&total_used) free_blocks
NB. Take from the end
blocks_to_move =: (#target_blocks) {. |. used_blocks
ids_to_move =: blocks_to_move { blocks

compacted_part1 =: total_used {. ids_to_move target_blocks} blocks

checksum =: (+/@:* i.@#) @: (>.&0)

echo checksum compacted_part1

NB. Part 2

NB. First span of x empty cells in y
first_fit =: (_1 $~ [) (i.&1 @: E.) ]

move_first_fit =: {{
 size =. (+:x) { input
 pos =. y i. x
 target =. size first_fit pos {. y
 if. target < pos do.
  x (target + i. size) } _1 (pos + i. size) } y
 else. y end.
}}

NB. Trim trailing empty blocks
trim =: {.~ [: >: [: (i:&0) (=&_1)
file_ids =: i. >. -: # input

compacted_part2 =: blocks (trim F.: move_first_fit) file_ids

echo checksum compacted_part2
