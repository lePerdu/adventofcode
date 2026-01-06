NB. Easier to work with transposed
input =. |: ". 'm' freads 'input.txt'

NB. Part 1
echo +/ | -/ sort"1 input

NB. Part 2
left =. {. input
right =. }. input
echo +/ left * +/"1 left =/ right
