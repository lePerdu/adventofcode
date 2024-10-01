#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "input.h"

void part1(const struct maze *input);

void part2(const struct maze *input);

int main(void) {
  struct input_grid input;
  read_input(&input);

  struct maze maze;
  process_maze(&maze, &input);
  print_maze(&maze);

  part1(&maze);
  part2(&maze);
  return 0;
}
