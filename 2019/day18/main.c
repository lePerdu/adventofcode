#include "maze.h"

void part1(const struct maze *input);
void part2(const struct maze *input);

int main(void) {
  struct maze input;
  read_maze(&input);

  part1(&input);
  part2(&input);
  return 0;
}
