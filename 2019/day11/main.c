#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "intcode.c"

enum direction {
  DIR_U = 0,
  DIR_L = 1,
  DIR_D = 2,
  DIR_R = 3,
};

enum rotation {
  ROT_L = 0,
  ROT_R = 1,
};

enum color {
  COL_BLACK = 0,
  COL_WHITE = 1,
};

struct coord {
  int row;
  int col;
};

#define GRID_SIZE 100
#define MAX_VISITED 100000

struct state {
  enum direction robot_dir;
  struct coord robot_pos;
  uint8_t grid[GRID_SIZE][GRID_SIZE];

  struct machine machine;

  unsigned visited_count;
  struct coord visited[MAX_VISITED];
};

enum direction dir_rotate(enum direction d, enum rotation r) {
  return ((unsigned char)(d + (r == ROT_L ? 1 : -1))) % 4;
}

void coord_move(struct coord *robot_pos, enum direction dir) {
  switch (dir) {
  case DIR_U:
    robot_pos->row--;
    break;
  case DIR_L:
    robot_pos->col--;
    break;
  case DIR_D:
    robot_pos->row++;
    break;
  case DIR_R:
    robot_pos->col++;
    break;
  }
}

void state_init(struct state *s, const struct intcode_program *input) {
  s->robot_dir = DIR_U;
  s->robot_pos.row = GRID_SIZE / 2;
  s->robot_pos.col = GRID_SIZE / 2;
  memset(s->grid, COL_BLACK, sizeof(s->grid));
  s->visited_count = 0;
  machine_init(&s->machine, input);
}

void state_advance(struct state *s) {
  if (!machine_can_input(&s->machine)) {
    die("Expected machine to accept input\n");
  }

  machine_input(&s->machine, s->grid[s->robot_pos.row][s->robot_pos.col]);

  machine_run_til_wait(&s->machine);

  if (!machine_has_output(&s->machine)) {
    die("Expected machine to have output\n");
  }

  intcode color_output = machine_output(&s->machine);
  if (!(color_output == 0 || color_output == 1)) {
    die("Invliad color output: %ld\n", color_output);
  }

  machine_run_til_wait(&s->machine);

  if (!machine_has_output(&s->machine)) {
    die("Expected machine to have output\n");
  }

  intcode turn_output = machine_output(&s->machine);
  if (!(turn_output == 0 || turn_output == 1)) {
    die("Invliad turn output: %ld\n", turn_output);
  }

  s->grid[s->robot_pos.row][s->robot_pos.col] = color_output;
  s->visited[s->visited_count++] = s->robot_pos;
  s->robot_dir = dir_rotate(s->robot_dir, turn_output);
  coord_move(&s->robot_pos, s->robot_dir);

  if (s->visited_count >= MAX_VISITED) {
    die("Too many visited states\n");
  }
  if (s->robot_pos.row < 0 || GRID_SIZE <= s->robot_pos.row) {
    die("Row out of bounds: %d\n", s->robot_pos.row);
  }
  if (s->robot_pos.col < 0 || GRID_SIZE <= s->robot_pos.col) {
    die("Col out of bounds: %d\n", s->robot_pos.col);
  }
}

int compare_coord(const void *a, const void *b) {
  const struct coord *coord_a = a;
  const struct coord *coord_b = b;
  int compare_row = coord_a->row - coord_b->row;
  int compare_col = coord_a->col - coord_b->col;
  if (compare_row != 0) {
    return compare_row;
  } else {
    return compare_col;
  }
}

void print_grid(const struct state *s) {
  for (unsigned row = 0; row < GRID_SIZE; row++) {
    for (unsigned col = 0; col < GRID_SIZE; col++) {
      putchar(s->grid[row][col] == COL_BLACK ? '.' : '#');
    }
    putchar('\n');
  }
}

void part1(const struct intcode_program *input) {
  struct state *s = malloc(sizeof(struct state));
  state_init(s, input);

  while (!s->machine.halted) {
    state_advance(s);
  }

  // Sort and de-dup to get the final count
  qsort(s->visited, s->visited_count, sizeof(s->visited[0]), compare_coord);

  unsigned visited_count = 0;
  struct coord last_coord = (struct coord){.row = -1, .col = -1};
  for (unsigned i = 0; i < s->visited_count; i++) {
    struct coord *c = &s->visited[i];
    if (c->row != last_coord.row || c->col != last_coord.col) {
      visited_count++;
      last_coord = *c;
    }
  }

  print_grid(s);

  free(s);

  printf("Part1: %u\n", visited_count);
}

void part2(const struct intcode_program *input) {
  struct state *s = malloc(sizeof(struct state));
  state_init(s, input);
  s->grid[s->robot_pos.row][s->robot_pos.col] = COL_WHITE;

  while (!s->machine.halted) {
    state_advance(s);
  }

  puts("Part2:");
  print_grid(s);

  free(s);
}

int main(void) {
  struct intcode_program input;
  read_input(&input);
  part1(&input);
  part2(&input);
  return 0;
}
