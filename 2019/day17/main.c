#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "intcode.c"

#define GRID_ROWS 45
#define GRID_COLS 49

enum cell {
  CELL_UNKNOWN = 0,
  CELL_SCAFFOLD = '#',
  CELL_EMPTY = '.',
  CELL_UP = '^',
  CELL_DOWN = 'v',
  CELL_LEFT = '<',
  CELL_RIGHT = '>',
  CELL_TUMBLING = 'X',
};

struct grid {
  enum cell data[GRID_ROWS][GRID_COLS];
};

bool in_bounds(unsigned row, unsigned col) {
  return 0 <= row && row < GRID_ROWS && 0 <= col && col < GRID_COLS;
}

void fill_grid(struct grid *g, struct machine *m) {
  memset(g, 0, sizeof(*g));

  unsigned row = 0;
  unsigned col = 0;

  while (!m->halted) {
    enum machine_state state = machine_run_til_wait(m);
    if (state == WAIT_INPUT) {
      die("Unexpected state: WAIT_INPUT\n");
    }

    if (machine_has_output(m)) {
      char c = machine_output(m);
      putchar(c);

      if (c == '\n') {
        row++;
        col = 0;
      } else {
        if (!in_bounds(row, col)) {
          die("Grid data out of bounds\n");
        }
        g->data[row][col] = c;
        col++;
      }
    }
  }

  for (unsigned row = 0; row < GRID_ROWS; row++) {
    for (unsigned col = 0; col < GRID_COLS; col++) {
      if (g->data[row][col] == CELL_UNKNOWN) {
        die("Grid not fully initialized (%u, %u)\n", row, col);
      }
    }
  }
}

bool is_scaffold(enum cell c) {
  switch (c) {
  case CELL_SCAFFOLD:
  case CELL_UP:
  case CELL_DOWN:
  case CELL_LEFT:
  case CELL_RIGHT:
    return true;
  default:
    return false;
  }
}

void part1(const struct intcode_program *input) {
  struct machine *m = malloc(sizeof(*m));
  machine_init(m, input);
  struct grid g;
  fill_grid(&g, m);

  free(m);

  unsigned ans = 0;
  for (unsigned row = 1; row < GRID_ROWS - 1; row++) {
    for (unsigned col = 1; col < GRID_COLS - 1; col++) {
      bool center = is_scaffold(g.data[row][col]);
      bool up = is_scaffold(g.data[row + 1][col]);
      bool down = is_scaffold(g.data[row - 1][col]);
      bool left = is_scaffold(g.data[row][col - 1]);
      bool right = is_scaffold(g.data[row][col + 1]);
      if (center && up && down && left && right) {
        ans += row * col;
      }
    }
  }

  printf("Part1: %u\n", ans);
}

void run_til_input(struct machine *m) {
  while (1) {
    enum machine_state state = machine_run_til_wait(m);
    if (state == WAIT_INPUT || state == HALTED) {
      return;
    }

    if (machine_has_output(m)) {
      intcode c = machine_output(m);
      putchar(c);
    }
  }
}

void send_command(struct machine *m, const char *command) {
  run_til_input(m);
  printf("%s", command);
  while (*command != 0) {
    enum machine_state state = machine_run_til_wait(m);
    if (state == HALTED) {
      die("Unexpected state: %d\n", state);
    }

    if (!machine_can_input(m)) {
      die("Expected machine to require input\n");
    }

    machine_input(m, *command);
    command++;
  }
}

void run_to_finish(struct machine *m) {
  while (!m->halted) {
    enum machine_state state = machine_run_til_wait(m);
    if (state == WAIT_INPUT) {
      die("Unexpected state: WAIT_INPUT\n");
    }

    if (machine_has_output(m)) {
      intcode output = machine_output(m);
      if (isascii(output)) {
        putchar(output);
      } else {
        printf("Part2: %ld\n", output);
      }
    }
  }
}

void part2(const struct intcode_program *input) {
  struct machine *m = malloc(sizeof(*m));
  machine_init(m, input);
  // Override control mdoe
  m->memory[0] = 2;

  // Commands found manually by examining the scaffold map
  send_command(m, "A,B,A,C,A,B,C,A,B,C\n");
  send_command(m, "R,12,R,4,R,10,R,12\n");
  send_command(m, "R,6,L,8,R,10\n");
  send_command(m, "L,8,R,4,R,4,R,6\n");
  send_command(m, "n\n");

  run_to_finish(m);

  // intcode ans = machine_output(m);
  free(m);

  // printf("Part2: %ld\n", ans);
}

int main(void) {
  struct intcode_program input;
  read_input(&input);
  part1(&input);
  part2(&input);
  return 0;
}
