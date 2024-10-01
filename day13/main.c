#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>

#include "common.h"
#include "intcode.c"

#define ROW_COUNT 22
#define COL_COUNT 38

enum tile {
  TILE_EMPTY = 0,
  TILE_WALL = 1,
  TILE_BLOCK = 2,
  TILE_HORIZ_PAD = 3,
  TILE_BALL = 4,
  TILE_MAX,
};

struct grid {
  enum tile data[ROW_COUNT][COL_COUNT];
};

void grid_init(struct grid *g) {
  for (unsigned y = 0; y < ROW_COUNT; y++) {
    for (unsigned x = 0; x < COL_COUNT; x++) {
      g->data[y][x] = TILE_EMPTY;
    }
  }
}

void grid_print(const struct grid *g) {
  for (unsigned y = 0; y < ROW_COUNT; y++) {
    for (unsigned x = 0; x < COL_COUNT; x++) {
      char c;
      switch (g->data[y][x]) {
      case TILE_EMPTY:
        c = ' ';
        break;
      case TILE_WALL:
        c = '#';
        break;
      case TILE_BLOCK:
        c = '*';
        break;
      case TILE_HORIZ_PAD:
        c = '-';
        break;
      case TILE_BALL:
        c = 'O';
        break;
      default:
        die("Invalid tile: %d\n", g->data[y][x]);
      }
      putchar(c);
    }
    putchar('\n');
  }
}

void part1(const struct intcode_program *input) {
  struct machine *m = malloc(sizeof(struct machine));
  machine_init(m, input);

  struct grid g;
  grid_init(&g);

  size_t output_count = 0;
  intcode output_buf[3];

  while (!m->halted) {
    enum machine_state state = machine_run_til_wait(m);
    if (state != WAIT_OUTPUT && state != HALTED) {
      die("Unexpected wait state: %d\n", state);
    }

    if (machine_has_output(m)) {
      output_buf[output_count] = machine_output(m);
      output_count++;
      if (output_count == 3) {
        intcode x = output_buf[0];
        intcode y = output_buf[1];
        intcode tile_id = output_buf[2];
        if (x < 0 || COL_COUNT <= x) {
          die("Invalid x coord: %ld\n", x);
        }
        if (y < 0 || ROW_COUNT <= y) {
          die("Invalid y coord: %ld\n", y);
        }
        if (tile_id < 0 || TILE_MAX <= tile_id) {
          die("Invalid tile ID: %ld\n", tile_id);
        }
        g.data[y][x] = tile_id;

        output_count = 0;
      }
    }
  }

  free(m);

  grid_print(&g);

  unsigned block_count = 0;
  for (unsigned y = 0; y < ROW_COUNT; y++) {
    for (unsigned x = 0; x < COL_COUNT; x++) {
      if (g.data[y][x] == TILE_BLOCK) {
        block_count++;
      }
    }
  }

  printf("Part1: %u\n", block_count);
}

#define COMMAND_LEN 3

struct game_state {
  int score;
  unsigned ball_x;
  unsigned ball_y;
  unsigned paddle_x;
  unsigned paddle_y;

  unsigned output_buf_len;
  intcode output_buf[COMMAND_LEN];

  struct grid grid;
  struct machine machine;
};

enum joystick_command {
  JOY_LEFT = -1,
  JOY_NEUTRAL = 0,
  JOY_RIGHT = 1,
};

void game_init(struct game_state *g, const struct intcode_program *program) {
  g->score = 0;
  g->output_buf_len = 0;

  grid_init(&g->grid);
  machine_init(&g->machine, program);
}

void game_run_til_input(struct game_state *g) {
  bool done = false;
  while (!done) {
    enum machine_state state = machine_run_til_wait(&g->machine);

    if (machine_has_output(&g->machine)) {
      g->output_buf[g->output_buf_len] = machine_output(&g->machine);
      g->output_buf_len++;
      if (g->output_buf_len == 3) {
        intcode x = g->output_buf[0];
        intcode y = g->output_buf[1];
        intcode tile_id = g->output_buf[2];

        // Special output command for score
        if (x == -1 && y == 0) {
          g->score = tile_id;
        } else {
          if (x < 0 || COL_COUNT <= x) {
            die("Invalid x coord: %ld\n", x);
          }
          if (y < 0 || ROW_COUNT <= y) {
            die("Invalid y coord: %ld\n", y);
          }
          if (tile_id < 0 || TILE_MAX <= tile_id) {
            die("Invalid tile ID: %ld\n", tile_id);
          }
          g->grid.data[y][x] = tile_id;
          if (tile_id == TILE_BALL) {
            g->ball_x = x;
            g->ball_y = y;
          } else if (tile_id == TILE_HORIZ_PAD) {
            g->paddle_x = x;
            g->paddle_y = y;
          }
        }

        g->output_buf_len = 0;
      }
    }

    switch (state) {
    case WAIT_INPUT:
    case HALTED:
      done = true;
      break;
    case WAIT_OUTPUT:
      break;
    default:
      die("Unexpected wait state: %d\n", state);
    }
  }
}

bool game_finished(const struct game_state *g) { return g->machine.halted; }

bool is_valid_joy_input(int joy_input) {
  return joy_input == JOY_LEFT || joy_input == JOY_NEUTRAL ||
         joy_input == JOY_RIGHT;
}

void game_print(const struct game_state *g) {
  printf("Score: %d\n", g->score);
  grid_print(&g->grid);
}

enum joystick_command get_user_input() {
  int joy_input;
  while (1) {
    printf("Joystick input: ");
    if (scanf("%d", &joy_input) != 1 || !is_valid_joy_input(joy_input)) {
      printf("Invalid joystick command\n");
    } else {
      return joy_input;
    }
  }
}

enum joystick_command get_ai_input(struct game_state *g) {
  if (g->ball_x < g->paddle_x) {
    return JOY_LEFT;
  } else if (g->ball_x > g->paddle_x) {
    return JOY_RIGHT;
  } else {
    return JOY_NEUTRAL;
  }
}

void game_run(struct game_state *g) {
  while (1) {
    game_run_til_input(g);
    game_print(g);

    if (game_finished(g)) {
      break;
    }
    // Otherwise, assume waiting for input

    // enum joystick_command joy_input = get_user_input();
    enum joystick_command joy_input = get_ai_input(g);

    machine_input(&g->machine, joy_input);
  }
}

void part2(const struct intcode_program *input) {
  struct game_state *game = malloc(sizeof(*game));
  game_init(game, input);
  // Override memory for part2
  game->machine.memory[0] = 2;

  game_run(game);

  printf("Part2: %d\n", game->score);
  free(game);
}

int main(void) {
  struct intcode_program input;
  FILE *input_file = fopen("input.txt", "r");
  read_input_from_file(&input, input_file);
  fclose(input_file);

  part1(&input);
  part2(&input);
  return 0;
}
