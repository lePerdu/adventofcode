#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "common.h"

#define INPUT_BUF_SIZE 2048

enum cell {
  EMPTY = '.',
  ASTEROID = '#',
};

#define MAX_GRID_SIZE 2048

struct grid {
  unsigned rows;
  unsigned cols;
  enum cell data[MAX_GRID_SIZE];
};

struct coord {
  int row;
  int col;
};

bool read_line(struct grid *grid, char **line_buf) {
  size_t line_size;
  if (getline(line_buf, &line_size, stdin) < 0) {
    perror_die("Failed to read input");
  }

  size_t line_offset = 0;
  unsigned row_offset = grid->rows * grid->cols;
  unsigned col_offset = 0;
  unsigned line_len = 0;
  char c;
  while (1) {
    c = (*line_buf)[line_offset];
    line_offset++;
    switch (c) {
    case '.':
      grid->data[row_offset + col_offset] = EMPTY;
      break;
    case '#':
      grid->data[row_offset + col_offset] = ASTEROID;
      break;
    case '\n':
    case 0:
      goto READ_LOOP;
    default:
      die("Invalid char: '%c'\n", c);
    }
    col_offset++;
    line_len++;
  }
READ_LOOP:

  if (line_len == 0) {
    return false;
  }

  if (grid->cols == 0) {
    // First line
    grid->cols = line_len;
  } else if (line_len != grid->cols) {
    die("Non-matchine line lengths\n");
  }

  grid->rows++;
  return true;
}

void read_input(struct grid *input) {
  input->rows = 0;
  input->cols = 0;
  memset(input->data, 0, sizeof(input->data));

  char *line_buf = NULL;
  while (read_line(input, &line_buf)) {
  }

  free(line_buf);

  if (input->rows * input->cols == 0) {
    die("Empty grid\n");
  }
}

enum cell grid_index(const struct grid *grid, unsigned row, unsigned col) {
  return grid->data[row * grid->cols + col];
}

void print_grid(const struct grid *grid) {
  for (unsigned row = 0; row < grid->rows; row++) {
    for (unsigned col = 0; col < grid->cols; col++) {
      putchar(grid_index(grid, row, col));
    }
    putchar('\n');
  }
}

int gcd(int a, int b) {
  while (b != 0) {
    int temp = a % b;
    a = b;
    b = temp;
  }
  return a;
}

bool visible_in_dir(const struct grid *grid, unsigned row, unsigned col,
                    int delta_row, int delta_col) {
  int cur_row = row;
  int cur_col = col;
  while (1) {
    cur_row += delta_row;
    cur_col += delta_col;
    bool in_bounds = 0 <= cur_row && cur_row < (int)grid->rows &&
                     0 <= cur_col && cur_col < (int)grid->cols;
    if (!in_bounds) {
      return false;
    }

    if (grid_index(grid, cur_row, cur_col) == ASTEROID) {
      return true;
    }
  }
}

unsigned count_visible(const struct grid *grid, unsigned row, unsigned col) {
  int total = 0;
  for (int drow = -row; drow < (int)(grid->rows - row); drow++) {
    for (int dcol = -col; dcol < (int)(grid->cols - col); dcol++) {
      // TODO Can abs be removed?
      if ((drow == 0 && dcol == 0) || gcd(abs(drow), abs(dcol)) > 1) {
        continue;
      }
      if (visible_in_dir(grid, row, col, drow, dcol)) {
        total++;
      }
    }
  }
  return total;
}

struct coord part1(const struct grid *input) {
  unsigned ans = 0;
  unsigned ans_row = 0;
  unsigned ans_col = 0;
  for (unsigned row = 0; row < input->rows; row++) {
    for (unsigned col = 0; col < input->cols; col++) {
      if (grid_index(input, row, col) == EMPTY) {
        continue;
      }

      unsigned count = count_visible(input, row, col);
      if (count > ans) {
        ans = count;
        ans_row = row;
        ans_col = col;
      }
    }
  }

  printf("Part1: %u (%d, %d)\n", ans, ans_col, ans_row);
  return (struct coord){.row = ans_row, .col = ans_col};
}

#define BUFFER_SIZE 300
#define PART2_COUNT 200

float get_angle(const struct coord *delta) {
  double pos_neg_pi_angle = atan2((float)delta->col, (float)-delta->row);
  if (pos_neg_pi_angle < 0) {
    return 2 * M_PI + pos_neg_pi_angle;
  } else {
    return pos_neg_pi_angle;
  }
}

int compare_angle(const void *a, const void *b) {
  const struct coord *delta_a = a;
  const struct coord *delta_b = b;
  float delta_angle = get_angle(delta_a) - get_angle(delta_b);
  if (delta_angle < 0) {
    return -1;
  } else if (delta_angle > 0) {
    return 1;
  } else {
    return 0;
  }
}

bool find_visible_in_dir(const struct grid *grid, struct coord origin,
                         struct coord delta, struct coord *found) {
  int cur_row = origin.row;
  int cur_col = origin.col;
  while (1) {
    cur_row += delta.row;
    cur_col += delta.col;
    bool in_bounds = 0 <= cur_row && cur_row < (int)grid->rows &&
                     0 <= cur_col && cur_col < (int)grid->cols;
    if (!in_bounds) {
      return false;
    }

    if (grid_index(grid, cur_row, cur_col) == ASTEROID) {
      found->row = cur_row - origin.row;
      found->col = cur_col - origin.col;
      return true;
    }
  }
}

struct coord get_nth_vaporized(const struct grid *grid, struct coord origin) {
  size_t asteroid_count = 0;
  struct coord buffer[BUFFER_SIZE];

  for (int drow = -origin.row; drow < (int)(grid->rows - origin.row); drow++) {
    for (int dcol = -origin.col; dcol < (int)(grid->cols - origin.col);
         dcol++) {
      // TODO Can abs be removed?
      if ((drow == 0 && dcol == 0) || gcd(abs(drow), abs(dcol)) > 1) {
        continue;
      }
      struct coord delta = (struct coord){.row = drow, .col = dcol};
      if (find_visible_in_dir(grid, origin, delta, &buffer[asteroid_count])) {
        asteroid_count++;
        if (asteroid_count >= BUFFER_SIZE) {
          die("Too many asteroids per iteration\n");
        }
      }
    }
  }

  qsort(buffer, asteroid_count, sizeof(buffer[0]), compare_angle);
  for (unsigned i = 0; i < asteroid_count; i++) {
    printf("%dth: (%d, %d) ", i + 1, origin.col + buffer[i].col,
           origin.row + buffer[i].row);
    printf("+(%d, %d) ", buffer[i].col, buffer[i].row);
    printf("%g deg\n", get_angle(&buffer[i]) / M_PI * 180);
  }

  if (asteroid_count < PART2_COUNT) {
    die("Not enough asteroids in first iteration\n");
  }

  struct coord nth_delta = buffer[PART2_COUNT - 1];
  return (struct coord){
      .row = origin.row + nth_delta.row,
      .col = origin.col + nth_delta.col,
  };
}

struct coord run_til_done(const struct grid *original_grid, struct coord origin,
                          int extract_nth) {
  struct grid grid;
  memcpy(&grid, original_grid, sizeof(grid));

  struct coord nth_coord;
  struct coord buffer[BUFFER_SIZE];

  size_t running_count = 0;
  do {
    print_grid(&grid);
    size_t asteroid_count = 0;
    memset(buffer, 0, sizeof(buffer));
    for (int drow = -origin.row; drow < (int)(grid.rows - origin.row); drow++) {
      for (int dcol = -origin.col; dcol < (int)(grid.cols - origin.col);
           dcol++) {
        // TODO Can abs be removed?
        if ((drow == 0 && dcol == 0) || gcd(abs(drow), abs(dcol)) > 1) {
          continue;
        }
        struct coord delta = (struct coord){.row = drow, .col = dcol};
        if (find_visible_in_dir(&grid, origin, delta,
                                &buffer[asteroid_count])) {
          asteroid_count++;
          if (asteroid_count >= BUFFER_SIZE) {
            die("Too many asteroids per iteration\n");
          }
        }
      }
    }

    if (asteroid_count == 0) {
      break;
    }

    printf("%lu vaporized\n", asteroid_count);

    qsort(buffer, asteroid_count, sizeof(buffer[0]), compare_angle);
    for (unsigned i = 0; i < asteroid_count; i++) {
      struct coord final_coord = (struct coord){
          .row = origin.row + buffer[i].row, .col = origin.col + buffer[i].col};
      printf("%luth: (%d, %d) ", running_count + i + 1, final_coord.col,
             final_coord.row);
      printf("+(%d, %d) ", buffer[i].col, buffer[i].row);
      printf("%g deg\n", get_angle(&buffer[i]) / M_PI * 180);

      if (grid.data[final_coord.row * grid.cols + final_coord.col] ==
          ASTEROID) {
        grid.data[final_coord.row * grid.cols + final_coord.col] = EMPTY;
      } else {
        die("Tried to vaporize nothing at (%d, %d)\n", final_coord.col,
            final_coord.row);
      }
    }

    running_count += asteroid_count;

    if (extract_nth == -1) {
      // No-op, already found
    } else if (extract_nth > (int)asteroid_count) {
      extract_nth -= asteroid_count;
    } else {
      nth_coord = (struct coord){
          .row = origin.row + buffer[extract_nth - 1].row,
          .col = origin.col + buffer[extract_nth - 1].col,
      };
      extract_nth = -1;
    }
  } while (1);

  if (extract_nth == -1) {
    return nth_coord;
  } else {
    die("Not enough deleted");
  }
}

void part2(const struct grid *input, struct coord base_coord) {
  struct coord ans_coord = run_til_done(input, base_coord, PART2_COUNT);
  int ans = 100 * ans_coord.col + ans_coord.row;
  printf("Part2: %d (%d, %d)\n", ans, ans_coord.col, ans_coord.row);
}

int main(void) {
  struct grid input;
  read_input(&input);
  printf("Grid size: (%u, %u)\n", input.cols, input.rows);
  print_grid(&input);
  struct coord opt_coord = part1(&input);
  part2(&input, opt_coord);
  return 0;
}
