#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "common.h"

enum cell {
  CELL_BUG = '#',
  CELL_EMPTY = '.',
};

#define GRID_SIZE 5

struct input_grid {
  uint8_t data[GRID_SIZE][GRID_SIZE];
};

static bool is_valid_cell(char cell) {
  return cell == CELL_BUG || cell == CELL_EMPTY;
}

void read_input(struct input_grid *grid) {
  char *line_buf = NULL;
  size_t buf_size = 0;

  for (unsigned row = 0; row < GRID_SIZE; row++) {
    errno = 0;
    ssize_t line_len = getline(&line_buf, &buf_size, stdin);
    if (line_len < 0) {
      if (errno != 0) {
        perror_die("Failed to read input");
      } else {
        die("Not enough lines\n");
      }
    }

    for (unsigned col = 0; col < GRID_SIZE; col++) {
      char c = line_buf[col];
      if (isspace(c) || c == 0) {
        die("Line too short\n");
      }
      if (!is_valid_cell(c)) {
        die("Invalid char: '%c'\n", c);
      }
      grid->data[row][col] = c;
    }
  }

  ssize_t line_len = getline(&line_buf, &buf_size, stdin);
  if (line_len > 0) {
    for (int i = 0; i < line_len; i++) {
      if (!isspace(line_buf[i])) {
        die("Too many rows\n");
      }
    }
  }

  free(line_buf);
}

void grid_print(const struct input_grid *g) {
  for (unsigned row = 0; row < GRID_SIZE; row++) {
    for (unsigned col = 0; col < GRID_SIZE; col++) {
      putchar(g->data[row][col]);
    }
    putchar('\n');
  }
}

typedef uint32_t opt_grid_t;
static_assert(GRID_SIZE * GRID_SIZE <= 32, "opt_grid_t not big enough");

enum opt_cell {
  OPT_BUG = 1,
  OPT_EMPTY = 0,
};

enum opt_cell cell_to_opt(enum cell c) {
  if (c == CELL_BUG) {
    return OPT_BUG;
  } else {
    return OPT_EMPTY;
  }
}

enum opt_cell opt_grid_get(opt_grid_t g, unsigned row, unsigned col) {
  uint32_t shift = row * GRID_SIZE + col;
  return (g >> shift) & 1;
}

void opt_grid_set(opt_grid_t *g, unsigned row, unsigned col,
                  enum opt_cell new) {
  uint32_t shift = row * GRID_SIZE + col;
  uint32_t mask = 1 << shift;
  *g = (*g & ~mask) | (new << shift);
}

opt_grid_t make_opt_grid(const struct input_grid *input) {
  opt_grid_t g = 0;
  for (unsigned row = 0; row < GRID_SIZE; row++) {
    for (unsigned col = 0; col < GRID_SIZE; col++) {
      opt_grid_set(&g, row, col, cell_to_opt(input->data[row][col]));
    }
  }
  return g;
}

void opt_grid_print(opt_grid_t g) {
  for (unsigned row = 0; row < GRID_SIZE; row++) {
    for (unsigned col = 0; col < GRID_SIZE; col++) {
      putchar(opt_grid_get(g, row, col) == OPT_BUG ? '#' : '.');
    }
    putchar('\n');
  }
}

unsigned opt_grid_count_bugs(opt_grid_t g) {
  unsigned count = 0;
  while (g != 0) {
    if (g & 1) {
      count++;
    }
    g >>= 1;
  }
  return count;
}

unsigned count_neighbors(opt_grid_t g, unsigned row, unsigned col) {
  unsigned count = 0;
  if (row > 0 && opt_grid_get(g, row - 1, col) == OPT_BUG) {
    count++;
  }
  if (col > 0 && opt_grid_get(g, row, col - 1) == OPT_BUG) {
    count++;
  }
  if (row < GRID_SIZE - 1 && opt_grid_get(g, row + 1, col) == OPT_BUG) {
    count++;
  }
  if (col < GRID_SIZE - 1 && opt_grid_get(g, row, col + 1) == OPT_BUG) {
    count++;
  }
  return count;
}

opt_grid_t grid_advance(opt_grid_t current) {
  opt_grid_t g = 0;
  for (unsigned row = 0; row < GRID_SIZE; row++) {
    for (unsigned col = 0; col < GRID_SIZE; col++) {
      enum opt_cell cur = opt_grid_get(current, row, col);
      unsigned neighbors = count_neighbors(current, row, col);
      enum opt_cell new = cur;
      if (cur == OPT_BUG && neighbors != 1) {
        new = OPT_EMPTY;
      } else if (cur == OPT_EMPTY && (neighbors == 1 || neighbors == 2)) {
        new = OPT_BUG;
      }
      opt_grid_set(&g, row, col, new);
    }
  }
  return g;
}

struct grid_set {
  size_t cap;
  size_t size;
  opt_grid_t *data;
};

void gs_init(struct grid_set *s) {
  s->cap = 100;
  s->size = 0;
  s->data = malloc(s->cap * sizeof(s->data[0]));
}

bool gs_contains_or_insert(struct grid_set *s, opt_grid_t g) {
  for (unsigned i = 0; i < s->size; i++) {
    if (s->data[i] == g) {
      return true;
    }
  }

  if (s->size >= s->cap) {
    s->cap *= 2;
    s->data = realloc(s->data, s->cap);
  }
  s->data[s->size++] = g;
  return false;
}

void gs_destroy(struct grid_set *s) { free(s->data); }

void part1(const struct input_grid *input) {
  struct grid_set cache;
  gs_init(&cache);

  opt_grid_t current = make_opt_grid(input);

  while (!gs_contains_or_insert(&cache, current)) {
    current = grid_advance(current);
  }

  gs_destroy(&cache);

  printf("\nPart1: %u\n", current);
}

#define PART2_MINUTES 200
#define MAX_LEVEL_COUNT (2 * (PART2_MINUTES + 1) + 1)
#define CENTER_LEVEL (MAX_LEVEL_COUNT / 2)

struct rec_grid_space {
  // Number of levels from the center which contain any bugs
  unsigned radius;
  opt_grid_t levels[MAX_LEVEL_COUNT];
};

enum opt_cell rec_grid_get(const struct rec_grid_space *space, int level,
                           unsigned row, unsigned col) {
  assert(level >= 0);
  assert(level < MAX_LEVEL_COUNT);
  assert(!(row == 2 && col == 2));
  return opt_grid_get(space->levels[level], row, col);
}

unsigned rec_count_neigh(const struct rec_grid_space *space, int level,
                         unsigned row, unsigned col) {
  unsigned count = 0;

#define END (GRID_SIZE - 1)
#define FOR(var) for (unsigned var = 0; var < GRID_SIZE; var++)
#define CHECK_POS(dlevel, r, c)                                                \
  if (rec_grid_get(space, level + dlevel, r, c) == OPT_BUG) {                  \
    count++;                                                                   \
  }

  // Left
  if (col == 0) {
    // Right center of containing
    CHECK_POS(-1, 2, 1);
  } else if (row == 2 && col == 3) {
    // Right edge of inner
    FOR(r) CHECK_POS(1, r, END);
  } else {
    CHECK_POS(0, row, col - 1);
  }

  // Right
  if (col == GRID_SIZE - 1) {
    CHECK_POS(-1, 2, 3);
  } else if (row == 2 && col == 1) {
    FOR(r) CHECK_POS(1, r, 0);
  } else {
    CHECK_POS(0, row, col + 1);
  }

  // Up
  if (row == 0) {
    CHECK_POS(-1, 1, 2);
  } else if (row == 3 && col == 2) {
    FOR(c) CHECK_POS(1, END, c);
  } else {
    CHECK_POS(0, row - 1, col);
  }

  // Down
  if (row == GRID_SIZE - 1) {
    CHECK_POS(-1, 3, 2);
  } else if (row == 1 && col == 2) {
    FOR(c) CHECK_POS(1, 0, c);
  } else {
    CHECK_POS(0, row + 1, col);
  }

#undef CHECK_POS
#undef FOR
#undef END

  return count;
}

opt_grid_t rec_grid_level_advance(const struct rec_grid_space *current,
                                  int level) {
  opt_grid_t g = 0;
  for (unsigned row = 0; row < GRID_SIZE; row++) {
    for (unsigned col = 0; col < GRID_SIZE; col++) {
      if (row == 2 && col == 2) {
        continue;
      }

      enum opt_cell cur = rec_grid_get(current, level, row, col);
      unsigned neighbors = rec_count_neigh(current, level, row, col);
      enum opt_cell new = cur;
      if (cur == OPT_BUG && neighbors != 1) {
        new = OPT_EMPTY;
      } else if (cur == OPT_EMPTY && (neighbors == 1 || neighbors == 2)) {
        new = OPT_BUG;
      }
      opt_grid_set(&g, row, col, new);
    }
  }
  return g;
}

void rec_grid_advance(const struct rec_grid_space *current,
                      struct rec_grid_space *next) {
  memset(next->levels, 0, sizeof(next->levels));
  int start_level = CENTER_LEVEL - current->radius;
  int end_level = CENTER_LEVEL + current->radius;
  // Go through level just outside of the radius
  for (int level = start_level - 1; level <= end_level + 1; level++) {
    next->levels[level] = rec_grid_level_advance(current, level);
  }

  next->radius = current->radius;
  // Quick check to only expand the radius if necessary
  if (next->levels[start_level - 1] != 0 || next->levels[end_level + 1] != 0) {
    next->radius++;
  }
}

unsigned long rec_count_bugs(const struct rec_grid_space *space) {
  unsigned long count = 0;
  for (unsigned level = 0; level < MAX_LEVEL_COUNT; level++) {
    count += opt_grid_count_bugs(space->levels[level]);
  }
  return count;
}

void rec_grid_print(const struct rec_grid_space *space) {
  int start_level = CENTER_LEVEL - space->radius;
  int end_level = CENTER_LEVEL + space->radius;
  // Go through level just outside of the radius
  for (int level = start_level; level <= end_level; level++) {
    printf("\nDepth %i:\n", level - CENTER_LEVEL);
    opt_grid_print(space->levels[level]);
  }
}

void part2(const struct input_grid *input) {
  // Double buffer
  struct rec_grid_space a;
  struct rec_grid_space b;

  struct rec_grid_space *current = &a;
  struct rec_grid_space *next = &b;

  current->radius = 0;
  memset(current->levels, 0, sizeof(current->levels));
  current->levels[CENTER_LEVEL] = make_opt_grid(input);

  for (unsigned minute = 1; minute <= PART2_MINUTES; minute++) {
    rec_grid_advance(current, next);

    void *tmp = current;
    current = next;
    next = tmp;
  }

  printf("Part2: %lu\n", rec_count_bugs(current));
}

int main(void) {
  struct input_grid input;
  read_input(&input);
  part1(&input);
  part2(&input);
  return 0;
}
