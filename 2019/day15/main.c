#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/types.h>

#include "common.h"
#include "intcode.c"

enum cell {
  CELL_WALL = 0,
  CELL_EMPTY = 1,
  CELL_OXYGEN = 2,
  CELL_UNKNOWN = 255,
};

enum direction {
  DIR_N = 1,
  DIR_S = 2,
  DIR_W = 3,
  DIR_E = 4,
  DIR_FIRST = DIR_N,
  DIR_LAST = DIR_E,
};

// Grid size found by experimentation
#define GRID_SIZE 42
#define INITIAL_POS ((struct coord){.row = GRID_SIZE / 2, .col = GRID_SIZE / 2})

struct coord {
  int row;
  int col;
};

struct state {
  bool found_oxygen_system;
  struct coord oxygen_system_pos;
  uint8_t grid[GRID_SIZE][GRID_SIZE];
  struct machine machine;
};

struct coord coord_move(struct coord c, enum direction d) {
  switch (d) {
  case DIR_N:
    c.row--;
    break;
  case DIR_S:
    c.row++;
    break;
  case DIR_W:
    c.col--;
    break;
  case DIR_E:
    c.col++;
    break;
  }
  return c;
}

bool coord_in_range(struct coord c) {
  return (0 <= c.row && c.row < GRID_SIZE && 0 <= c.col && c.col < GRID_SIZE);
}

enum direction dir_reverse(enum direction d) {
  switch (d) {
  case DIR_N:
    return DIR_S;
  case DIR_S:
    return DIR_N;
  case DIR_W:
    return DIR_E;
  case DIR_E:
    return DIR_W;
  }
}

void grid_print(const struct state *s) {
  for (int col = 0; col < GRID_SIZE; col++) {
    putchar('=');
  }
  putchar('\n');

  for (int row = 0; row < GRID_SIZE; row++) {
    for (int col = 0; col < GRID_SIZE; col++) {
      char c;
      if (INITIAL_POS.row == row && INITIAL_POS.col == col) {
        c = 'I';
      } else {

        switch (s->grid[row][col]) {
        case CELL_WALL:
          c = '#';
          break;
        case CELL_EMPTY:
          c = '.';
          break;
        case CELL_OXYGEN:
          c = 'O';
          break;
        case CELL_UNKNOWN:
          c = ' ';
          break;
        }
      }
      putchar(c);
    }
    putchar('\n');
  }
  putchar('\n');
}

enum cell send_command(struct state *s, enum direction dir) {
  if (!machine_can_input(&s->machine)) {
    die("Expected machine to accept input\n");
  }
  machine_input(&s->machine, dir);

  machine_run_til_wait(&s->machine);

  if (!machine_has_output(&s->machine)) {
    die("Expected machine to have output\n");
  }
  intcode output = machine_output(&s->machine);
  return output;
}

bool explore_recursive(struct state *s, struct coord droid_pos) {
  grid_print(s);
  for (enum direction d = DIR_FIRST; d <= DIR_LAST; d++) {
    struct coord check_coord = coord_move(droid_pos, d);
    if (!coord_in_range(check_coord)) {
      die("Grid not big enough\n");
    }

    if (s->grid[check_coord.row][check_coord.col] != CELL_UNKNOWN) {
      continue;
    }

    enum cell output = send_command(s, d);
    s->grid[check_coord.row][check_coord.col] = output;

    if (output == CELL_OXYGEN && !s->found_oxygen_system) {
      s->found_oxygen_system = true;
      s->oxygen_system_pos = check_coord;
    }

    if (output == CELL_WALL) {
      // No-op
    } else if (output == CELL_EMPTY || output == CELL_OXYGEN) {
      explore_recursive(s, check_coord);
      // Reset state (assume recursive all also did so)
      send_command(s, dir_reverse(d));
    } else {
      die("Invalid output command: %d\n", output);
    }
  }

  return false;
}

void state_init(struct state *s, const struct intcode_program *input) {
  machine_init(&s->machine, input);

  struct coord initial_pos = INITIAL_POS;
  memset(s->grid, CELL_UNKNOWN, sizeof(s->grid));
  s->grid[initial_pos.row][initial_pos.col] = CELL_EMPTY;
  s->found_oxygen_system = false;
}

/**
 * Fully explore the maze.
 */
void explore_maze(struct state *s) {
  explore_recursive(s, INITIAL_POS);

  if (s->found_oxygen_system) {
    puts("Found oxygen system");
    grid_print(s);
  } else {
    die("Could not find oxygen system\n");
  }
}

#define QUEUE_SIZE GRID_SIZE

struct queue_entry {
  struct coord pos;
  unsigned steps;
  // Steps + distance to destination
  unsigned cost;
};

struct heap {
  unsigned size;
  struct queue_entry data[QUEUE_SIZE];
};

void heap_init(struct heap *h) { h->size = 0; }

void heap_insert(struct heap *h, struct queue_entry entry) {
  if (h->size >= QUEUE_SIZE) {
    die("Heap too small\n");
  }

  h->data[h->size] = entry;
  // Propogate upwards until heap property is in place
  unsigned index = h->size;
  unsigned parent_index = (index - 1) / 2;

  while (index > 0 && h->data[index].cost <= h->data[parent_index].cost) {
    struct queue_entry tmp = h->data[parent_index];
    h->data[parent_index] = h->data[index];
    h->data[index] = tmp;

    index = parent_index;
    parent_index = (index - 1) / 2;
  }

  h->size++;
}

struct queue_entry heap_remove(struct heap *h) {
  if (h->size == 0) {
    die("Heap empty\n");
  }

  struct queue_entry entry = h->data[0];
  h->size--;
  h->data[0] = h->data[h->size];

  // Propogate downwards until heap property is in place
  unsigned index = 0;

  while (1) {
    unsigned max_index = index;

    // Check if either child exists and is greater

    unsigned child1 = index * 2 + 1;
    if (child1 < h->size && h->data[child1].cost > h->data[max_index].cost) {
      max_index = child1;
    }

    unsigned child2 = child1 + 1;
    if (child2 < h->size && h->data[child2].cost > h->data[max_index].cost) {
      max_index = child2;
    }

    if (max_index != index) {
      struct queue_entry tmp = h->data[max_index];
      h->data[max_index] = h->data[index];
      h->data[index] = tmp;
      index = max_index;
    } else {
      break;
    }
  }

  return entry;
}

struct queue_entry make_queue_entry(const struct state *s, struct coord pos,
                                    unsigned steps) {
  unsigned dist = abs(pos.row - s->oxygen_system_pos.row) +
                  abs(pos.col - s->oxygen_system_pos.col);
  return (struct queue_entry){
      .pos = pos,
      .steps = steps,
      .cost = steps + dist,
  };
}

bool is_visitable(const struct state *s, struct coord c) {
  enum cell cell = s->grid[c.row][c.col];
  return cell == CELL_EMPTY || cell == CELL_OXYGEN;
}

unsigned find_shortest_distance(const struct state *s) {
  // Marked once put in the queue to avoid extra queue entries
  unsigned visited_with_steps[GRID_SIZE][GRID_SIZE];
  for (unsigned row = 0; row < GRID_SIZE; row++) {
    for (unsigned col = 0; col < GRID_SIZE; col++) {
      visited_with_steps[row][col] = -1; // Max value
    }
  }

  // DFS queue for A*, sorted by cost
  struct heap queue;
  heap_init(&queue);

  heap_insert(&queue, make_queue_entry(s, INITIAL_POS, 0));
  while (queue.size > 0) {
    struct queue_entry current = heap_remove(&queue);

    if (current.pos.row == s->oxygen_system_pos.row &&
        current.pos.col == s->oxygen_system_pos.col) {
      return current.steps;
    }

    for (enum direction d = DIR_FIRST; d <= DIR_LAST; d++) {
      struct coord check_coord = coord_move(current.pos, d);
      if (!is_visitable(s, check_coord)) {
        continue;
      }

      unsigned check_coord_steps = current.steps + 1;

      // Check if already in queue or visited
      unsigned *already_visited_steps =
          &visited_with_steps[check_coord.row][check_coord.col];
      if (*already_visited_steps <= check_coord_steps) {
        continue;
      }
      *already_visited_steps = check_coord_steps;

      struct queue_entry new_entry =
          make_queue_entry(s, check_coord, check_coord_steps);
      heap_insert(&queue, new_entry);
    }
  }

  die("Could not find path\n");
}

void part1(const struct state *s) {
  unsigned distance = find_shortest_distance(s);

  printf("Part1: %u\n", distance);
}

struct buffer {
  unsigned size;
  struct coord data[QUEUE_SIZE];
};

void buf_insert(struct buffer *b, struct coord c) {
  if (b->size >= QUEUE_SIZE) {
    die("Buffer too small\n");
  }

  b->data[b->size++] = c;
}

struct coord buf_remove(struct buffer *b) {
  if (b->size == 0) {
    die("Buffer empty\n");
  }

  return b->data[--b->size];
}

unsigned find_fill_time(struct state *s) {
  struct buffer buf1;
  buf1.size = 0;
  struct buffer buf2;
  buf2.size = 0;

  struct buffer *buf_current = &buf1;
  struct buffer *buf_next = &buf2;

  buf_current->size = 1;
  buf_current->data[0] = s->oxygen_system_pos;

  unsigned time = 0;
  while (1) {
    while (buf_current->size > 0) {
      struct coord edge_coord = buf_remove(buf_current);

      for (enum direction d = DIR_FIRST; d <= DIR_LAST; d++) {
        struct coord check_coord = coord_move(edge_coord, d);
        if (s->grid[check_coord.row][check_coord.col] != CELL_EMPTY) {
          continue;
        }

        buf_insert(buf_next, check_coord);
        s->grid[check_coord.row][check_coord.col] = CELL_OXYGEN;
      }
    }

    if (buf_next->size == 0) {
      return time;
    }

    time++;

    // swap buffers to avoid copying
    struct buffer *tmp = buf_current;
    buf_current = buf_next;
    buf_next = tmp;
  }
}

// Note: This modifies the grid state
void part2(struct state *s) {
  unsigned fill_time = find_fill_time(s);
  grid_print(s);
  printf("Part2: %u\n", fill_time);
}

int main(void) {
  struct intcode_program input;
  read_input(&input);

  struct state *s = malloc(sizeof(*s));
  state_init(s, &input);
  explore_maze(s);

  part1(s);
  part2(s);

  free(s);
  return 0;
}
