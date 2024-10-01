#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

enum cell {
  CELL_EMPTY = '.',
  CELL_WALL = '#',
  // Space "outside" of the maze which can't be used
  CELL_OUTER = ' ',
};

#define MAX_GRID_ROWS 127
#define MAX_GRID_COLS 135

/**
 * Raw input grid.
 */
struct input_grid {
  unsigned rows;
  unsigned cols;
  char data[MAX_GRID_ROWS][MAX_GRID_COLS];
};

void read_input(struct input_grid *grid);

struct coord {
  unsigned row;
  unsigned col;
};

enum portal_type {
  PORT_INNER = 0,
  PORT_OUTER = 1,
};

union portal_id {
  char name[2];
  uint16_t key;
};

// Make sure fields are the same size
static_assert(sizeof(((union portal_id *)NULL)->name) ==
                  sizeof(((union portal_id *)NULL)->key),
              "Portal name and ID union sizes do not match");

struct portal_desc {
  union portal_id id;
  // Indexed by portal_type
  // [0] - Inner portal
  // [1] - Outer portal
  struct coord pos[2];
};

#define MAX_PORTAL_COUNT 63

#define START_PORTAL_ID ((union portal_id){.name = {'A', 'A'}})
#define FINISH_PORTAL_ID ((union portal_id){.name = {'Z', 'Z'}})

typedef uint8_t grid_cell;

struct maze {
  unsigned rows;
  unsigned cols;
  grid_cell grid[MAX_GRID_ROWS][MAX_GRID_COLS];
  struct coord start;
  struct coord finish;

  unsigned portal_count;
  struct portal_desc portals[MAX_PORTAL_COUNT];
};

inline bool coord_zero(struct coord c) { return c.row == 0 && c.col == 0; }

inline bool is_portal_cell(grid_cell cell) { return (cell & (1 << 7)) != 0; }

inline uint8_t get_portal_index(grid_cell cell) { return cell & 0b00111111; }

inline enum portal_type get_portal_type(grid_cell cell) {
  return (cell >> 6) & 1;
}

inline enum portal_type get_opposite_type(enum portal_type t) {
  return t == PORT_INNER ? PORT_OUTER : PORT_INNER;
}

grid_cell maze_get(const struct maze *maze, unsigned row, unsigned col);
grid_cell maze_getc(const struct maze *maze, struct coord c);

void process_maze(struct maze *maze, const struct input_grid *input);
void print_maze(const struct maze *maze);
