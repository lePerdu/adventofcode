#ifndef MAZE_H_
#define MAZE_H_

#include <stdbool.h>

enum cell {
  CELL_WALL = '#',
  CELL_DOOR = '@',
  CELL_EMPTY = '.',
  CELL_UNKNOWN = 0,
  CELL_VISITED = '*',
};

struct coord {
  unsigned row;
  unsigned col;
};

enum direction {
  DIR_U,
  DIR_D,
  DIR_L,
  DIR_R,

  // For iteration
  DIR_START = DIR_U,
  DIR_END = DIR_R,
};

#define MAX_DOOR_COUNT 4
#define MAX_GRID_SIZE 81

struct maze {
  unsigned rows;
  unsigned cols;
  char grid[MAX_GRID_SIZE][MAX_GRID_SIZE];
};

struct coord coord_move(struct coord c, enum direction d);

bool is_entry_cell(char cell);

bool is_graph_node_cell(char cell);

bool is_valid_cell(char cell);

void read_maze(struct maze *input);

void print_maze(const struct maze *maze);

#endif
