#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "maze.h"

struct coord coord_move(struct coord c, enum direction d) {
  switch (d) {
  case DIR_U:
    c.row--;
    break;
  case DIR_D:
    c.row++;
    break;
  case DIR_L:
    c.col--;
    break;
  case DIR_R:
    c.col++;
    break;
  }
  return c;
}

bool is_entry_cell(char cell) {
  return cell == CELL_DOOR || ('1' <= cell && cell <= '4');
}

bool is_graph_node_cell(char cell) {
  return is_entry_cell(cell) || isalpha(cell);
}

bool is_valid_cell(char cell) {
  return cell == CELL_WALL || cell == CELL_EMPTY || is_graph_node_cell(cell);
}

static bool read_line(struct maze *maze, char **line_buf) {
  size_t line_size;
  if (getline(line_buf, &line_size, stdin) < 0) {
    perror_die("Failed to read input");
  }

  size_t line_offset = 0;
  char *row = maze->grid[maze->rows];
  unsigned col_offset = 0;
  unsigned line_len = 0;
  char c;
  while (1) {
    c = (*line_buf)[line_offset];
    line_offset++;
    if (c == '\n' || c == 0) {
      break;
    } else if (is_valid_cell(c)) {
      row[col_offset] = c;
    } else {
      die("Invalid char: '%c'\n", c);
    }

    col_offset++;
    line_len++;
  }

  if (line_len == 0) {
    return false;
  }

  if (maze->cols == 0) {
    // First line
    maze->cols = line_len;
  } else if (line_len != maze->cols) {
    die("Non-matchine line lengths\n");
  }

  maze->rows++;
  return true;
}

void read_maze(struct maze *input) {
  input->rows = 0;
  input->cols = 0;
  memset(input->grid, 0, sizeof(input->grid));

  char *line_buf = NULL;
  while (read_line(input, &line_buf)) {
  }

  free(line_buf);

  if (input->rows * input->cols == 0) {
    die("Empty grid\n");
  }
}

void print_maze(const struct maze *maze) {
  for (unsigned row = 0; row < maze->rows; row++) {
    for (unsigned col = 0; col < maze->cols; col++) {
      putchar(maze->grid[row][col]);
    }
    putchar('\n');
  }
}
