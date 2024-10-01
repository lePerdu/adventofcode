#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "input.h"

static bool is_valid_cell(char cell) {
  return cell == CELL_EMPTY || cell == CELL_WALL || cell == CELL_OUTER ||
         isupper(cell);
}

static bool read_line(struct input_grid *grid, char **line_buf) {
  size_t line_size;
  if (getline(line_buf, &line_size, stdin) < 0) {
    perror_die("Failed to read input");
  }

  size_t line_offset = 0;
  char *row = grid->data[grid->rows];
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

  if (line_len > MAX_GRID_COLS) {
    die("Row too long\n");
  }

  if (grid->rows > MAX_GRID_ROWS) {
    die("Too many rows\n");
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

void read_input(struct input_grid *grid) {
  grid->rows = 0;
  grid->cols = 0;
  memset(grid->data, 0, sizeof(grid->data));

  char *line_buf = NULL;
  while (read_line(grid, &line_buf)) {
  }

  free(line_buf);

  if (grid->rows * grid->cols == 0) {
    die("Empty grid\n");
  }
}

struct portal_endpoint {
  union portal_id id;
  struct coord pos;
};

grid_cell maze_get(const struct maze *maze, unsigned row, unsigned col) {
  if (0 <= row && row < maze->rows && 0 <= col && col < maze->cols) {
    return maze->grid[row][col];
  } else {
    return CELL_OUTER;
  }
}

grid_cell maze_getc(const struct maze *maze, struct coord c) {
  return maze_get(maze, c.row, c.col);
}

static void maze_set(struct maze *maze, unsigned row, unsigned col,
                     grid_cell cell) {
  if (0 <= row && row < maze->rows && 0 <= col && col < maze->cols) {
    maze->grid[row][col] = cell;
  } else {
    die("Coord out of bounds: (%u, %u)\n", row, col);
  }
}

static void maze_setc(struct maze *maze, struct coord c, grid_cell cell) {
  maze_set(maze, c.row, c.col, cell);
}

/**
 * Check for a portal, and remove from the maze grid if it exists.
 */
static bool try_extract_portal_endpoint(struct portal_endpoint *portal,
                                        struct maze *maze, unsigned row,
                                        unsigned col) {
  char cell = maze_get(maze, row, col);
  if (isupper(cell)) {
    portal->id.name[0] = cell;
    maze_set(maze, row, col, CELL_OUTER);

    char c_right = maze_get(maze, row, col + 1);
    char c_down = maze_get(maze, row + 1, col);

    struct coord possible_ends[2];
    if (isupper(c_right)) {
      portal->id.name[1] = c_right;
      maze_set(maze, row, col + 1, CELL_OUTER);
      possible_ends[0] = (struct coord){row, col - 1};
      possible_ends[1] = (struct coord){row, col + 2};
    } else if (isupper(c_down)) {
      portal->id.name[1] = c_down;
      maze_set(maze, row + 1, col, CELL_OUTER);
      possible_ends[0] = (struct coord){row - 1, col};
      possible_ends[1] = (struct coord){row + 2, col};
    } else {
      die("Found single-character portal char at (%u, %u): %c\n", row, col,
          cell);
    }

    if (maze_getc(maze, possible_ends[0]) == CELL_EMPTY) {
      portal->pos = possible_ends[0];
    } else if (maze_getc(maze, possible_ends[1]) == CELL_EMPTY) {
      portal->pos = possible_ends[1];
    } else {
      die("Portal entrance not found for portal at (%u, %u)\n", row, col);
    }

    return true;
  } else {
    return false;
  }
}

#define OUTER_BORDER 3

static enum portal_type portal_type_from_pos(const struct maze *maze,
                                        struct coord pos) {
  if (pos.row < OUTER_BORDER || maze->rows - OUTER_BORDER <= pos.row ||
      pos.col < OUTER_BORDER || maze->cols - OUTER_BORDER <= pos.col) {
    return PORT_OUTER;
  } else {
    return PORT_INNER;
  }
}

static void maze_add_or_update_portal(struct maze *maze,
                                      const struct portal_endpoint portal_end) {
  if (portal_end.id.key == START_PORTAL_ID.key) {
    maze->start = portal_end.pos;
    return;
  }
  if (portal_end.id.key == FINISH_PORTAL_ID.key) {
    maze->finish = portal_end.pos;
    return;
  }

  enum portal_type type = portal_type_from_pos(maze, portal_end.pos);

  // Try to link with existing portal
  for (unsigned i = 0; i < maze->portal_count; i++) {
    struct portal_desc *p = &maze->portals[i];
    if (p->id.key == portal_end.id.key) {

      // TODO Check that exactly 2 of each portal is found
      maze->portals[i].pos[type] = portal_end.pos;
      return;
    }
  }

  // Not found, so add a new one
  unsigned new_index = maze->portal_count++;
  maze->portals[new_index] = (struct portal_desc){
      .id = portal_end.id,
      .pos = {{0, 0}, {0, 0}},
  };
  maze->portals[new_index].pos[type] = portal_end.pos;
}

static grid_cell make_portal_cell(uint8_t portal_index, enum portal_type portal_type) {
  // Top 2 bits are free since there are at most 64 portals
  static_assert(MAX_PORTAL_COUNT < (1 << 6), "Too many portals allowed for bitmask");
  return (1 << 7) | (portal_type << 6) | portal_index;
}

void process_maze(struct maze *maze, const struct input_grid *input) {
  maze->rows = input->rows;
  maze->cols = input->cols;
  memcpy(maze->grid, input->data, sizeof(maze->grid));
  static_assert(sizeof(maze->grid) == sizeof(input->data),
                "Grid sizes do not match");
  maze->start = (struct coord){0, 0};
  maze->finish = (struct coord){0, 0};

  maze->portal_count = 0;

  for (unsigned row = 0; row < maze->rows; row++) {
    for (unsigned col = 0; col < maze->cols; col++) {
      struct portal_endpoint endpoint;
      if (try_extract_portal_endpoint(&endpoint, maze, row, col)) {
        maze_add_or_update_portal(maze, endpoint);
      }
    }
  }

  if (maze->start.row == 0 && maze->start.col == 0) {
    die("Start not found\n");
  }
  if (maze->finish.row == 0 && maze->finish.col == 0) {
    die("Finish not found\n");
  }

  // TODO Is sorting needed?
  // qsort(maze->sorted_portals, maze->portal_count,
  //       sizeof(maze->sorted_portals[0]), compare_portal_ids);

  for (unsigned i = 0; i < maze->portal_count; i++) {
    // Make sure all portals have both sides
    assert(!coord_zero(maze->portals[i].pos[PORT_INNER]));
    assert(!coord_zero(maze->portals[i].pos[PORT_OUTER]));
    maze_setc(maze, maze->portals[i].pos[PORT_INNER], make_portal_cell(i, PORT_INNER));
    maze_setc(maze, maze->portals[i].pos[PORT_OUTER], make_portal_cell(i, PORT_OUTER));
  }
}

void print_maze(const struct maze *maze) {
  for (unsigned row = 0; row < maze->rows; row++) {
    for (unsigned col = 0; col < maze->cols; col++) {
      grid_cell cell = maze_get(maze, row, col);
      if (is_portal_cell(cell)) {
        if (get_portal_type(cell) == PORT_INNER) {
          putchar('I');
        } else {
          putchar('O');
        }
      } else {
        putchar(cell);
      }
    }
    putchar('\n');
  }
  putchar('\n');
  for (unsigned i = 0; i < maze->portal_count; i++) {
    const struct portal_desc *p = &maze->portals[i];
    printf("%.2s: (%u, %u) <=> (%u, %u)\n", p->id.name, p->pos[0].row,
           p->pos[0].col, p->pos[1].row, p->pos[1].col);
  }
  putchar('\n');
  printf("start: (%u, %u)\n", maze->start.row, maze->start.col);
  printf("finish: (%u, %u)\n", maze->finish.row, maze->finish.col);
  putchar('\n');
}
