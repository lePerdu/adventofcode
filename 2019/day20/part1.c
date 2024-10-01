#include <string.h>

#include "common.h"
#include "input.h"

struct step_list {
  unsigned size;
  struct coord data[5];
};

#define MAX_QUEUE_SIZE 64

struct explore_state {
  unsigned dist;
  struct coord pos;
};

struct explore_queue {
  size_t head;
  size_t tail;
  struct explore_state data[MAX_QUEUE_SIZE];
};

static void qinit(struct explore_queue *q) {
  q->head = 0;
  q->tail = 0;
}

static void qinsert(struct explore_queue *q, struct explore_state node) {
  q->data[q->tail] = node;
  q->tail = q->tail < MAX_QUEUE_SIZE - 1 ? q->tail + 1 : 0;
  if (q->tail == q->head) {
    die("Queue full\n");
  }
}

static struct explore_state qremove(struct explore_queue *q) {
  if (q->head == q->tail) {
    die("Queue empty\n");
  }

  struct explore_state result = q->data[q->head];
  q->head = q->head < MAX_QUEUE_SIZE - 1 ? q->head + 1 : 0;
  return result;
}

static bool qempty(const struct explore_queue *q) { return q->head == q->tail; }

static void maze_get_next_steps(struct step_list *output,
                                const struct maze *maze, struct coord pos) {
  output->size = 0;
#define CHECK_COORD(row, col)                                                  \
  {                                                                            \
    struct coord c = (struct coord){(row), (col)};                             \
    grid_cell cell = maze_getc(maze, c);                                       \
    if (cell == CELL_EMPTY || is_portal_cell(cell)) {                          \
      output->data[output->size++] = c;                                        \
    }                                                                          \
  }

  CHECK_COORD(pos.row, pos.col + 1);
  CHECK_COORD(pos.row, pos.col - 1);
  CHECK_COORD(pos.row + 1, pos.col);
  CHECK_COORD(pos.row - 1, pos.col);
#undef CHECK_COORD

  grid_cell current = maze_getc(maze, pos);
  if (is_portal_cell(current)) {
    const struct portal_desc *p = &maze->portals[get_portal_index(current)];
    enum portal_type t = get_portal_type(current);
    // Portal goes to the one of the opposite type
    output->data[output->size++] = p->pos[get_opposite_type(t)];
  }
}

static unsigned find_shortest_dist(const struct maze *maze) {
  bool visited[MAX_GRID_ROWS][MAX_GRID_COLS];
  memset(visited, 0, sizeof(visited));

  struct explore_queue q;
  qinit(&q);
  qinsert(&q, (struct explore_state){
                  .pos = maze->start,
                  .dist = 0,
              });
  visited[maze->start.row][maze->start.col] = true;

  while (!qempty(&q)) {
    struct explore_state current = qremove(&q);
    if (current.pos.row == maze->finish.row &&
        current.pos.col == maze->finish.col) {
      return current.dist;
    }

    struct step_list next_steps;
    maze_get_next_steps(&next_steps, maze, current.pos);
    for (unsigned i = 0; i < next_steps.size; i++) {
      bool *vis_ptr = &visited[next_steps.data[i].row][next_steps.data[i].col];
      if (*vis_ptr) {
        continue;
      } else {
        *vis_ptr = true;
        qinsert(&q, (struct explore_state){
                        .pos = next_steps.data[i],
                        .dist = current.dist + 1,
                    });
      }
    }
  }

  die("Path not found\n");
}

void part1(const struct maze *input) {
  unsigned ans = find_shortest_dist(input);
  printf("Part1: %u\n", ans);
}
