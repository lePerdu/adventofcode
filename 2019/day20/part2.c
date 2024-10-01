#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "input.h"

struct step {
  struct coord pos;
  uint8_t depth;
};

struct step_list {
  unsigned size;
  struct step data[5];
};

#define MAX_QUEUE_SIZE 1000

struct explore_state {
  unsigned dist;
  struct coord pos;
  uint8_t depth;
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
                                const struct maze *maze, struct coord pos,
                                uint8_t depth) {
  output->size = 0;
#define CHECK_COORD(row, col)                                                  \
  {                                                                            \
    struct coord c = (struct coord){(row), (col)};                             \
    grid_cell cell = maze_getc(maze, c);                                       \
    if (cell == CELL_EMPTY || is_portal_cell(cell)) {                          \
      output->data[output->size++] = (struct step){c, depth};                  \
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
    enum portal_type portal_type = get_portal_type(current);
    if (depth > 0 || portal_type == PORT_INNER) {
      output->data[output->size++] = (struct step){
          // Portal goes to the one of the opposite type
          .pos = p->pos[get_opposite_type(portal_type)],
          .depth = portal_type == PORT_INNER ? depth + 1 : depth - 1,
      };
    }
  }
}

#define MAX_DEPTH 254

struct visit_cache {
  struct step parents[MAX_DEPTH][MAX_GRID_ROWS][MAX_GRID_COLS];
};

bool has_visited(const struct visit_cache *v, uint8_t depth, struct coord pos) {
  return !coord_zero(v->parents[depth][pos.row][pos.col].pos);
}

bool should_visit(struct visit_cache *v, uint8_t depth, struct coord pos,
                  struct step prev_step) {
  struct step *vis_ptr = &v->parents[depth][pos.row][pos.col];
  if (coord_zero(vis_ptr->pos)) {
    *vis_ptr = prev_step;
    return true;
  } else {
    return false;
  }
}

static void print_path(const struct visit_cache *v, const struct step *to) {
  if (to->depth == 255) {
    return;
  }
  if (coord_zero(to->pos)) {
    die("Path incomplete\n");
  }

  const struct step *from = &v->parents[to->depth][to->pos.row][to->pos.col];
  print_path(v, from);
  printf("(%u, %u)[%u] -> (%u, %u)[%u]\n", from->pos.row, from->pos.col,
         from->depth, to->pos.row, to->pos.col, to->depth);
}

static unsigned find_shortest_dist(const struct maze *maze) {
  struct visit_cache *visited = malloc(sizeof(*visited));
  memset(visited, 0, sizeof(*visited));

  struct explore_queue *queue = malloc(sizeof(*queue));
  qinit(queue);
  qinsert(queue, (struct explore_state){
                  .pos = maze->start,
                  .dist = 0,
                  .depth = 0,
              });
  should_visit(visited, 0, maze->start, (struct step){maze->start, 255});

  unsigned result = 0;
  while (!qempty(queue)) {
    struct explore_state current = qremove(queue);
    if (current.depth == 0 && current.pos.row == maze->finish.row &&
        current.pos.col == maze->finish.col) {
      print_path(visited, &(struct step){maze->finish, 0});
      result = current.dist;
      break;
    }

    struct step_list next_steps;
    maze_get_next_steps(&next_steps, maze, current.pos, current.depth);
    for (unsigned i = 0; i < next_steps.size; i++) {
      const struct step *s = &next_steps.data[i];
      if (s->depth >= MAX_DEPTH) {
        die("Reached max depth\n");
      }

      if (should_visit(visited, s->depth, s->pos,
                       (struct step){current.pos, current.depth})) {
        qinsert(queue, (struct explore_state){
                        .pos = s->pos,
                        .dist = current.dist + 1,
                        .depth = s->depth,
                    });
      }
    }
  }

  free(visited);
  free(queue);

  if (result == 0) {
    die("Path not found\n");
  } else {
    return result;
  }
}

void part2(const struct maze *input) {
  unsigned ans = find_shortest_dist(input);
  printf("Part2: %u\n", ans);
}
