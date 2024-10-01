#include "graph.h"
#include "common.h"
#include "maze.h"
#include <stdint.h>
#include <string.h>

graph_node get_node_index(char node_char) {
  if (node_char == CELL_DOOR) {
    return 0;
  } else if ('1' <= node_char && node_char <= '4') {
    // Digits are also entry doors
    return node_char - '1';
  } else if ('a' <= node_char && node_char <= 'z') {
    return KEYS_OFFSET + node_char - 'a';
  } else if ('A' <= node_char && node_char <= 'Z') {
    return LOCKS_OFFSET + node_char - 'A';
  } else {
    die("Invalid graph node: '%c'\n", node_char);
  }
}

char get_node_char(graph_node node) {
  if (node_is_door(node)) {
    return node + '1';
  } else if (node_is_key(node)) {
    return node - KEYS_OFFSET + 'a';
  } else if (node_is_lock(node)) {
    return node - LOCKS_OFFSET + 'A';
  } else {
    die("Invalid graph index: %u\n", node);
  }
}

int8_t get_key_index(graph_node node) {
  if (node_is_door(node)) {
    return -1;
  } else if (node_is_key(node)) {
    return node - KEYS_OFFSET;
  } else if (node_is_lock(node)) {
    return node - LOCKS_OFFSET;
  } else {
    die("Invalid graph index: %u\n", node);
  }
}

static void update_graph(struct graph *g, char from, char to, unsigned dist) {
  graph_node from_index = get_node_index(from);
  graph_node to_index = get_node_index(to);

  unsigned current_dist = g->matrix[from_index][to_index];
  if (current_dist > 0 && dist < current_dist) {
    fprintf(stderr, "Reached node %c -> %c with 2 paths: %d, %d\n", from, to,
            current_dist, dist);
    return;
  }

  g->matrix[from_index][to_index] = dist;

  int8_t key_index = get_key_index(to_index);
  if (key_index >= 0 && (unsigned)key_index + 1 > g->key_count) {
    g->key_count = key_index + 1;
  }
}

struct explore_state {
  unsigned dist;
  char from_node;
  struct coord pos;
};

#define MAX_QUEUE_SIZE MAX_GRID_SIZE

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

static void explore_maze_from_cell(const struct maze *original_maze,
                                   struct coord start, struct graph *g) {
  // Copy maze to modify it
  struct maze maze;
  memcpy(&maze, original_maze, sizeof(maze));

  struct explore_queue queue;
  qinit(&queue);
  qinsert(&queue, (struct explore_state){
                      .dist = 0,
                      .from_node = maze.grid[start.row][start.col],
                      .pos = start,
                  });
  maze.grid[start.row][start.col] = CELL_VISITED;

  while (!qempty(&queue)) {
    struct explore_state node = qremove(&queue);

    unsigned adj_dist = node.dist + 1;
    for (enum direction d = DIR_START; d <= DIR_END; d++) {
      struct coord adj = coord_move(node.pos, d);
      char *cell = &maze.grid[adj.row][adj.col];
      if (*cell == CELL_EMPTY) {
        qinsert(&queue, (struct explore_state){
                            .dist = adj_dist,
                            .from_node = node.from_node,
                            .pos = adj,
                        });
      } else if (is_graph_node_cell(*cell)) {
        update_graph(g, node.from_node, *cell, adj_dist);
        qinsert(&queue, (struct explore_state){
                            .dist = 0,
                            .from_node = *cell,
                            .pos = adj,
                        });
      } else {
        continue;
      }
      *cell = CELL_VISITED;
    }
  }
}

static void build_edge_lists(struct graph *g) {
  for (graph_node from = 0; from < MAX_NODE_COUNT; from++) {
    struct edge_list *elist = &g->edges_from_node[from];
    elist->size = 0;

    for (graph_node to = 0; to < MAX_NODE_COUNT; to++) {
      if (g->matrix[from][to] == 0) {
        continue;
      }

      elist->edges[elist->size++] = to;
    }
  }
}

void build_graph(struct graph *g, const struct maze *maze) {
  g->key_count = 0;
  memset(g->matrix, 0, sizeof(g->matrix));

  struct coord c;
  for (c.row = 0; c.row < maze->rows; c.row++) {
    for (c.col = 0; c.col < maze->cols; c.col++) {
      if (is_graph_node_cell(maze->grid[c.row][c.col])) {
        explore_maze_from_cell(maze, c, g);
      }
    }
  }

  build_edge_lists(g);
}

void print_graph(const struct graph *g) {
  printf("Graphs contains %u keys\n", g->key_count);
  for (graph_node from = 0; from < MAX_NODE_COUNT; from++) {
    printf("%c: ", get_node_char(from));
    for (graph_node to = 0; to < MAX_NODE_COUNT; to++) {
      unsigned dist = g->matrix[from][to];
      if (dist > 0) {
        printf("%c(%d), ", get_node_char(to), dist);
      }
    }
    putchar('\n');
  }
}
