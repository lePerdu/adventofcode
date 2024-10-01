#ifndef GRAPH_H_
#define GRAPH_H_

#include <stdint.h>

#include "maze.h"

#define MAX_KEY_COUNT 26
#define MAX_NODE_COUNT (MAX_DOOR_COUNT + 2 * MAX_KEY_COUNT)

#define KEYS_OFFSET MAX_DOOR_COUNT
#define LOCKS_OFFSET (KEYS_OFFSET + MAX_KEY_COUNT)

typedef uint8_t graph_node;

struct edge_list {
  uint8_t size;
  graph_node edges[MAX_NODE_COUNT - 1];
};

struct graph {
  uint8_t key_count;
  // TODO Remove this and inline distances into the edge list?
  unsigned matrix[MAX_NODE_COUNT][MAX_NODE_COUNT];
  struct edge_list edges_from_node[MAX_NODE_COUNT];
};

inline bool node_is_door(graph_node node) { return node < MAX_DOOR_COUNT; }

inline bool node_is_key(graph_node node) {
  return KEYS_OFFSET <= node && node < LOCKS_OFFSET;
}

inline bool node_is_lock(graph_node node) {
  return LOCKS_OFFSET <= node && node < MAX_NODE_COUNT;
}

graph_node get_node_index(char node_char);

char get_node_char(graph_node node);

int8_t get_key_index(graph_node node);

void build_graph(struct graph *g, const struct maze *maze);

void print_graph(const struct graph *g);

#endif
