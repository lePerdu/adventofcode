#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "graph.h"
#include "maze.h"

struct search_node {
  graph_node node;
  unsigned dist;
};

struct node_list {
  unsigned size;
  struct search_node data[MAX_NODE_COUNT];
};

struct search_state {
  graph_node current_node;
  unsigned total_dist;
  uint32_t keys_bitmask;
  static_assert(MAX_KEY_COUNT <= 32, "Keys bitmask not wide enough");
};

struct heap {
  uint64_t visited_bitmask;
  static_assert(MAX_NODE_COUNT <= 64, "Visited bitmask not wide enough");

  unsigned size;
  struct search_node data[MAX_NODE_COUNT];
};

static void hinit(struct heap *h) {
  h->visited_bitmask = 0;
  h->size = 0;
}

static void hinsert(struct heap *h, struct search_node entry) {
  if (h->size >= MAX_NODE_COUNT) {
    die("Heap too small\n");
  }

  h->visited_bitmask |= (uint64_t)1 << entry.node;

  h->data[h->size] = entry;
  // Propogate upwards until heap property is in place
  unsigned index = h->size;
  unsigned parent_index = (index - 1) / 2;

  while (index > 0 && h->data[index].dist <= h->data[parent_index].dist) {
    struct search_node tmp = h->data[parent_index];
    h->data[parent_index] = h->data[index];
    h->data[index] = tmp;

    index = parent_index;
    parent_index = (index - 1) / 2;
  }

  h->size++;
}

static struct search_node hremove(struct heap *h) {
  if (h->size == 0) {
    die("Heap empty\n");
  }

  struct search_node entry = h->data[0];
  h->size--;
  h->data[0] = h->data[h->size];

  // Propogate downwards until heap property is in place
  unsigned index = 0;

  while (1) {
    unsigned max_index = index;

    // Check if either child exists and is greater

    unsigned child1 = index * 2 + 1;
    if (child1 < h->size && h->data[child1].dist > h->data[max_index].dist) {
      max_index = child1;
    }

    unsigned child2 = child1 + 1;
    if (child2 < h->size && h->data[child2].dist > h->data[max_index].dist) {
      max_index = child2;
    }

    if (max_index != index) {
      struct search_node tmp = h->data[max_index];
      h->data[max_index] = h->data[index];
      h->data[index] = tmp;
      index = max_index;
    } else {
      break;
    }
  }

  return entry;
}

static bool hvisited(const struct heap *h, graph_node node) {
  return (h->visited_bitmask & ((uint64_t)1 << node)) != 0;
}

static void find_accessible_keys_dfs(struct node_list *output,
                                     const struct search_state *state,
                                     const struct graph *graph) {
  output->size = 0;

  struct heap q;
  hinit(&q);
  hinsert(&q, (struct search_node){
                  .node = state->current_node,
                  .dist = 0,
              });

  while (q.size > 0) {
    struct search_node current = hremove(&q);

    const struct edge_list *adj_list = &graph->edges_from_node[current.node];
    for (unsigned i = 0; i <= adj_list->size; i++) {
      graph_node to_index = adj_list->edges[i];

      if (hvisited(&q, to_index)) {
        continue;
      }

      unsigned dist_to_next = graph->matrix[current.node][to_index];
      if (dist_to_next == 0) {
        continue;
      }

      unsigned total_dist = current.dist + dist_to_next;

      bool recurse_at_to_index = false;
      if (node_is_door(to_index)) {
        // Can always traverse through the door
        recurse_at_to_index = true;
      } else if (node_is_key(to_index)) {
        // Keys
        uint8_t key_index = to_index - KEYS_OFFSET;
        if (state->keys_bitmask & (1 << key_index)) {
          // Already seen, so traverse through it like an empty space
          recurse_at_to_index = true;
        } else {
          if (output->size >= MAX_NODE_COUNT) {
            die("Too many accessible nodes\n");
          }

          output->data[output->size++] = (struct search_node){
              .node = to_index,
              .dist = total_dist,
          };
        }
      } else {
        // Can traverse through the gateway if we've seen the key
        uint8_t key_index = to_index - LOCKS_OFFSET;
        recurse_at_to_index = state->keys_bitmask & (1 << key_index);
      }

      if (recurse_at_to_index) {
        hinsert(&q, (struct search_node){
                        .node = to_index,
                        .dist = total_dist,
                    });
      }
    }
  }
}

#define BUCKET_SIZE 8

struct cache_entry {
  uint64_t key;
  unsigned total_dist;
};

struct state_bucket {
  uint8_t size;
  struct cache_entry cells[BUCKET_SIZE];
  struct state_bucket *next;
};

struct state_cache {
  unsigned size;
  uint64_t mask;
  struct state_bucket *buckets;
};

static struct cache_entry *bucket_lookup(struct state_bucket *b, uint64_t key) {
  for (unsigned i = 0; i < b->size; i++) {
    if (b->cells[i].key == key) {
      return &b->cells[i];
    }
  }

  if (b->next == NULL) {
    return NULL;
  } else {
    return bucket_lookup(b->next, key);
  }
}

static void bucket_insert(struct state_bucket *b, uint64_t key,
                          unsigned total_dist) {
  if (b->size < BUCKET_SIZE) {
    b->cells[b->size++] = (struct cache_entry){
        .key = key,
        .total_dist = total_dist,
    };
  } else {
    if (b->next == NULL) {
      b->next = malloc(sizeof(struct state_bucket));
      b->next->size = 0;
      b->next->next = NULL;
    }

    bucket_insert(b->next, key, total_dist);
  }
}

static uint64_t hash64(uint64_t x) {
  x = (x ^ (x >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
  x = (x ^ (x >> 27)) * UINT64_C(0x94d049bb133111eb);
  x = x ^ (x >> 31);
  return x;
}

#define INIT_CACHE_SIZE (1 << 10)

static void cache_init(struct state_cache *c) {
  uint64_t capacity = INIT_CACHE_SIZE;
  c->size = 0;
  c->mask = capacity - 1;
  c->buckets = malloc(sizeof(c->buckets[0]) * capacity);
  for (unsigned i = 0; i < capacity; i++) {
    c->buckets[i].size = 0;
    c->buckets[i].next = NULL;
  }
}

static void cache_destroy(struct state_cache *c) {
  for (unsigned i = 0; i < c->mask + 1; i++) {
    struct state_bucket *b = c->buckets[i].next;
    while (b != NULL) {
      struct state_bucket *next = b->next;
      free(b);
      b = next;
    }
  }

  free(c->buckets);
}

static bool should_visit_state(struct state_cache *c,
                               const struct search_state *state) {
  uint64_t value =
      ((uint64_t)state->current_node << 32) + (uint64_t)state->keys_bitmask;
  uint64_t hash = hash64(value);
  uint64_t index = hash & c->mask;

  struct cache_entry *existing = bucket_lookup(&c->buckets[index], value);
  if (existing == NULL) {
    // TODO Re-size as needed
    bucket_insert(&c->buckets[index], value, state->total_dist);
    c->size++;
    return true;
  } else {
    if (existing->total_dist <= state->total_dist) {
      return false;
    } else {
      existing->total_dist = state->total_dist;
      return true;
    }
  }
}

struct search_path {
  uint8_t len;
  struct search_node steps[26];
};

struct search_context {
  struct search_path current_path;
  struct search_path shortest_path;
  unsigned shortest_dist;

  struct state_cache visited_states;

  const struct graph *graph;
  const uint32_t all_keys_bitmask;
};

static unsigned cache_hits = 0;
static unsigned cache_misses = 0;

static void find_best_path(struct search_context *context,
                           const struct search_state *state) {
  if (state->keys_bitmask == context->all_keys_bitmask) {
    // Found a path
    if (state->total_dist < context->shortest_dist) {
      memcpy(&context->shortest_path, &context->current_path,
             sizeof(struct search_path));
      context->shortest_dist = state->total_dist;

      printf("Found new shortets path of distance: %u\n", state->total_dist);
      for (unsigned i = 0; i < context->current_path.len; i++) {
        printf("%c (%u), ", get_node_char(context->current_path.steps[i].node),
               context->current_path.steps[i].dist);
      }
      printf("\n");
    }

    return;
  }

  if (should_visit_state(&context->visited_states, state)) {
    cache_misses++;
  } else {
    cache_hits++;
    return;
  }

  struct node_list next_steps;
  find_accessible_keys_dfs(&next_steps, state, context->graph);

  if (next_steps.size == 0) {
    debugf("Dead end at %c (bitmask = %x)\n",
           get_node_char(state->current_node), state->keys_bitmask);
  }

  for (unsigned i = 0; i < next_steps.size; i++) {
    const struct search_node *step = &next_steps.data[i];
    int8_t key_index = get_key_index(step->node);
    assert(0 <= key_index && key_index <= MAX_KEY_COUNT);

    debugf("Step %c -> %c (dist = %d)\n", get_node_char(state->current_node),
           get_node_char(step->node), step->dist);

    context->current_path.steps[context->current_path.len++] = *step;
    find_best_path(context,
                   &(struct search_state){
                       .current_node = step->node,
                       .total_dist = state->total_dist + step->dist,
                       .keys_bitmask = state->keys_bitmask | (1 << key_index),
                   });
    context->current_path.len--;
  }
}

void part1(const struct maze *input) {
  struct graph graph;
  build_graph(&graph, input);
  print_graph(&graph);

  unsigned all_keys_bitmask = 0;
  for (unsigned i = 0; i < graph.key_count; i++) {
    all_keys_bitmask |= (1 << i);
  }

  struct search_context search_context = {
      .current_path = {.len = 0},
      .shortest_path = {.len = 0},
      .shortest_dist = (unsigned)-1,
      .graph = &graph,
      .all_keys_bitmask = all_keys_bitmask,
  };

  cache_init(&search_context.visited_states);

  find_best_path(&search_context, &(struct search_state){
                                      .current_node = 0,
                                      .total_dist = 0,
                                      .keys_bitmask = 0,
                                  });

  puts("Cache stats:");
  printf("Hits: %u\nMisses: %u\nRatio: %g\n", cache_hits, cache_misses,
         cache_hits / (float)(cache_hits + cache_misses));

  cache_destroy(&search_context.visited_states);

  puts("Shortest path:");
  for (unsigned i = 0; i < search_context.shortest_path.len; i++) {
    printf("%c (%u), ",
           get_node_char(search_context.shortest_path.steps[i].node),
           search_context.shortest_path.steps[i].dist);
  }
  putchar('\n');
  printf("Part2: %u\n", search_context.shortest_dist);
}
