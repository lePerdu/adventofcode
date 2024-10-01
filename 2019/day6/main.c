#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define OBJECT_NAME "%3[0-9A-Za-z]"

typedef uint32_t object_id;

struct orbit {
  object_id stationary;
  object_id orbiting;
};

#define MAX_ORBITS 2048

struct orbit_list {
  size_t size;
  struct orbit data[MAX_ORBITS];
};

object_id gen_object_id(const char object_name[4]) {
  static_assert(sizeof(object_name[0]) * 4 == sizeof(object_id),
                "Cannot correctly convert object name to ID");
  return *(object_id *)object_name;
}

int read_orbit(struct orbit_list *input) {
  // Initialize to 0 so that we can cast to an int pointer "hash" of the
  // sequence
  char stationary[4] = {};
  char orbiting[4] = {};

  errno = 0;
  int n_matched = scanf(OBJECT_NAME ")" OBJECT_NAME "\n", stationary, orbiting);
  if (n_matched == EOF) {
    return EOF;
  }

  if (n_matched != 2) {
    if (errno != 0) {
      perror("Failed to read input");
    } else {
      fprintf(stderr, "Failed to parse input\n");
    }
    exit(EXIT_FAILURE);
  }

  if (input->size >= MAX_ORBITS) {
    fprintf(stderr, "Too many orbits\n");
    exit(EXIT_FAILURE);
  }

  input->data[input->size++] = (struct orbit){
      .stationary = gen_object_id(stationary),
      .orbiting = gen_object_id(orbiting),
  };
  return 0;
}

void read_input(struct orbit_list *input) {
  while (read_orbit(input) == 0) {
  }
}

int compare_by_stationary(const void *a, const void *b) {
  const struct orbit *orb_a = a;
  const struct orbit *orb_b = b;
  return orb_a->stationary - orb_b->stationary;
}

struct object_data {
  object_id object;
  object_id parent;
  unsigned depth;
};

struct orbit_graph {
  size_t orbit_count;
  struct orbit orbits[MAX_ORBITS];
  size_t object_count;
  struct object_data objects[MAX_ORBITS];
};

struct orbit_slice {
  size_t size;
  struct orbit *data;
};

struct object_data *find_object(const struct orbit_graph *graph,
                                object_id object) {
  struct object_data key = {.object = object};
  return bsearch(&key, graph->objects, graph->object_count,
                 sizeof(graph->objects[0]), compare_by_stationary);
}

size_t find_object_index(const struct orbit_graph *graph, object_id object) {
  const struct object_data *obj = find_object(graph, object);
  if (obj == NULL) {
    fprintf(stderr, "Cannot find object: %u\n", object);
    exit(EXIT_FAILURE);
  } else {
    return obj - graph->objects;
  }
}

struct orbit_slice find_orbits_around(const struct orbit_graph *graph,
                                      object_id stationary_id) {
  struct orbit key = {.stationary = stationary_id};
  struct orbit *found =
      bsearch(&key, graph->orbits, graph->orbit_count, sizeof(graph->orbits[0]),
              compare_by_stationary);
  if (found == NULL) {
    return (struct orbit_slice){
        .size = 0,
        .data = NULL,
    };
  }

  // Expand left and right of the found item to get the full slice of matching
  // orbits
  const struct orbit *start = &graph->orbits[0];
  const struct orbit *end = &graph->orbits[graph->orbit_count];

  struct orbit *left = found;
  while (left >= start && left->stationary == stationary_id) {
    left--;
  }
  left++;

  struct orbit *right = found;
  while (right < end && right->stationary == stationary_id) {
    right++;
  }

  return (struct orbit_slice){
      .size = right - left,
      .data = left,
  };
}

void build_orbit_graph(struct orbit_graph *graph,
                       const struct orbit_list *list) {
  graph->orbit_count = list->size;
  for (size_t i = 0; i < list->size; i++) {
    graph->orbits[i] = (struct orbit){
        .stationary = list->data[i].stationary,
        .orbiting = list->data[i].orbiting,
    };
  }
  graph->object_count = 0;

  qsort(graph->orbits, graph->orbit_count, sizeof(graph->orbits[0]),
        // This technically takes struct orbit, not struct orbit, but it
        // works for both
        compare_by_stationary);

  struct object_data queue[MAX_ORBITS];
  queue[0] = (struct object_data){
      .object = gen_object_id("COM\0"), .parent = 0, .depth = 0};
  size_t queue_head = 0;
  size_t queue_tail = 1;

  while (queue_head != queue_tail) {
    struct object_data next = queue[queue_head++];

    graph->objects[graph->object_count++] = next;

    struct orbit_slice orbits_around_next =
        find_orbits_around(graph, next.object);
    for (size_t i = 0; i < orbits_around_next.size; i++) {
      if (queue_tail >= MAX_ORBITS) {
        fprintf(stderr, "Too many orbits in queue\n");
        exit(EXIT_FAILURE);
      }

      queue[queue_tail++] =
          (struct object_data){.object = orbits_around_next.data[i].orbiting,
                               .parent = next.object,
                               .depth = next.depth + 1};
    }
  }

  qsort(graph->objects, graph->object_count, sizeof(graph->objects[0]),
        // This technically takes struct orbit, not struct orbit_parent, but it
        // works for both
        compare_by_stationary);
}

void part1(const struct orbit_graph *graph) {
  unsigned total_orbits = 0;
  for (size_t i = 0; i < graph->object_count; i++) {
    total_orbits += graph->objects[i].depth;
  }

  printf("Part1: %u\n", total_orbits);
}

struct queue {
  size_t head;
  size_t tail;
  struct object_data data[MAX_ORBITS];
  bool visited_objects[MAX_ORBITS];
};

void qinit(struct queue *q) {
  q->head = 0;
  q->tail = 0;
  memset(q->visited_objects, false, MAX_ORBITS);
}

void qinsert(struct queue *q, const struct orbit_graph *graph,
             object_id current_node, object_id next_node, unsigned next_depth) {
  size_t next_index = find_object_index(graph, next_node);
  if (q->visited_objects[next_index]) {
    return;
  }
  q->visited_objects[next_index] = true;

  q->data[q->tail] = (struct object_data){
      .object = next_node, .parent = current_node, .depth = next_depth};
  q->tail = q->tail < MAX_ORBITS - 1 ? q->tail + 1 : 0;
  if (q->tail == q->head) {
    fprintf(stderr, "Queue full\n");
    exit(EXIT_FAILURE);
  }
}

struct object_data qremove(struct queue *q) {
  if (q->head == q->tail) {
    fprintf(stderr, "Queue empty\n");
    exit(EXIT_FAILURE);
  }

  struct object_data result = q->data[q->head];
  q->head = q->head < MAX_ORBITS - 1 ? q->head + 1 : 0;
  return result;
}

bool qempty(const struct queue *q) { return q->head == q->tail; }

void part2(const struct orbit_graph *graph) {
  object_id you_id = gen_object_id("YOU\0");
  object_id santa_id = gen_object_id("SAN\0");

  size_t you_index = find_object_index(graph, you_id);
  object_id start_object = graph->objects[you_index].parent;
  object_id target_object = find_object(graph, santa_id)->parent;

  struct queue q;
  qinit(&q);
  qinsert(&q, graph, you_id, start_object, 0);

  unsigned shortest_path = 0;
  while (!qempty(&q)) {
    struct object_data current = qremove(&q);

    if (current.object == target_object) {
      shortest_path = current.depth;
      break;
    }

    struct object_data *obj_data = find_object(graph, current.object);
    unsigned next_depth = current.depth + 1;
    if (obj_data->parent != 0) {
      qinsert(&q, graph, current.object, obj_data->parent, next_depth);
    }

    const struct orbit_slice orbiting =
        find_orbits_around(graph, current.object);
    for (size_t i = 0; i < orbiting.size; i++) {
      qinsert(&q, graph, current.object, orbiting.data[i].orbiting, next_depth);
    }
  }

  printf("Part2: %u\n", shortest_path);
}

int main(void) {
  static struct orbit_list input;
  read_input(&input);
  static struct orbit_graph graph;
  build_orbit_graph(&graph, &input);
  part1(&graph);
  part2(&graph);
  return 0;
}
