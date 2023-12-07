#include <ctype.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

enum direction {
  DIRR,
  DIRU,
  DIRL,
  DIRD,
};

struct move {
  enum direction dir;
  unsigned dist;
};

#define MAX_MOVES 512

struct move_list {
  size_t size;
  struct move moves[MAX_MOVES];
};

struct input {
  struct move_list wire1;
  struct move_list wire2;
};

enum parse_result {
  PR_SUCCESS = 0,
  PR_EXPECT_DIR,
  PR_EXPECT_NUMBER,
  PR_EXPECT_COMMA,
  PR_EXPECT_2_LINES,
  PR_TOO_MANY_MOVES,
  PR_EXPECT_EOF,
};

enum parse_result parse_dir(const char **cursor, enum direction *d) {
  switch (**cursor) {
  case 'R':
    *d = DIRR;
    break;
  case 'U':
    *d = DIRU;
    break;
  case 'L':
    *d = DIRL;
    break;
  case 'D':
    *d = DIRD;
    break;
  default:
    return PR_EXPECT_DIR;
  }

  (*cursor)++;
  return PR_SUCCESS;
}

enum parse_result parse_int(const char **cursor, unsigned *n) {
  char *endptr;
  *n = strtoul(*cursor, &endptr, 10);
  if (endptr == *cursor) {
    return PR_EXPECT_NUMBER;
  }

  *cursor = endptr;

  return PR_SUCCESS;
}

enum parse_result parse_move(const char **cursor, struct move *m) {
  enum parse_result res;
  if ((res = parse_dir(cursor, &m->dir)) != PR_SUCCESS) {
    return res;
  }

  if ((res = parse_int(cursor, &m->dist)) != PR_SUCCESS) {
    return res;
  }

  return PR_SUCCESS;
}

enum parse_result expect_comma(const char **cursor) {
  if (**cursor == ',') {
    (*cursor)++;
    return PR_SUCCESS;
  } else {
    return PR_EXPECT_COMMA;
  }
}

enum parse_result expect_eol(const char **cursor) {
  while (isspace(**cursor)) {
    (*cursor)++;
  }

  if (**cursor == 0) {
    *cursor += 2;
    return PR_SUCCESS;
  } else {
    return PR_EXPECT_COMMA;
  }
}

enum parse_result parse_move_seq(const char **cursor, struct move_list *moves) {
  enum parse_result res;
  if ((res = parse_move(cursor, &moves->moves[0])) != PR_SUCCESS) {
    return res;
  }
  moves->size = 1;

  while (expect_comma(cursor) == PR_SUCCESS) {
    if (moves->size >= MAX_MOVES) {
      return PR_TOO_MANY_MOVES;
    }

    if ((res = parse_move(cursor, &moves->moves[moves->size])) != PR_SUCCESS) {
      return res;
    }
    moves->size++;
  }

  return expect_eol(cursor);
}

void read_input_line(char **buf, struct move_list *moves) {
  size_t buf_size;
  if (getline(buf, &buf_size, stdin) < 0) {
    perror("Failed to read input");
    exit(EXIT_FAILURE);
  }

  const char *cbuf = *buf;
  enum parse_result res;
  if ((res = parse_move_seq(&cbuf, moves)) != PR_SUCCESS) {
    fprintf(stderr, "Failed to parse input: %d\n", res);
    exit(EXIT_FAILURE);
  }
}

void read_input(struct input *input) {
  // Share buffer betwen both getline calls
  char *buf = NULL;

  read_input_line(&buf, &input->wire1);
  read_input_line(&buf, &input->wire2);

  free(buf);
}

struct point {
  int x;
  int y;
};

struct line_seg {
  int x1;
  int y1;
  int x2;
  int y2;
};

struct line_list {
  size_t size;
  // Note: This max size must be the same as move_list
  struct line_seg lines[MAX_MOVES];
};

#ifdef DEBUG
#define debugf printf

int debug_line(const char *label, const struct line_seg *line) {
  return printf("%s: (%d, %d) -> (%d, %d)\n", label, line->x1, line->x2,
                line->y1, line->y2);
}

#else
int noop(const char *s, ...) {
  (void)s;
  return 0;
}
#define debugf noop
#define debug_line noop
#endif

void moves_to_lines(const struct move_list *moves, struct line_list *lines) {
  int x1 = 0;
  int y1 = 0;

  lines->size = moves->size;
  for (size_t i = 0; i < moves->size; i++) {
    const struct move *m = &moves->moves[i];
    int x2 = x1;
    int y2 = y1;
    switch (m->dir) {
    case DIRR:
      x2 += m->dist;
      break;
    case DIRU:
      y2 += m->dist;
      break;
    case DIRL:
      x2 -= m->dist;
      break;
    case DIRD:
      y2 -= m->dist;
      break;
    }

    lines->lines[i] = (struct line_seg){.x1 = x1, .y1 = y1, .x2 = x2, .y2 = y2};
    debug_line("Found line", &lines->lines[i]);
    x1 = x2;
    y1 = y2;
  }
}

bool range_contains(int a, int b, int value) {
  int min;
  int max;
  if (a <= b) {
    min = a;
    max = b;
  } else {
    min = b;
    max = a;
  }

  return min <= value && value <= max;
}

bool find_vh_intersect(const struct line_seg *vert_line,
                       const struct line_seg *horiz_line,
                       struct point *intersection) {
  bool vert_in_x =
      range_contains(horiz_line->x1, horiz_line->x2, vert_line->x1);
  bool horiz_in_y =
      range_contains(vert_line->y1, vert_line->y2, horiz_line->y1);

  debug_line("V", vert_line);
  debug_line("H", horiz_line);
  if (vert_in_x && horiz_in_y) {
    intersection->x = vert_line->x1;
    intersection->y = horiz_line->y1;
    return true;
  } else {
    return false;
  }
}

bool find_intersection(const struct line_seg *l1, const struct line_seg *l2,
                       struct point *intersection) {
  bool l1_horiz = l1->y1 == l1->y2;
  bool l2_horiz = l2->y1 == l2->y2;
  if (l1_horiz == l2_horiz) {
    return false;
  } else if (!l1_horiz && l2_horiz) {
    // Vert + horiz
    return find_vh_intersect(l1, l2, intersection);
  } else /* if (l1_horiz && !l2_horiz) */ {
    // Horiz + vert
    return find_vh_intersect(l2, l1, intersection);
  }
}

unsigned find_closest_intersect(const struct line_list *wire1,
                                const struct line_list *wire2) {
  unsigned closest_dist = -1;
  for (size_t l1 = 0; l1 < wire1->size; l1++) {
    for (size_t l2 = 0; l2 < wire2->size; l2++) {
      struct point p;
      if (find_intersection(&wire1->lines[l1], &wire2->lines[l2], &p)) {
        unsigned point_dist = abs(p.x) + abs(p.y);
        if (0 < point_dist && point_dist < closest_dist) {
          closest_dist = point_dist;
        }
      }
    }
  }

  return closest_dist;
}

void part1(const struct input *input) {
  struct line_list wire1_lines;
  moves_to_lines(&input->wire1, &wire1_lines);
  struct line_list wire2_lines;
  moves_to_lines(&input->wire2, &wire2_lines);
  unsigned ans = find_closest_intersect(&wire1_lines, &wire2_lines);
  printf("Part1: %u\n", ans);
}

unsigned find_quickest_intersect(const struct move_list *wire1_moves,
                                 const struct line_list *wire1_lines,
                                 const struct move_list *wire2_moves,
                                 const struct line_list *wire2_lines) {
  unsigned lowest_steps = -1;

  unsigned wire1_steps = 0;
  for (size_t i1 = 0; i1 < wire1_moves->size; i1++) {
    const struct line_seg *l1 = &wire1_lines->lines[i1];

    unsigned wire2_steps = 0;
    for (size_t i2 = 0; i2 < wire2_moves->size; i2++) {
      const struct line_seg *l2 = &wire2_lines->lines[i2];

      struct point p;
      if (find_intersection(&wire1_lines->lines[i1], &wire2_lines->lines[i2],
                            &p)) {
        unsigned extra_wire1 = abs(p.x - l1->x1) + abs(p.y - l1->y1);
        unsigned extra_wire2 = abs(p.x - l2->x1) + abs(p.y - l2->y1);
        unsigned total_steps =
            wire1_steps + extra_wire1 + wire2_steps + extra_wire2;

        if (0 < total_steps && total_steps < lowest_steps) {
          lowest_steps = total_steps;
        }
      }

      wire2_steps += wire2_moves->moves[i2].dist;
    }

    wire1_steps += wire1_moves->moves[i1].dist;
  }

  return lowest_steps;
}

void part2(const struct input *input) {
  struct line_list wire1_lines;
  moves_to_lines(&input->wire1, &wire1_lines);
  struct line_list wire2_lines;
  moves_to_lines(&input->wire2, &wire2_lines);
  unsigned ans = find_quickest_intersect(&input->wire1, &wire1_lines,
                                         &input->wire2, &wire2_lines);
  printf("Part2: %u\n", ans);
}

int main(void) {
  struct input input;
  read_input(&input);
  part1(&input);
  part2(&input);
  return 0;
}
