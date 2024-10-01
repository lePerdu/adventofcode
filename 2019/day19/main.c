#include <assert.h>
#include <stdio.h>

#include "common.h"
#include "intcode.h"

#define GRID_SIZE 50

struct coord {
  unsigned row;
  unsigned col;
};

enum pull_state {
  STATIONARY = 0,
  PULLED = 1,
};

enum pull_state check_pos(const struct intcode_program *input,
                          struct coord pos) {
  struct machine m;
  machine_init(&m, input);

  machine_input(&m, pos.col);
  machine_run_til_wait(&m);
  machine_input(&m, pos.row);
  machine_run_til_wait(&m);

  intcode result = machine_output(&m);
  switch (result) {
  case STATIONARY:
  case PULLED:
    return result;
  default:
    die("Invalid output: %ld\n", result);
  }
}

void part1(const struct intcode_program *input) {
  unsigned count = 0;
  for (unsigned row = 0; row < GRID_SIZE; row++) {
    for (unsigned col = 0; col < GRID_SIZE; col++) {
      if (check_pos(input, (struct coord){row, col}) == PULLED) {
        count++;
        putchar('#');
      } else {
        putchar('.');
      }
    }
    putchar('\n');
  }
  putchar('\n');

  printf("Part1: %u\n", count);
}

bool square_contains(struct coord bl, struct coord tr, struct coord c) {
  return (tr.row <= c.row && c.row <= bl.row && bl.col <= c.col &&
          c.col <= tr.col);
}

bool is_bl_edge(const struct intcode_program *input, struct coord bl) {
  return (check_pos(input, bl) == PULLED &&
          check_pos(input, (struct coord){bl.row, bl.col - 1}) == STATIONARY &&
          check_pos(input, (struct coord){bl.row + 1, bl.col}) == STATIONARY);
}

struct coord find_prev_bl_edge(const struct intcode_program *input,
                               struct coord bl) {
  if (!is_bl_edge(input, bl)) {
    bl.col--;
    while (check_pos(input, bl) == STATIONARY) {
      bl.row--;
    }
  }
  assert(is_bl_edge(input, bl));
  return bl;
}

struct coord find_next_bl_edge(const struct intcode_program *input,
                               struct coord bl) {
  while (check_pos(input, (struct coord){bl.row + 1, bl.col}) == PULLED) {
    bl.row++;
  }
  assert(is_bl_edge(input, bl));
  return bl;
}

struct coord bl_to_tr(struct coord bl_corner, unsigned ship_size) {
  return (struct coord){
      .row = bl_corner.row - ship_size + 1,
      .col = bl_corner.col + ship_size - 1,
  };
}

unsigned part2_bin_search(const struct intcode_program *input,
                          unsigned ship_size) {
  struct coord upper_bound_bl = {.row = ship_size - 1, .col = 0};

  // Exponential search to find an upper bound
  {
    while (true) {
      while (check_pos(input, upper_bound_bl) == STATIONARY) {
        upper_bound_bl.col++;
      }

      debugf("At row %u; first col: %u\n", upper_bound_bl.row,
             upper_bound_bl.col);

      // See if the width is at least SHIP_SIZE
      if (check_pos(input, bl_to_tr(upper_bound_bl, ship_size)) == PULLED) {
        break;
      }

      upper_bound_bl.row *= 2;
      // Under estimate because of rounding errors
      upper_bound_bl.col = (upper_bound_bl.col - 1) * 2;
    }
  }

  upper_bound_bl = find_next_bl_edge(input, upper_bound_bl);

  debugf("Upper row bound: (%u, %u)\n", upper_bound_bl.row, upper_bound_bl.col);

  // Binary search to find the lower bound
  struct coord lower_bound_bl = {.row = 0, .col = 0};
  {
    while (lower_bound_bl.row < upper_bound_bl.row) {
      debugf("(%u, %u) -> (%u, %u)\n", lower_bound_bl.row, lower_bound_bl.col,
             upper_bound_bl.row, upper_bound_bl.col);
      struct coord mid = {
          .row = (lower_bound_bl.row + upper_bound_bl.row) / 2,
          // Under estmate col
          .col = (lower_bound_bl.col + upper_bound_bl.col - 1) / 2,
      };

      while (check_pos(input, mid) == STATIONARY) {
        mid.col++;
      }
      mid = find_prev_bl_edge(input, mid);

      if (check_pos(input, bl_to_tr(mid, ship_size)) == PULLED) {
        debugf("Fits at: (%u, %u)\n", mid.row, mid.col);
        upper_bound_bl = mid;
      } else {
        debugf("Does not fit at: (%u, %u)\n", mid.row, mid.col);
        lower_bound_bl = mid;
        lower_bound_bl.row++;
        while (check_pos(input, lower_bound_bl) == STATIONARY) {
          lower_bound_bl.col++;
        }
        lower_bound_bl = find_next_bl_edge(input, lower_bound_bl);
      }
    }

    if (lower_bound_bl.row != upper_bound_bl.row ||
        lower_bound_bl.col != upper_bound_bl.col) {
      die("Coordinates not an exact match\n");
    }
  }

  debugf("Lower row bound: (%u, %u)\n", lower_bound_bl.row, lower_bound_bl.col);

  struct coord bl_corner = lower_bound_bl;

  /*
  struct coord tr_corner = bl_to_tr(bl_corner);
  for (unsigned row = tr_corner.row - 5; row < bl_corner.row + 5; row++) {
    for (unsigned col = bl_corner.col - 5; col < tr_corner.col + 5; col++) {
      struct coord c = {row, col};
      bool in_ship = square_contains(bl_corner, tr_corner, c);
      bool in_beam = check_pos(input, c) == PULLED;

      if (in_ship && !in_beam) {
        die("\nCoord (%u, %u) in ship but not in beam\n", c.row, c.col);
      } else if (in_ship) {
        putchar('O');
      } else if (in_beam) {
        putchar('#');
      } else {
        putchar('.');
      }
    }
    putchar('\n');
  }
  putchar('\n');
  */

  struct coord tl_corner = {
      .row = bl_corner.row - ship_size + 1,
      .col = bl_corner.col,
  };

  return tl_corner.col * 10000 + tl_corner.row;
}

unsigned part2_brute_force(const struct intcode_program *input,
                           unsigned ship_size) {
  // Find first edge
  struct coord bl_corner = {ship_size - 1, 0};
  while (check_pos(input, bl_corner) == STATIONARY) {
    bl_corner.col++;
  }

  while (check_pos(input, bl_to_tr(bl_corner, ship_size)) == STATIONARY) {
    bl_corner.row++;
    while (check_pos(input, bl_corner) == STATIONARY) {
      bl_corner.col++;
    }
  }

  struct coord tl_corner = {
      .row = bl_corner.row - ship_size + 1,
      .col = bl_corner.col,
  };

  return tl_corner.col * 10000 + tl_corner.row;
}

#define SHIP_SIZE 100

void part2(const struct intcode_program *input) {
  for (unsigned size = 10; size <= SHIP_SIZE; size++) {
    unsigned bin = part2_bin_search(input, size);
    unsigned brute = part2_brute_force(input, size);

    if (bin == brute) {
      printf("Match for size %u: %u\n", size, bin);
    } else {
      printf("Mismatch for size %u: %u != %u\n", size, bin, brute);
    }
  }

  printf("Part2: %u\n", part2_brute_force(input, SHIP_SIZE));
}

int main(void) {
  struct intcode_program input;
  read_input(&input);
  part1(&input);
  part2(&input);
  return 0;
}
