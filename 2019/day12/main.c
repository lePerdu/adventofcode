#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"

struct vec {
  int x, y, z;
};

#define MOON_COUNT 4

struct input {
  struct vec moon_pos[MOON_COUNT];
};

void read_vec(struct vec *v) {
  if (scanf("<x=%d, y=%d, z=%d>\n", &v->x, &v->y, &v->z) != 3) {
    die("Failed to parse input\n");
  }
}

void read_input(struct input *input) {
  for (unsigned i = 0; i < MOON_COUNT; i++) {
    read_vec(&input->moon_pos[i]);
  }
}

struct moon {
  struct vec pos;
  struct vec vel;
};

struct state {
  struct moon moons[MOON_COUNT];
};

void vec_add(struct vec *a, const struct vec *b) {
  a->x += b->x;
  a->y += b->y;
  a->z += b->z;
}

unsigned vec_abs(const struct vec *a) {
  return abs(a->x) + abs(a->y) + abs(a->z);
}

void state_init(struct state *s, const struct input *input) {
  for (unsigned i = 0; i < MOON_COUNT; i++) {
    s->moons[i].pos = input->moon_pos[i];
    s->moons[i].vel = (struct vec){.x = 0, .y = 0, .z = 0};
  }
}

void state_print(const struct state *s) {
  for (unsigned i = 0; i < MOON_COUNT; i++) {
    const struct vec *pos = &s->moons[i].pos;
    const struct vec *vel = &s->moons[i].vel;
    printf("pos=<x=%d, y=%d, z=%d>, vel=<x=%d, y=%d, z=%d>\n", pos->x, pos->y,
           pos->z, vel->x, vel->y, vel->z);
  }
}

void apply_gravity_on_axis(int pos_a, int *vel_a, int pos_b, int *vel_b) {
  int delta_a;
  if (pos_a < pos_b) {
    delta_a = 1;
  } else if (pos_a > pos_b) {
    delta_a = -1;
  } else {
    delta_a = 0;
  }

  *vel_a += delta_a;
  *vel_b -= delta_a;
}

void apply_gravity(struct moon *a, struct moon *b) {
  apply_gravity_on_axis(a->pos.x, &a->vel.x, b->pos.x, &b->vel.x);
  apply_gravity_on_axis(a->pos.y, &a->vel.y, b->pos.y, &b->vel.y);
  apply_gravity_on_axis(a->pos.z, &a->vel.z, b->pos.z, &b->vel.z);
}

void state_step(struct state *s) {
  // Apply gravity to every pair of moons
  for (unsigned i = 0; i < MOON_COUNT - 1; i++) {
    struct moon *moon_a = &s->moons[i];
    for (unsigned j = i + 1; j < MOON_COUNT; j++) {
      struct moon *moon_b = &s->moons[j];
      apply_gravity(moon_a, moon_b);
    }
  }

  // Apply velocity
  for (unsigned i = 0; i < MOON_COUNT; i++) {
    vec_add(&s->moons[i].pos, &s->moons[i].vel);
  }
}

unsigned calc_engery(const struct state *s) {
  unsigned total = 0;
  for (unsigned i = 0; i < MOON_COUNT; i++) {
    unsigned potential = vec_abs(&s->moons[i].pos);
    unsigned kinetic = vec_abs(&s->moons[i].vel);
    unsigned moon_energy = potential * kinetic;
    printf("pot: %d, kin: %d, total: %d\n", potential, kinetic, moon_energy);
    total += moon_energy;
  }
  return total;
}

#define PART1_STEP_COUNT 1000

void part1(const struct input *input) {
  struct state s;
  state_init(&s, input);
  for (unsigned step = 1; step <= PART1_STEP_COUNT; step++) {
    state_step(&s);
  }

  unsigned ans = calc_engery(&s);
  printf("Part1: %u\n", ans);
}

struct axis_state {
  int pos[MOON_COUNT];
  int vel[MOON_COUNT];
};

bool axis_state_eq(const struct axis_state *s1, const struct axis_state *s2) {
  return memcmp(s1, s2, sizeof(struct axis_state)) == 0;
}

void axis_state_step(struct axis_state *s) {
  // Apply gravity to every pair of moons
  for (unsigned i = 0; i < MOON_COUNT - 1; i++) {
    for (unsigned j = i + 1; j < MOON_COUNT; j++) {
      apply_gravity_on_axis(s->pos[i], &s->vel[i], s->pos[j], &s->vel[j]);
    }
  }

  // Apply velocity
  for (unsigned i = 0; i < MOON_COUNT; i++) {
    s->pos[i] += s->vel[i];
  }
}

unsigned long find_axis_period(const struct axis_state *initial) {
  struct axis_state s;
  memcpy(&s, initial, sizeof(s));

  unsigned long step_count = 0;
  do {
    axis_state_step(&s);
    step_count++;
  } while (!axis_state_eq(&s, initial));

  return step_count;
}

unsigned long gcd(unsigned long a, unsigned long b) {
  while (b != 0) {
    unsigned long tmp = a % b;
    a = b;
    b = tmp;
  }
  return a;
}

unsigned long lcm(unsigned long a, unsigned long b) {
  return a / gcd(a, b) * b;
}

void part2(const struct input *input) {
  struct axis_state s;
  for (unsigned i = 0; i < MOON_COUNT; i++) {
    s.pos[i] = input->moon_pos[i].x;
    s.vel[i] = 0;
  }
  unsigned long x_period = find_axis_period(&s);
  printf("T(x) = %lu\n", x_period);

  for (unsigned i = 0; i < MOON_COUNT; i++) {
    s.pos[i] = input->moon_pos[i].y;
    s.vel[i] = 0;
  }
  unsigned long y_period = find_axis_period(&s);
  printf("T(y) = %lu\n", y_period);

  for (unsigned i = 0; i < MOON_COUNT; i++) {
    s.pos[i] = input->moon_pos[i].z;
    s.vel[i] = 0;
  }
  unsigned long z_period = find_axis_period(&s);
  printf("T(z) = %lu\n", z_period);

  unsigned long total_period = lcm(lcm(x_period, y_period), z_period);
  printf("Part2: %lu\n", total_period);
}

int main(void) {
  struct input input;
  read_input(&input);
  part1(&input);
  part2(&input);
  return 0;
}
