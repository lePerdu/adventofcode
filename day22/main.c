#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"

#define SPACE_CARD_COUNT 10007

enum shuffle_type {
  SHUF_NEW_STACK,
  SHUF_CUT,
  SHUF_INCREMENT,
};

struct shuffle {
  enum shuffle_type type;
  int arg;
};

#define MAX_SHUFFLES 100

struct shuffle_list {
  size_t size;
  struct shuffle data[MAX_SHUFFLES];
};

void skip_white(const char **cursor) {
  while (isspace(**cursor)) {
    (*cursor)++;
  }
}

bool expect_str(const char **cursor, const char *s) {
  const char *matching = *cursor;
  while (*s != 0) {
    if (*matching == *s) {
      matching++;
      s++;
    } else {
      return false;
    }
  }
  *cursor = matching;
  return true;
}

bool parse_num(const char **cursor, int *n) {
  char *endptr;
  long n_long = strtol(*cursor, &endptr, 10);
  if (endptr == *cursor) {
    return false;
  }

  if (n_long < (long)INT_MIN || (long)INT_MAX < n_long) {
    die("Number out of range: %ld\n", n_long);
  }
  if (labs(n_long) > SPACE_CARD_COUNT) {
    die("Number too big for SPACE_CARD_COUNT: %ld\n", n_long);
  }

  *n = n_long;
  *cursor = endptr;
  return true;
}

bool is_eol(const char *line) {
  skip_white(&line);
  return *line == 0;
}

bool read_shuffle(struct shuffle_list *input, char **line_buf,
                  size_t *line_buf_size) {
  errno = 0;
  ssize_t line_len = getline(line_buf, line_buf_size, stdin);
  if (line_len < 0) {
    if (errno != 0) {
      perror_die("Failed to read input");
    } else {
      // EOF
      return false;
    }
  }

  const char *cursor = *line_buf;
  if (is_eol(cursor)) {
    return false;
  }

  if (input->size >= MAX_SHUFFLES) {
    die("Too many shuffles\n");
  }

  struct shuffle *shuf = &input->data[input->size];
  if (expect_str(&cursor, "deal into new stack")) {
    shuf->type = SHUF_NEW_STACK;
  } else if (expect_str(&cursor, "cut")) {
    shuf->type = SHUF_CUT;
    if (!parse_num(&cursor, &shuf->arg)) {
      die("Expected argument for CUT\n");
    }
  } else if (expect_str(&cursor, "deal with increment")) {
    shuf->type = SHUF_INCREMENT;
    if (!parse_num(&cursor, &shuf->arg)) {
      die("Expected argument for INCREMENT\n");
    }
  } else {
    die("Invalid shuffle: %s\n", *line_buf);
  }

  if (!is_eol(cursor)) {
    die("Expected EOL. Got: %s\n", cursor);
  }

  input->size++;

  return true;
}

void read_input(struct shuffle_list *input) {
  input->size = 0;

  char *line_buf = NULL;
  size_t line_buf_size = 0;
  while (read_shuffle(input, &line_buf, &line_buf_size)) {
  }
}

struct deck {
  unsigned cards[SPACE_CARD_COUNT];
};

void init_space_deck(struct deck *d) {
  for (unsigned i = 0; i < SPACE_CARD_COUNT; i++) {
    d->cards[i] = i;
  }
}

void deal_new_stack(const struct deck *input, struct deck *output) {
  for (unsigned i = 0; i < SPACE_CARD_COUNT; i++) {
    output->cards[SPACE_CARD_COUNT - 1 - i] = input->cards[i];
  }
}

void cut_deck(const struct deck *input, struct deck *output, int n) {
  for (unsigned i = 0; i < SPACE_CARD_COUNT; i++) {
    unsigned j = (SPACE_CARD_COUNT + i - n) % SPACE_CARD_COUNT;
    output->cards[j] = input->cards[i];
  }
}

void deal_with_increment(const struct deck *input, struct deck *output, int n) {
  for (unsigned i = 0, j = 0; i < SPACE_CARD_COUNT;
       i++, j = (j + n) % SPACE_CARD_COUNT) {
    output->cards[j] = input->cards[i];
  }
}

void print_deck(const char *label, const struct deck *d) {
  printf("%s:", label);
  for (unsigned i = 0; i < SPACE_CARD_COUNT; i++) {
    printf(" %u", d->cards[i]);
  }
  putchar('\n');
}

#define PART1_CARD 2019

/**
 * Find card position using array manipulation.
 */
void part1_array(const struct shuffle_list *input) {
  // Double buffer decks
  struct deck d1;
  struct deck d2;

  struct deck *current = &d1;
  struct deck *next = &d2;

  init_space_deck(current);

  for (unsigned i = 0; i < input->size; i++) {
    const struct shuffle *shuf = &input->data[i];

    switch (shuf->type) {
    case SHUF_NEW_STACK:
      deal_new_stack(current, next);
      break;
    case SHUF_CUT:
      cut_deck(current, next, shuf->arg);
      break;
    case SHUF_INCREMENT:
      deal_with_increment(current, next, shuf->arg);
      break;
    }

    // Swap decks
    struct deck *tmp = current;
    current = next;
    next = tmp;
  }

  int ans = -1;
  for (unsigned i = 0; i < SPACE_CARD_COUNT; i++) {
    if (current->cards[i] == PART1_CARD) {
      ans = i;
      break;
    }
  }

  // print_deck("Result", current);
  printf("Part1(array): %d\n", ans);
}

/**
 * Find card position using modular arithmetic with card indices.
 */
void part1_mod(const struct shuffle_list *input) {
  unsigned position = PART1_CARD;

  for (unsigned i = 0; i < input->size; i++) {
    const struct shuffle *shuf = &input->data[i];

    switch (shuf->type) {
    case SHUF_NEW_STACK:
      position = SPACE_CARD_COUNT - 1 - position;
      break;
    case SHUF_CUT:
      position = (SPACE_CARD_COUNT + position - shuf->arg) % SPACE_CARD_COUNT;
      break;
    case SHUF_INCREMENT:
      position = (position * shuf->arg) % SPACE_CARD_COUNT;
      break;
    }
  }

  printf("Part1(modular): %d\n", position);
}

void part1(const struct shuffle_list *input) {
  part1_array(input);
  part1_mod(input);
}

// #define PART2_CARD_COUNT 11UL
// #define PART2_REPEAT_COUNT 5UL
// #define PART2_CARD_INDEX 3UL
#define PART2_CARD_COUNT 119315717514047UL
#define PART2_REPEAT_COUNT 101741582076661UL
#define PART2_CARD_INDEX 2020UL

/**
 * Represents an expression of the form `e(i) = i*mul + add`.
 */
struct expr {
  unsigned long mul;
  unsigned long add;
};

struct expr shuf_to_expr(const struct shuffle *s) {
  const unsigned long minus1 = PART2_CARD_COUNT - 1;
  switch (s->type) {
  case SHUF_NEW_STACK:
    return (struct expr){
        .mul = minus1,
        .add = minus1,
    };
  case SHUF_CUT:
    return (struct expr){
        .mul = 1,
        .add = (PART2_CARD_COUNT - s->arg) % PART2_CARD_COUNT,
    };
  case SHUF_INCREMENT:
    return (struct expr){
        .mul = s->arg,
        .add = 0,
    };
  }
}

unsigned long mul_mod(unsigned long a, unsigned long b, unsigned long modulus) {
  // Multiple by halving to avoid overflow
  if (b == 0 || a == 0) {
    return 0;
  }
  if (a == 1) {
    return b;
  }

  unsigned long acc = 0;
  while (b > 1) {
    if (b % 2 == 1) {
      acc += a;
    }
    a = (2 * a) % modulus;
    b /= 2;
  }

  return (a + acc) % modulus;
}

struct expr expr_compose(struct expr e1, struct expr e2) {
  // TODO Does multiplication here overflow?
  return (struct expr){
      .mul = mul_mod(e1.mul, e2.mul, PART2_CARD_COUNT),
      .add = (e1.add + mul_mod(e1.mul, e2.add, PART2_CARD_COUNT)) %
             PART2_CARD_COUNT,
  };
}

unsigned long expr_apply(struct expr e, unsigned long v) {
  return (mul_mod(e.mul, v, PART2_CARD_COUNT) + e.add) % PART2_CARD_COUNT;
}

struct expr expr_pow(struct expr e, unsigned long n) {
  struct expr acc = {.mul = 1, .add = 0};
  if (n == 0) {
    return acc;
  }

  // Order doesn't matter here since it's associative
  while (n > 1) {
    if (n % 2 == 1) {
      acc = expr_compose(e, acc);
    }
    e = expr_compose(e, e);
    n /= 2;
  }
  return expr_compose(e, acc);
}

unsigned long inv_mod(unsigned long x, unsigned long modulus) {
  // Avoid mutating the arguments
  unsigned long a = x;
  unsigned long b = modulus;
  unsigned long s0 = 1;
  unsigned long s1 = 0;

  while (true) {
    unsigned long q1 = a / b;
    unsigned long c = a - q1 * b;
    if (c == 0) {
      if (b != 1) {
        die("Cannot invert %lu mod %lu\n", x, modulus);
      }
      return s1;
    }
    a = b;
    b = c;

    unsigned long s2 = s0 - q1 * s1;
    s0 = s1;
    s1 = s2;
  }
}

struct expr expr_inv(struct expr e) {
  unsigned long inv_mul = inv_mod(e.mul, PART2_CARD_COUNT);
  return (struct expr){
      .mul = inv_mul,
      .add = PART2_CARD_COUNT - mul_mod(e.add, inv_mul, PART2_CARD_COUNT),
  };
}

void print_expr(const char *label, struct expr e) {
  printf("%s(i) = %lu + %lu * i\n", label, e.add, e.mul);
}

void part2(const struct shuffle_list *input) {
  struct expr single_round = {.mul = 1, .add = 0};
  for (unsigned i = 0; i < input->size; i++) {
    single_round = expr_compose(shuf_to_expr(&input->data[i]), single_round);
  }

  printf("After 1 round: %lu -> %lu\n", PART2_CARD_INDEX,
         expr_apply(single_round, PART2_CARD_INDEX));
  struct expr repeated = expr_pow(single_round, PART2_REPEAT_COUNT);
  printf("After %lu times: %lu -> %lu\n", PART2_REPEAT_COUNT, PART2_CARD_INDEX,
         expr_apply(repeated, PART2_CARD_INDEX));
  struct expr inverted = expr_inv(repeated);
  printf("After %lu times: %lu <- %lu\n", PART2_REPEAT_COUNT, PART2_CARD_INDEX,
         expr_apply(inverted, PART2_CARD_INDEX));

  /*
  struct expr r4 = {.mul = 1, .add = 0};
  for (unsigned i = 0; i < input->size * PART2_REPEAT_COUNT; i++) {
    r4 = expr_compose(shuf_to_expr(&input->data[i % input->size]), r4);
  }

  printf("After %lu rounds: %lu -> %lu\n", PART2_REPEAT_COUNT, PART2_CARD_INDEX,
         expr_apply(r4, PART2_CARD_INDEX));
  */
}

int main(void) {
  struct shuffle_list input;
  read_input(&input);
  part1(&input);
  part2(&input);
  return 0;
}
