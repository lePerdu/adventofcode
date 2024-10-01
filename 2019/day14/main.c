#include <ctype.h>
#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"

#define MAX_REACTANTS 8
#define MAX_REACTIONS 64
#define MAX_NAME_LEN 5

struct reactant_name {
  char data[MAX_NAME_LEN + 1];
};

struct react_amount {
  uint8_t amount;
  struct reactant_name name;
};

struct reaction {
  struct react_amount output;
  uint8_t reactant_count;
  struct react_amount reactants[MAX_REACTANTS];
};

struct input {
  uint8_t reaction_count;
  struct reaction reactions[MAX_REACTIONS];
};

enum parse_error {
  PE_SUCESS = 0,
  PE_EXPECT_AMOUNT,
  PE_AMOUNT_OOB,
  PE_EXPECT_NAME,
  PE_NAME_TOO_LONG,
  PE_EXPECT_ARROW,
  PE_EXPECT_EOF,
  PE_UNKNOWN,
};

void skip_whitespace(const char **cursor) {
  while (isspace(**cursor)) {
    (*cursor)++;
  }
}

int expect_str(const char **cursor, const char *s) {
  skip_whitespace(cursor);
  while (*s != 0) {
    if (**cursor != *s) {
      return PE_UNKNOWN;
    }

    (*cursor)++;
    s++;
  }

  skip_whitespace(cursor);
  return 0;
}

enum parse_error expect_eol(const char **cursor) {
  skip_whitespace(cursor);
  return **cursor == 0 ? PE_SUCESS : PE_EXPECT_EOF;
}

enum parse_error parse_number(const char **cursor, uint8_t *n) {
  char *endptr;
  unsigned long parsed = strtoul(*cursor, &endptr, 10);
  if (endptr == *cursor) {
    return PE_EXPECT_AMOUNT;
  }

  if (parsed > UINT8_MAX) {
    fprintf(stderr, "Warning: Amount out of range: %lu\n", parsed);
    return PE_AMOUNT_OOB;
  }

  *n = parsed;
  *cursor = endptr;
  return PE_SUCESS;
}

int parse_name(const char **cursor, struct reactant_name *name) {
  const char *initial_cursor = *cursor;

  unsigned offset = 0;
  char c;
  while (isalpha(c = **cursor)) {
    if (offset >= MAX_NAME_LEN) {
      fprintf(stderr, "Warning: Name too long: %s\n", initial_cursor);
      return PE_NAME_TOO_LONG;
    }

    name->data[offset] = c;
    (*cursor)++;
    offset++;
  }
  if (offset == 0) {
    return PE_EXPECT_NAME;
  }

  name->data[offset] = 0;
  return PE_SUCESS;
}

int parse_react_amount(const char **cursor, struct react_amount *amount) {
  int err = parse_number(cursor, &amount->amount);
  if (err != 0) {
    return err;
  }

  skip_whitespace(cursor);
  return parse_name(cursor, &amount->name);
}

int parse_reactant_list(const char **cursor, struct reaction *r) {
  r->reactant_count = 0;

  int err;
  while (1) {
    err = parse_react_amount(cursor, &r->reactants[r->reactant_count]);
    if (err != 0) {
      return err;
    }
    r->reactant_count++;

    err = expect_str(cursor, ",");
    if (err != 0) {
      // List is done
      return PE_SUCESS;
    }
  }
}

int parse_reaction(const char **cursor, struct reaction *r) {
  int err = parse_reactant_list(cursor, r);
  if (err != 0) {
    return err;
  }

  err = expect_str(cursor, "=>");
  if (err != 0) {
    return PE_EXPECT_ARROW;
  }

  err = parse_react_amount(cursor, &r->output);
  if (err != 0) {
    return err;
  }

  return expect_eol(cursor);
}

int read_reaction(char **line_buf, struct input *input) {
  size_t line_len;

  errno = 0;
  if (getline(line_buf, &line_len, stdin) < 0) {
    if (errno == 0) {
      return -1;
    } else {
      perror_die("Failed to read input");
    }
  }

  const char *cursor = *line_buf;
  // Empty line -> done reading
  if (expect_eol(&cursor) == 0) {
    return -1;
  }

  if (input->reaction_count >= MAX_REACTIONS) {
    die("Too many reactions");
  }

  int err = parse_reaction(&cursor, &input->reactions[input->reaction_count]);
  if (err != 0) {
    die("Failed to parse input line. Error %d: %s", err, cursor);
  }
  input->reaction_count++;

  return 0;
}

void read_input(struct input *input) {
  input->reaction_count = 0;

  char *shared_line_buf = NULL;

  while (read_reaction(&shared_line_buf, input) == 0) {
  }

  free(shared_line_buf);

  if (input->reaction_count == 0) {
    die("Failed to parse input");
  }
}

void input_print(const struct input *input) {
  for (unsigned i = 0; i < input->reaction_count; i++) {
    const struct reaction *r = &input->reactions[i];

    printf("%u %s", r->reactants[0].amount, r->reactants[0].name.data);
    for (unsigned j = 1; j < r->reactant_count; j++) {
      printf(", %u %s", r->reactants[j].amount, r->reactants[j].name.data);
    }

    printf(" => %u %s\n", r->output.amount, r->output.name.data);
  }
}

// Increasing index of reactants
typedef uint8_t reactant_id;

struct reactant_desc {
  // Put name first so we can search the list just by the name
  struct reactant_name name;
  reactant_id id;
};

#define ORE_NAME ((struct reactant_name){.data = "ORE"})
#define FUEL_NAME ((struct reactant_name){.data = "FUEL"})

struct recipe_amount {
  uint8_t amount;
  reactant_id id;
};

struct recipe {
  uint8_t output_amount;
  uint8_t reactant_count;
  struct recipe_amount reactants[MAX_REACTIONS];
};

// More optimized organization of reactions.
// Uses indicies to represent reactants instead of names, with mappings between
// the representations.
struct reaction_set {
  uint8_t reactant_count;
  // Includes an empty entry at index ORE_ID for consistency
  struct recipe reactant_recipes[MAX_REACTIONS + 1];
  // Sorted by name, each entry contains ID
  struct reactant_desc sorted_name_to_id[MAX_REACTIONS + 1];
  // sorted_name_to_id[id_to_name_index[id]] -> reactant descriptor
  uint8_t id_to_name_index[MAX_REACTIONS + 1];
};

int compare_reactant_desc(const void *a, const void *b) {
  const struct reactant_desc *ra = a;
  const struct reactant_desc *rb = b;
  return strcmp(ra->name.data, rb->name.data);
}

reactant_id reaction_set_lookup_id(const struct reaction_set *set,
                                   const struct reactant_name *name) {
  const struct reactant_desc *desc =
      bsearch(name, set->sorted_name_to_id, set->reactant_count,
              sizeof(set->sorted_name_to_id[0]), compare_reactant_desc);
  if (desc == NULL) {
    die("Invalid reactant name: %s\n", name->data);
  }
  return desc->id;
}

const struct reactant_name *
reaction_set_lookup_name(const struct reaction_set *set, reactant_id id) {
  if (0 <= id && id < set->reactant_count) {
    uint8_t sorted_index = set->id_to_name_index[id];
    return &set->sorted_name_to_id[sorted_index].name;
  } else {
    die("Invalid reactant ID: %d\n", id);
  }
}

void reaction_set_init(struct reaction_set *set,
                       const struct input *raw_reactions) {
  set->reactant_count = 1;
  // Blank info for ORE
  set->reactant_recipes[0] =
      (struct recipe){.reactant_count = 0, .output_amount = 1};
  set->sorted_name_to_id[0] = (struct reactant_desc){
      .name = ORE_NAME,
      .id = 0,
  };
  //  id_to_name_index is filled in at the end

  // Create ID mapping in frist pass
  for (unsigned raw_index = 0; raw_index < raw_reactions->reaction_count;
       raw_index++) {
    const struct reaction *raw = &raw_reactions->reactions[raw_index];
    reactant_id new_id = set->reactant_count++;
    set->sorted_name_to_id[new_id] = (struct reactant_desc){
        .id = new_id,
        .name = raw->output.name,
    };
  }

  qsort(set->sorted_name_to_id, set->reactant_count,
        sizeof(set->sorted_name_to_id[0]), compare_reactant_desc);

  // Populate reverse mapping
  // TODO Is this actually used?
  for (unsigned sorted_index = 0; sorted_index < set->reactant_count;
       sorted_index++) {
    set->id_to_name_index[set->sorted_name_to_id[sorted_index].id] =
        sorted_index;
  }

  for (unsigned raw_index = 0; raw_index < raw_reactions->reaction_count;
       raw_index++) {
    const struct reaction *raw = &raw_reactions->reactions[raw_index];
    // Output ID = index into recipes
    unsigned output_id = reaction_set_lookup_id(set, &raw->output.name);
    struct recipe *recipe = &set->reactant_recipes[output_id];

    recipe->output_amount = raw->output.amount;
    recipe->reactant_count = raw->reactant_count;
    for (unsigned i = 0; i < raw->reactant_count; i++) {
      recipe->reactants[i] = (struct recipe_amount){
          .amount = raw->reactants[i].amount,
          .id = reaction_set_lookup_id(set, &raw->reactants[i].name),
      };
    }
  }
}

void reaction_set_print(const struct reaction_set *set) {
  for (unsigned i = 0; i < set->reactant_count; i++) {
    const struct recipe *r = &set->reactant_recipes[i];

    for (unsigned j = 0; j < r->reactant_count; j++) {
      reactant_id id = r->reactants[j].id;
      const char *name = reaction_set_lookup_name(set, id)->data;
      printf("%u %s(%d), ", r->reactants[j].amount, name, id);
    }

    const char *output_name = reaction_set_lookup_name(set, i)->data;
    printf(" => %u %s(%d)\n", r->output_amount, output_name, i);
  }
}

unsigned long find_required_ore(const struct reaction_set *input,
                                unsigned long fuel_amount) {
  // Amounts of each reactant, indexed by ID
  // Negative value indicates extra amounts unused
  long amounts_needed[MAX_REACTIONS + 1];
  memset(amounts_needed, 0, sizeof(amounts_needed));

  reactant_id fuel_id = reaction_set_lookup_id(input, &FUEL_NAME);
  amounts_needed[fuel_id] = fuel_amount;

  // Start at 1 to skip ORE
  bool has_remaining;
  do {
    has_remaining = false;
    // TODO Queue instead of searching the full array?
    for (unsigned i = 1; i < input->reactant_count; i++) {
      if (amounts_needed[i] > 0) {
        has_remaining = true;
        const struct recipe *r = &input->reactant_recipes[i];
        unsigned long needed = amounts_needed[i];

        uint8_t recipe_output = r->output_amount;
        // Number of times the recipe needs to be run
        unsigned long recipe_multiplier =
            // Integer version of ceil(needed / recipe_output)
            needed / recipe_output + (needed % recipe_output != 0 ? 1 : 0);

        unsigned long amount_produced = recipe_multiplier * recipe_output;

        amounts_needed[i] -= amount_produced;

        // printf("Need %lu %s(%d): Run recipe %lu times\n", needed,
        //        reaction_set_lookup_name(input, i)->data, i,
        //        recipe_multiplier);

        for (unsigned req = 0; req < r->reactant_count; req++) {
          reactant_id req_id = r->reactants[req].id;
          unsigned long req_amount =
              recipe_multiplier * r->reactants[req].amount;

          // printf("\tNeed => %lu %s(%d)\n", req_amount,
          //        reaction_set_lookup_name(input, req_id)->data, req_id);
          amounts_needed[req_id] += req_amount;
        }
      }
    }
  } while (has_remaining);

  return amounts_needed[0];
}

void part1(const struct reaction_set *input) {
  printf("Part1: %lu\n", find_required_ore(input, 1));
}

#define INITIAL_ORE 1000000000000

void part2(const struct reaction_set *input) {
  // Binary search to find the maximum fuel
  unsigned long low = 0;
  unsigned long high = INITIAL_ORE;
  while (low < high) {
    unsigned long mid = (high + low + 1) / 2;
    unsigned long ore_for_mid = find_required_ore(input, mid);
    if (ore_for_mid > INITIAL_ORE) {
      high = mid - 1;
    } else {
      low = mid;
    }
  }

  unsigned long max_fuel = low;
  printf("Part2: %lu\n", max_fuel);
}

int main(void) {
  struct input input;
  read_input(&input);
  struct reaction_set set;
  reaction_set_init(&set, &input);

  part1(&set);
  part2(&set);
  return 0;
}
