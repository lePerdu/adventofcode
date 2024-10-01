#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "intcode.h"

#define INPUT_FILE "input.txt"
#define INSTR_FILE "instr.txt"

#define ITEM_COUNT 8

static const char *const ALL_ITEMS[ITEM_COUNT] = {
    "astrolabe", "candy cane", "coin", "easter egg",
    "hypercube", "manifold",   "mug",  "pointer",
};

static void read_output(struct machine *m) {
  do {
    machine_run_til_wait(m);
    while (machine_has_output(m)) {
      putchar(machine_output_one(m));
    }
  } while (m->state == WAIT_OUTPUT);
}

static void send_input(const char *command, struct machine *m) {
  while (*command != 0) {
    machine_run_til_wait(m);
    assert(m->state == WAIT_INPUT);

    while (*command != 0 && machine_can_input(m)) {
      machine_input_one(m, *command);
      command++;
    }
  }
}

static void send_print_input(const char *command, struct machine *m) {
  printf("%s", command);

  while (*command != 0) {
    machine_run_til_wait(m);
    assert(m->state == WAIT_INPUT);

    while (*command != 0 && machine_can_input(m)) {
      machine_input_one(m, *command);
      command++;
    }
  }
}

static bool input_command(FILE *input_file, FILE *output_file,
                          struct machine *m) {
  char *line_buf = NULL;
  size_t buf_size = 0;
  ssize_t line_size = getline(&line_buf, &buf_size, input_file);
  if (line_size == -1) {
    free(line_buf);
    return false;
  }

  fputs(line_buf, output_file);
  send_input(line_buf, m);
  free(line_buf);
  return true;
}

void record(const struct intcode_program *input) {
  FILE *instr_file = fopen(INSTR_FILE, "w");

  struct machine *m = malloc(sizeof(*m));
  machine_init(m, input);

  while (true) {
    read_output(m);
    switch (m->state) {
    case RUNNING:
      abort();
    case WAIT_INPUT:
      if (input_command(stdin, instr_file, m)) {
        break;
      } else {
        goto END;
      }
    case WAIT_OUTPUT:
      break;
    case HALTED:
      goto END;
    default:
      abort();
    }
  }
END:

  free(m);
  fclose(instr_file);
}

void replay(FILE *instr_file, struct machine *m) {
  while (true) {
    read_output(m);

    switch (m->state) {
    case RUNNING:
      abort();
    case WAIT_INPUT:
      if (input_command(instr_file, stdout, m)) {
        break;
      } else {
        return;
      }
    case WAIT_OUTPUT:
      break;
    case HALTED:
      return;
    default:
      abort();
    }
  }
}

void try_items(struct machine *m, int start) {
  if (start == ITEM_COUNT) {
    send_print_input("inv\n", m);
    read_output(m);
    send_print_input("east\n", m);
    read_output(m);
    if (m->state == HALTED) {
      exit(EXIT_SUCCESS);
    }
    return;
  }

  try_items(m, start + 1);
  send_print_input("drop ", m);
  send_print_input(ALL_ITEMS[start], m);
  send_print_input("\n", m);
  read_output(m);
  try_items(m, start + 1);
  send_print_input("take ", m);
  send_print_input(ALL_ITEMS[start], m);
  send_print_input("\n", m);
  read_output(m);
}

void part1(struct intcode_program *input) {
  struct machine *m = malloc(sizeof(*m));
  machine_init(m, input);

  FILE *instr_file = fopen(INSTR_FILE, "r");
  replay(instr_file, m);

  try_items(m, 0);

  fclose(instr_file);

  free(m);
}

int main(void) {
  FILE *f = fopen("input.txt", "r");
  struct intcode_program input;
  read_input_from_file(&input, f);
  fclose(f);

  // record(&input);
  part1(&input);
  return 0;
}
