#include <assert.h>
#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "intcode.h"

size_t char_to_intcode(intcode *output, const char *s) {
  size_t n = 0;
  while (*s != 0) {
    *output = *s;
    output++;
    s++;
    n++;
  }
  return n;
}

void print_output(struct machine *m) {
  machine_run_til_wait(m);
  while (machine_has_output(m)) {
    while (machine_has_output(m)) {
      intcode v = machine_output_one(m);
      if (isascii(v)) {
        putchar(v);
      } else {
        printf("[%ld]", v);
      }
    }
    machine_run_til_wait(m);
  }
}

void write_instr(struct machine *m, const char *command) {
  print_output(m);

  const char *p = command;
  while (*p != 0) {
    machine_run_til_wait(m);
    if (machine_halted(m)) {
      die("Machine halted before consuming input: %s\n", command);
    }

    while (machine_can_input(m) && *p != 0) {
      machine_input_one(m, *p);
      putchar(*p);
      p++;
    }
  }
}

void part1(const struct intcode_program *input) {
  puts("Part1:");

  struct machine m;
  machine_init(&m, input);

  write_instr(&m, "NOT C J\n");
  write_instr(&m, "AND D J\n");
  write_instr(&m, "NOT A T\n");
  write_instr(&m, "OR T J\n");
  write_instr(&m, "WALK\n");
  print_output(&m);
  if (!machine_halted(&m)) {
    die("Machine in unexpected state: %d\n", m.state);
  }
}

void part2(const struct intcode_program *input) {
  puts("Part2:");

  struct machine m;
  machine_init(&m, input);

  write_instr(&m, "NOT C J\n");
  write_instr(&m, "AND D J\n");
  write_instr(&m, "AND H J\n");
  write_instr(&m, "NOT A T\n");
  write_instr(&m, "OR T J\n");
  write_instr(&m, "NOT B T\n");
  write_instr(&m, "AND D T\n");
  write_instr(&m, "OR T J\n");
  write_instr(&m, "RUN\n");
  print_output(&m);
  if (!machine_halted(&m)) {
    die("Machine in unexpected state: %d\n", m.state);
  }
}


int main(void) {
  struct intcode_program input;
  FILE *f = fopen("input.txt", "r");
  read_input_from_file(&input, f);
  fclose(f);

  part1(&input);
  part2(&input);
  return 0;
}
