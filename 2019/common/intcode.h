#ifndef INTCODE_H_
#define INTCODE_H_

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_PROGRAM_SIZE 18000

typedef long intcode;

struct intcode_program {
  size_t size;
  intcode data[MAX_PROGRAM_SIZE];
};

enum parse_error {
  SUCCESS = 0,
  EXPECTED_COMMA,
  EXPECTED_NUMBER,
  PROGRAM_TOO_LONG,
  EXPECTED_EOF,
};

void read_input_from_file(struct intcode_program *prog, FILE *file);

void read_input(struct intcode_program *prog);

enum machine_state {
  RUNNING,
  WAIT_INPUT,
  WAIT_OUTPUT,
  HALTED,
};

const char *machine_state_str(enum machine_state s);

#define MACHINE_MEM_SIZE (1 << 16)

#define MACHINE_BUF_SIZE 16

/**
 * Ring buffer for storing input/output.
 */
struct machine_int_buffer {
  unsigned head;
  unsigned size;
  intcode data[MACHINE_BUF_SIZE];
};

struct machine {
  intcode memory[MACHINE_MEM_SIZE];
  size_t instr_pointer;
  size_t relative_base;
  enum machine_state state;
  struct machine_int_buffer input_buf;
  struct machine_int_buffer output_buf;
};

void machine_init(struct machine *machine,
                  const struct intcode_program *program_code);

inline bool machine_halted(const struct machine *m) {
  return m->state == HALTED;
}

bool machine_can_input(const struct machine *m);
bool machine_has_output(const struct machine *m);

/**
 * Input a single value into the machine. Caller must ensure that
 * `machine_can_input()` returns true.
 */
void machine_input_one(struct machine *machine, intcode v);

/**
 * Output a single value from the machine. Caller must ensure that
 * `machine_has_output()` returns true.
 */
intcode machine_output_one(struct machine *machine);

unsigned machine_input(struct machine *machine, const intcode buf[],
                       unsigned n);
unsigned machine_output(struct machine *machine, intcode buf[], unsigned n);

void machine_run_til_wait(struct machine *machine);

/**
 * Run a single program with fixed input/output buffers.
 */
size_t run_program(const struct intcode_program *program_code,
                   size_t input_size, const intcode input_buf[],
                   size_t output_size, intcode output_buf[]);

#endif
