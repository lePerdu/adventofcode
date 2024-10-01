#ifndef INTCODE_H_
#define INTCODE_H_

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_PROGRAM_SIZE 3000

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

// TODO Store inside machine struct?
enum machine_state {
  RUNNING,
  WAIT_INPUT,
  WAIT_OUTPUT,
  HALTED,
};

#define MACHINE_MEM_SIZE (1 << 16)

struct machine {
  intcode memory[MACHINE_MEM_SIZE];
  size_t instr_pointer;
  size_t relative_base;
  bool halted;
  // TODO expand these?
  bool input_buf_full;
  intcode input_buf[1];
  bool output_buf_full;
  intcode output_buf[1];
};

void machine_init(struct machine *machine,
                  const struct intcode_program *program_code);

inline bool machine_can_input(const struct machine *m) { return !m->input_buf_full; }
inline bool machine_has_output(const struct machine *m) { return m->output_buf_full; }

void machine_input(struct machine *machine, intcode input);
intcode machine_output(struct machine *machine);

enum machine_state machine_run_til_wait(struct machine *machine);

/**
 * Run a single program with fixed input/output buffers.
 */
size_t run_program(const struct intcode_program *program_code,
                   size_t input_size, const intcode input_buf[],
                   size_t output_size, intcode output_buf[]);

#endif
