#include <ctype.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_PROGRAM_SIZE 1024

enum op_code {
  OP_ADD = 1,
  OP_MUL = 2,
  OP_IN = 3,
  OP_OUT = 4,
  OP_JNZ = 5,
  OP_JZ = 6,
  OP_LT = 7,
  OP_EQ = 8,
  OP_HALT = 99,
};

enum param_mode {
  PARAM_POS = 0,
  PARAM_IMM = 1,
};

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

enum parse_error parse_intcode(const char **cursor,
                               struct intcode_program *prog) {
  char *number_end = NULL;
  intcode parsed_number = strtol(*cursor, &number_end, 10);
  if (number_end == *cursor) {
    return EXPECTED_NUMBER;
  }

  if (prog->size >= MAX_PROGRAM_SIZE) {
    return PROGRAM_TOO_LONG;
  }

  prog->data[prog->size] = parsed_number;
  prog->size++;
  *cursor = number_end;
  return SUCCESS;
}

enum parse_error expect_comma(const char **cursor) {
  if (**cursor == ',') {
    (*cursor)++;
    return SUCCESS;
  } else {
    return EXPECTED_COMMA;
  }
}

enum parse_error expect_eof(const char **cursor) {
  while (isspace(**cursor)) {
    (*cursor)++;
  }

  if (**cursor == 0) {
    return SUCCESS;
  } else {
    return EXPECTED_EOF;
  }
}

int parse_line(const char *line, struct intcode_program *prog) {
  enum parse_error err;

  if ((err = parse_intcode(&line, prog)) != SUCCESS) {
    return err;
  }

  while (expect_comma(&line) == SUCCESS) {
    if ((err = parse_intcode(&line, prog)) != SUCCESS) {
      return err;
    }
  }

  return expect_eof(&line);
}

void read_input(struct intcode_program *prog) {
  char *line_buf = NULL;
  size_t buf_size = 0;
  if (getline(&line_buf, &buf_size, stdin) < 0) {
    free(line_buf);
    perror("Failed to read line");
    exit(EXIT_FAILURE);
  }

  enum parse_error err = parse_line(line_buf, prog);
  free(line_buf);

  if (err != SUCCESS) {
    fprintf(stderr, "Failed to parse line: %d\n", err);
    exit(EXIT_FAILURE);
  }
}

intcode program_get_pos(const struct intcode_program *program,
                        intcode pointer) {
  if (0 <= pointer && (size_t)pointer < program->size) {
    return program->data[pointer];
  } else {
    fprintf(stderr, "Attmpted to read invalid position: %ld\n", pointer);
    exit(EXIT_FAILURE);
  }
}

intcode program_get(const struct intcode_program *program, enum param_mode mode,
                    size_t pointer) {
  switch (mode) {
  case PARAM_POS:
    return program_get_pos(program, pointer);
  case PARAM_IMM:
    return pointer;
  default:
    fprintf(stderr, "Invalid mode: %d\n", mode);
    exit(EXIT_FAILURE);
  }
}

void program_set_pos(struct intcode_program *program, intcode pointer,
                     intcode value) {
  if (0 <= pointer && (size_t)pointer < program->size) {
    program->data[pointer] = value;
  } else {
    fprintf(stderr, "Attmpted to read invalid position: %lu\n", pointer);
    exit(EXIT_FAILURE);
  }
}

// TODO Store inside machine struct?
enum machine_state {
  RUNNING,
  WAIT_INPUT,
  WAIT_OUTPUT,
  HALTED,
};

struct machine {
  struct intcode_program memory;
  size_t instr_pointer;
  bool halted;
  // TODO expand these?
  bool input_buf_full;
  intcode input_buf[1];
  bool output_buf_full;
  intcode output_buf[1];
};

void machine_init(struct machine *machine,
                  const struct intcode_program *program_code) {
  machine->memory.size = program_code->size;
  memcpy(machine->memory.data, program_code->data,
         sizeof(program_code->data[0]) * program_code->size);

  machine->instr_pointer = 0;
  machine->halted = false;
  machine->input_buf_full = false;
  machine->output_buf_full = false;
}

intcode machine_read_instr(struct machine *machine) {
  return program_get_pos(&machine->memory, machine->instr_pointer++);
}

typedef intcode (*binary_operator)(intcode a, intcode b);
typedef bool (*pred_operator)(intcode a);

void run_binary_op(struct machine *machine, intcode instr, binary_operator op) {
  intcode param_modes = instr / 100;
  intcode arg_a = machine_read_instr(machine);
  enum param_mode mode_a = param_modes % 10;
  intcode arg_b = machine_read_instr(machine);
  enum param_mode mode_b = param_modes / 10 % 10;
  intcode arg_dest = machine_read_instr(machine);
  enum param_mode mode_dest = param_modes / 100 % 10;
  if (mode_dest != PARAM_POS) {
    fprintf(stderr,
            "Invalid mode for output parameter of instruction %ld: %d\n", instr,
            mode_dest);
    exit(EXIT_FAILURE);
  }
  intcode value_a = program_get(&machine->memory, mode_a, arg_a);
  intcode value_b = program_get(&machine->memory, mode_b, arg_b);
  program_set_pos(&machine->memory, arg_dest, op(value_a, value_b));
}

intcode add_op(intcode a, intcode b) { return a + b; }

intcode mul_op(intcode a, intcode b) { return a * b; }

intcode lt_op(intcode a, intcode b) { return a < b ? 1 : 0; }

intcode eq_op(intcode a, intcode b) { return a == b ? 1 : 0; }

void run_jump_op(struct machine *machine, intcode instr, pred_operator op) {
  intcode param_modes = instr / 100;
  intcode arg_flag = machine_read_instr(machine);
  enum param_mode mode_flag = param_modes % 10;
  intcode arg_dest = machine_read_instr(machine);
  enum param_mode mode_dest = param_modes / 10 % 10;

  intcode flag = program_get(&machine->memory, mode_flag, arg_flag);
  intcode dest = program_get(&machine->memory, mode_dest, arg_dest);

  if (op(flag)) {
    machine->instr_pointer = dest;
  }
}

bool if_nz(intcode a) { return a != 0; }
bool if_z(intcode a) { return a == 0; }

enum machine_state machine_step(struct machine *machine) {
  if (machine->halted) {
    return HALTED;
  }

  intcode instr = machine_read_instr(machine);
  enum op_code op = instr % 100;
  intcode param_modes = instr / 100;
  switch (op) {
  case OP_ADD:
    run_binary_op(machine, instr, add_op);
    break;
  case OP_MUL:
    run_binary_op(machine, instr, mul_op);
    break;
  case OP_IN: {
    if (!machine->input_buf_full) {
      machine->instr_pointer--;
      return WAIT_INPUT;
    }

    intcode dest = machine_read_instr(machine);
    enum param_mode mode = param_modes % 10;
    if (mode != PARAM_POS) {
      fprintf(stderr,
              "Invalid mode for output parameter of opcode %d (%ld): %d\n", op,
              instr, mode);
      exit(EXIT_FAILURE);
    }

    program_set_pos(&machine->memory, dest, machine->input_buf[0]);
    machine->input_buf_full = false;

    break;
  }
  case OP_OUT: {
    if (machine->output_buf_full) {
      machine->instr_pointer--;
      return WAIT_OUTPUT;
    }

    intcode arg = machine_read_instr(machine);
    enum param_mode mode = param_modes % 10;

    intcode value = program_get(&machine->memory, mode, arg);

    machine->output_buf[0] = value;
    machine->output_buf_full = true;

    break;
  }
  case OP_JNZ:
    run_jump_op(machine, instr, if_nz);
    break;
  case OP_JZ:
    run_jump_op(machine, instr, if_z);
    break;
  case OP_LT:
    run_binary_op(machine, instr, lt_op);
    break;
  case OP_EQ:
    run_binary_op(machine, instr, eq_op);
    break;
  case OP_HALT:
    machine->halted = true;
    return HALTED;
  default:
    fprintf(stderr, "Invalid opcode %d (%ld)\n", op, instr);
    exit(EXIT_FAILURE);
  }

  return RUNNING;
}

enum machine_state machine_run_til_wait(struct machine *machine) {
  enum machine_state state;
  while ((state = machine_step(machine)) == RUNNING) {
  }
  return state;
}

bool machine_can_input(const struct machine *m) { return !m->input_buf_full; }

void machine_input(struct machine *machine, intcode input) {
  machine->input_buf[0] = input;
  machine->input_buf_full = true;
}

bool machine_has_output(const struct machine *m) { return m->output_buf_full; }

intcode machine_output(struct machine *machine) {
  machine->output_buf_full = false;
  return machine->output_buf[0];
}

/**
 * Run a single program with fixed input/output buffers.
 */
void run_program(const struct intcode_program *program_code, size_t input_size,
                 const intcode input_buf[], size_t output_size,
                 intcode output_buf[]) {
  struct machine m;
  machine_init(&m, program_code);

  size_t input_offset = 0;
  size_t output_offset = 0;
  while (!m.halted) {
    if (machine_can_input(&m) && input_offset < input_size) {
      machine_input(&m, input_buf[input_offset++]);
    }

    enum machine_state state = machine_run_til_wait(&m);
    switch (state) {
    case RUNNING:
      fprintf(stderr, "Machine stopped in running state\n");
      exit(EXIT_FAILURE);
      break;
    case WAIT_INPUT:
      if (input_offset >= input_size) {
        fprintf(stderr, "Not enough input\n");
        exit(EXIT_FAILURE);
      }
      break;

    case WAIT_OUTPUT:
      if (output_offset >= output_size) {
        fprintf(stderr, "Too much output\n");
        exit(EXIT_FAILURE);
      }
      break;
    case HALTED:
      break;
    }

    if (machine_has_output(&m) && output_offset < output_size) {
      output_buf[output_offset++] = machine_output(&m);
    }
  }

  if (input_offset < input_size) {
    fprintf(stderr, "Input remaining (%lu)\n", input_size - input_offset);
    exit(EXIT_FAILURE);
  }
  if (output_offset < output_size) {
    fprintf(stderr, "Output remaining (%lu)\n", output_size - output_offset);
    exit(EXIT_FAILURE);
  }
}

#define THRUSTER_COUNT 5
#define PERM_COUNT 120

static const intcode permutations[PERM_COUNT][5] = {
    {0, 1, 2, 3, 4}, {0, 1, 2, 4, 3}, {0, 1, 3, 2, 4}, {0, 1, 3, 4, 2},
    {0, 1, 4, 2, 3}, {0, 1, 4, 3, 2}, {0, 2, 1, 3, 4}, {0, 2, 1, 4, 3},
    {0, 2, 3, 1, 4}, {0, 2, 3, 4, 1}, {0, 2, 4, 1, 3}, {0, 2, 4, 3, 1},
    {0, 3, 1, 2, 4}, {0, 3, 1, 4, 2}, {0, 3, 2, 1, 4}, {0, 3, 2, 4, 1},
    {0, 3, 4, 1, 2}, {0, 3, 4, 2, 1}, {0, 4, 1, 2, 3}, {0, 4, 1, 3, 2},
    {0, 4, 2, 1, 3}, {0, 4, 2, 3, 1}, {0, 4, 3, 1, 2}, {0, 4, 3, 2, 1},
    {1, 0, 2, 3, 4}, {1, 0, 2, 4, 3}, {1, 0, 3, 2, 4}, {1, 0, 3, 4, 2},
    {1, 0, 4, 2, 3}, {1, 0, 4, 3, 2}, {1, 2, 0, 3, 4}, {1, 2, 0, 4, 3},
    {1, 2, 3, 0, 4}, {1, 2, 3, 4, 0}, {1, 2, 4, 0, 3}, {1, 2, 4, 3, 0},
    {1, 3, 0, 2, 4}, {1, 3, 0, 4, 2}, {1, 3, 2, 0, 4}, {1, 3, 2, 4, 0},
    {1, 3, 4, 0, 2}, {1, 3, 4, 2, 0}, {1, 4, 0, 2, 3}, {1, 4, 0, 3, 2},
    {1, 4, 2, 0, 3}, {1, 4, 2, 3, 0}, {1, 4, 3, 0, 2}, {1, 4, 3, 2, 0},
    {2, 0, 1, 3, 4}, {2, 0, 1, 4, 3}, {2, 0, 3, 1, 4}, {2, 0, 3, 4, 1},
    {2, 0, 4, 1, 3}, {2, 0, 4, 3, 1}, {2, 1, 0, 3, 4}, {2, 1, 0, 4, 3},
    {2, 1, 3, 0, 4}, {2, 1, 3, 4, 0}, {2, 1, 4, 0, 3}, {2, 1, 4, 3, 0},
    {2, 3, 0, 1, 4}, {2, 3, 0, 4, 1}, {2, 3, 1, 0, 4}, {2, 3, 1, 4, 0},
    {2, 3, 4, 0, 1}, {2, 3, 4, 1, 0}, {2, 4, 0, 1, 3}, {2, 4, 0, 3, 1},
    {2, 4, 1, 0, 3}, {2, 4, 1, 3, 0}, {2, 4, 3, 0, 1}, {2, 4, 3, 1, 0},
    {3, 0, 1, 2, 4}, {3, 0, 1, 4, 2}, {3, 0, 2, 1, 4}, {3, 0, 2, 4, 1},
    {3, 0, 4, 1, 2}, {3, 0, 4, 2, 1}, {3, 1, 0, 2, 4}, {3, 1, 0, 4, 2},
    {3, 1, 2, 0, 4}, {3, 1, 2, 4, 0}, {3, 1, 4, 0, 2}, {3, 1, 4, 2, 0},
    {3, 2, 0, 1, 4}, {3, 2, 0, 4, 1}, {3, 2, 1, 0, 4}, {3, 2, 1, 4, 0},
    {3, 2, 4, 0, 1}, {3, 2, 4, 1, 0}, {3, 4, 0, 1, 2}, {3, 4, 0, 2, 1},
    {3, 4, 1, 0, 2}, {3, 4, 1, 2, 0}, {3, 4, 2, 0, 1}, {3, 4, 2, 1, 0},
    {4, 0, 1, 2, 3}, {4, 0, 1, 3, 2}, {4, 0, 2, 1, 3}, {4, 0, 2, 3, 1},
    {4, 0, 3, 1, 2}, {4, 0, 3, 2, 1}, {4, 1, 0, 2, 3}, {4, 1, 0, 3, 2},
    {4, 1, 2, 0, 3}, {4, 1, 2, 3, 0}, {4, 1, 3, 0, 2}, {4, 1, 3, 2, 0},
    {4, 2, 0, 1, 3}, {4, 2, 0, 3, 1}, {4, 2, 1, 0, 3}, {4, 2, 1, 3, 0},
    {4, 2, 3, 0, 1}, {4, 2, 3, 1, 0}, {4, 3, 0, 1, 2}, {4, 3, 0, 2, 1},
    {4, 3, 1, 0, 2}, {4, 3, 1, 2, 0}, {4, 3, 2, 0, 1}, {4, 3, 2, 1, 0},
};
