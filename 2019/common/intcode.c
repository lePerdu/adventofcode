#include <ctype.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "intcode.h"

static enum parse_error parse_intcode(const char **cursor,
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

static enum parse_error expect_comma(const char **cursor) {
  if (**cursor == ',') {
    (*cursor)++;
    return SUCCESS;
  } else {
    return EXPECTED_COMMA;
  }
}

static enum parse_error expect_eof(const char **cursor) {
  while (isspace(**cursor)) {
    (*cursor)++;
  }

  if (**cursor == 0) {
    return SUCCESS;
  } else {
    return EXPECTED_EOF;
  }
}

static int parse_line(const char *line, struct intcode_program *prog) {
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

void read_input_from_file(struct intcode_program *prog, FILE *file) {
  char *line_buf = NULL;
  size_t buf_size = 0;
  if (getline(&line_buf, &buf_size, file) < 0) {
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

void read_input(struct intcode_program *prog) {
  read_input_from_file(prog, stdin);
}

const char *machine_state_str(enum machine_state s) {
  switch (s) {
  case RUNNING:
    return "RUNNING";
  case WAIT_INPUT:
    return "WAIT_INPUT";
  case WAIT_OUTPUT:
    return "WAIT_OUTPUT";
  case HALTED:
    return "HALTED";
  }
}

enum op_code {
  OP_ADD = 1,
  OP_MUL = 2,
  OP_IN = 3,
  OP_OUT = 4,
  OP_JNZ = 5,
  OP_JZ = 6,
  OP_LT = 7,
  OP_EQ = 8,
  OP_BASE = 9,
  OP_HALT = 99,
};

enum param_mode {
  PARAM_POS = 0,
  PARAM_IMM = 1,
  PARAM_REL = 2,
};

static void intbuf_init(struct machine_int_buffer *b) { b->head = b->size = 0; }

static bool intbuf_empty(const struct machine_int_buffer *b) {
  return b->size == 0;
}

static bool intbuf_full(const struct machine_int_buffer *b) {
  return b->size == MACHINE_BUF_SIZE;
}

static void intbuf_push(struct machine_int_buffer *b, intcode val) {
  if (intbuf_full(b)) {
    die("Attempted to push to full buffer\n");
  }
  b->data[(b->head + b->size) % MACHINE_BUF_SIZE] = val;
  b->size++;
}

static intcode intbuf_pop(struct machine_int_buffer *b) {
  if (intbuf_empty(b)) {
    die("Attempted to pop from empty buffer\n");
  }
  intcode val = b->data[b->head];
  b->head = (b->head + 1) % MACHINE_BUF_SIZE;
  b->size--;
  return val;
}

void machine_init(struct machine *machine,
                  const struct intcode_program *program_code) {
  memset(machine->memory, 0, sizeof(machine->memory));
  memcpy(machine->memory, program_code->data,
         sizeof(program_code->data[0]) * program_code->size);

  machine->instr_pointer = 0;
  machine->relative_base = 0;
  machine->state = RUNNING;
  intbuf_init(&machine->input_buf);
  intbuf_init(&machine->output_buf);
}

bool machine_can_input(const struct machine *m) {
  return !machine_halted(m) && !intbuf_full(&m->input_buf);
}

bool machine_has_output(const struct machine *m) {
  return !intbuf_empty(&m->output_buf);
}

void machine_input_one(struct machine *m, intcode v) {
  intbuf_push(&m->input_buf, v);
}

intcode machine_output_one(struct machine *m) {
  return intbuf_pop(&m->output_buf);
}

unsigned machine_input(struct machine *machine, const intcode buf[],
                       unsigned n) {
  unsigned n_read = 0;
  while (machine_can_input(machine) && n_read < n) {
    machine_input_one(machine, buf[n_read++]);
  }
  return n_read;
}

unsigned machine_output(struct machine *machine, intcode buf[], unsigned n) {
  unsigned n_read = 0;
  while (machine_has_output(machine) && n_read < n) {
    buf[n_read++] = machine_output_one(machine);
  }
  return n_read;
}

static intcode machine_read_pos(const struct machine *machine,
                                intcode pointer) {
  if (0 <= pointer && (size_t)pointer < MACHINE_MEM_SIZE) {
    return machine->memory[pointer];
  } else {
    die("Attmpted to read invalid position: %ld\n", pointer);
  }
}

static void machine_set_pos(struct machine *machine, intcode pointer,
                            intcode value) {
  if (0 <= pointer && (size_t)pointer < MACHINE_MEM_SIZE) {
    machine->memory[pointer] = value;
  } else {
    fprintf(stderr, "Attmpted to read invalid position: %lu\n", pointer);
    exit(EXIT_FAILURE);
  }
}

static intcode machine_read(const struct machine *m, enum param_mode mode,
                            size_t pointer) {
  switch (mode) {
  case PARAM_POS:
    return machine_read_pos(m, pointer);
  case PARAM_IMM:
    return pointer;
  case PARAM_REL:
    return machine_read_pos(m, m->relative_base + pointer);
  default:
    fprintf(stderr, "Invalid mode: %d\n", mode);
    exit(EXIT_FAILURE);
  }
}

static void machine_write(struct machine *m, enum param_mode mode,
                          intcode pointer, intcode value) {
  switch (mode) {
  case PARAM_POS:
    return machine_set_pos(m, pointer, value);
  case PARAM_IMM:
    fprintf(stderr, "Invalid mode for set: PARAM_IMM\n");
    exit(EXIT_FAILURE);
  case PARAM_REL:
    return machine_set_pos(m, m->relative_base + pointer, value);
  default:
    fprintf(stderr, "Invalid mode: %d\n", mode);
    exit(EXIT_FAILURE);
  }
}

static intcode machine_read_instr(struct machine *machine) {
  return machine_read_pos(machine, machine->instr_pointer++);
}

typedef intcode (*binary_operator)(intcode a, intcode b);
typedef bool (*pred_operator)(intcode a);

static void run_binary_op(struct machine *machine, intcode instr,
                          binary_operator op) {
  intcode param_modes = instr / 100;
  intcode arg_a = machine_read_instr(machine);
  enum param_mode mode_a = param_modes % 10;
  intcode arg_b = machine_read_instr(machine);
  enum param_mode mode_b = param_modes / 10 % 10;
  intcode arg_dest = machine_read_instr(machine);
  enum param_mode mode_dest = param_modes / 100 % 10;
  intcode value_a = machine_read(machine, mode_a, arg_a);
  intcode value_b = machine_read(machine, mode_b, arg_b);
  machine_write(machine, mode_dest, arg_dest, op(value_a, value_b));
}

static intcode add_op(intcode a, intcode b) { return a + b; }

static intcode mul_op(intcode a, intcode b) { return a * b; }

static intcode lt_op(intcode a, intcode b) { return a < b ? 1 : 0; }

static intcode eq_op(intcode a, intcode b) { return a == b ? 1 : 0; }

static void run_jump_op(struct machine *machine, intcode instr,
                        pred_operator op) {
  intcode param_modes = instr / 100;
  intcode arg_flag = machine_read_instr(machine);
  enum param_mode mode_flag = param_modes % 10;
  intcode arg_dest = machine_read_instr(machine);
  enum param_mode mode_dest = param_modes / 10 % 10;

  intcode flag = machine_read(machine, mode_flag, arg_flag);
  intcode dest = machine_read(machine, mode_dest, arg_dest);

  if (op(flag)) {
    machine->instr_pointer = dest;
  }
}

static bool if_nz(intcode a) { return a != 0; }
static bool if_z(intcode a) { return a == 0; }

static void machine_step(struct machine *machine) {
  if (machine_halted(machine)) {
    return;
  }
  // Assume running, change it later
  // TODO Change to running at end of program?
  machine->state = RUNNING;

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
    if (intbuf_empty(&machine->input_buf)) {
      machine->instr_pointer--;
      machine->state = WAIT_INPUT;
      break;
    }

    intcode dest = machine_read_instr(machine);
    enum param_mode mode = param_modes % 10;

    machine_write(machine, mode, dest, intbuf_pop(&machine->input_buf));

    break;
  }
  case OP_OUT: {
    if (intbuf_full(&machine->output_buf)) {
      machine->instr_pointer--;
      machine->state = WAIT_OUTPUT;
      break;
    }

    intcode arg = machine_read_instr(machine);
    enum param_mode mode = param_modes % 10;

    intcode value = machine_read(machine, mode, arg);

    intbuf_push(&machine->output_buf, value);

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
  case OP_BASE: {
    intcode dest = machine_read_instr(machine);
    enum param_mode mode = param_modes % 10;
    machine->relative_base += machine_read(machine, mode, dest);
    break;
  }
  case OP_HALT:
    machine->state = HALTED;
    break;
  default:
    fprintf(stderr, "Invalid opcode %d (%ld)\n", op, instr);
    exit(EXIT_FAILURE);
  }
}

void machine_run_til_wait(struct machine *machine) {
  do {
    machine_step(machine);
  } while (machine->state == RUNNING);
}

/**
 * Run a single program with fixed input/output buffers.
 */
size_t run_program(const struct intcode_program *program_code,
                   size_t input_size, const intcode input_buf[],
                   size_t output_size, intcode output_buf[]) {
  struct machine m;
  machine_init(&m, program_code);

  size_t input_offset = 0;
  size_t output_offset = 0;
  while (!machine_halted(&m)) {
    unsigned read_from_input =
        machine_input(&m, &input_buf[input_offset], input_size - input_offset);
    input_offset += read_from_input;

    machine_run_til_wait(&m);
    switch (m.state) {
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

    unsigned written_to_output =
        machine_output(&m, output_buf, output_size - output_offset);
    output_offset += written_to_output;
  }

  if (input_offset < input_size) {
    fprintf(stderr, "Input remaining (%lu)\n", input_size - input_offset);
    exit(EXIT_FAILURE);
  }
  // if (output_offset < output_size) {
  //   fprintf(stderr, "Output remaining (%lu)\n", output_size - output_offset);
  //   exit(EXIT_FAILURE);
  // }
  return output_offset;
}
