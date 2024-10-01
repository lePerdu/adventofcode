#include <ctype.h>
#include <stdbool.h>
#include <stddef.h>
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

typedef intcode (*binary_operator)(intcode a, intcode b);
typedef bool (*pred_operator)(intcode a);

void run_binary_op(struct intcode_program *program, size_t *instr_pointer,
                   intcode instr, binary_operator op) {
  intcode param_modes = instr / 100;
  intcode arg_a = program_get_pos(program, (*instr_pointer)++);
  enum param_mode mode_a = param_modes % 10;
  intcode arg_b = program_get_pos(program, (*instr_pointer)++);
  enum param_mode mode_b = param_modes / 10 % 10;
  intcode arg_dest = program_get_pos(program, (*instr_pointer)++);
  enum param_mode mode_dest = param_modes / 100 % 10;
  if (mode_dest != PARAM_POS) {
    fprintf(stderr,
            "Invalid mode for output parameter of instruction %ld: %d\n", instr,
            mode_dest);
    exit(EXIT_FAILURE);
  }
  intcode value_a = program_get(program, mode_a, arg_a);
  intcode value_b = program_get(program, mode_b, arg_b);
  program_set_pos(program, arg_dest, op(value_a, value_b));
}

intcode add_op(intcode a, intcode b) { return a + b; }

intcode mul_op(intcode a, intcode b) { return a * b; }

intcode lt_op(intcode a, intcode b) { return a < b ? 1 : 0; }

intcode eq_op(intcode a, intcode b) { return a == b ? 1 : 0; }

void run_jump_op(struct intcode_program *program, size_t *instr_pointer,
                 intcode instr, pred_operator op) {
  intcode param_modes = instr / 100;
  intcode arg_flag = program_get_pos(program, (*instr_pointer)++);
  enum param_mode mode_flag = param_modes % 10;
  intcode arg_dest = program_get_pos(program, (*instr_pointer)++);
  enum param_mode mode_dest = param_modes / 10 % 10;

  intcode flag = program_get(program, mode_flag, arg_flag);
  intcode dest = program_get(program, mode_dest, arg_dest);

  if (op(flag)) {
    *instr_pointer = dest;
  }
}

bool if_nz(intcode a) { return a != 0; }
bool if_z(intcode a) { return a == 0; }

void run_program(const struct intcode_program *program_code,
                 intcode input_value) {
  // Copy into running program to not modify the input
  struct intcode_program program;
  program.size = program_code->size;
  memcpy(program.data, program_code->data,
         sizeof(program.data[0]) * program.size);

  printf("Running\n");

  size_t instr_pointer = 0;
  bool input_used = false;
  bool halted = false;
  while (!halted) {
    intcode instr = program_get_pos(&program, instr_pointer++);
    enum op_code op = instr % 100;
    intcode param_modes = instr / 100;
    switch (op) {
    case OP_ADD:
      run_binary_op(&program, &instr_pointer, instr, add_op);
      break;
    case OP_MUL:
      run_binary_op(&program, &instr_pointer, instr, mul_op);
      break;
    case OP_IN: {
      intcode dest = program_get_pos(&program, instr_pointer++);
      enum param_mode mode = param_modes % 10;
      if (mode != PARAM_POS) {
        fprintf(stderr,
                "Invalid mode for output parameter of opcode %d (%ld): %d\n",
                op, instr, mode);
        exit(EXIT_FAILURE);
      }

      if (input_used) {
        fprintf(stderr, "No input available");
        exit(EXIT_FAILURE);
      }

      program_set_pos(&program, dest, input_value);
      input_used = true;
      break;
    }
    case OP_OUT: {
      intcode arg = program_get_pos(&program, instr_pointer++);
      enum param_mode mode = param_modes % 10;

      intcode value = program_get(&program, mode, arg);

      printf("%ld\n", value);
      break;
    }
    case OP_JNZ:
      run_jump_op(&program, &instr_pointer, instr, if_nz);
      break;
    case OP_JZ:
      run_jump_op(&program, &instr_pointer, instr, if_z);
      break;
    case OP_LT:
      run_binary_op(&program, &instr_pointer, instr, lt_op);
      break;
    case OP_EQ:
      run_binary_op(&program, &instr_pointer, instr, eq_op);
      break;
    case OP_HALT:
      halted = true;
      break;
    default:
      fprintf(stderr, "Invalid opcode %d (%ld)\n", op, instr);
      exit(EXIT_FAILURE);
    }
  }

  printf("Done\n");
}

void part1(const struct intcode_program *program) { run_program(program, 1); }

void part2(const struct intcode_program *program) { run_program(program, 5); }

int main(void) {
  struct intcode_program program;
  read_input(&program);
  part1(&program);
  part2(&program);

  return 0;
}
