#include <ctype.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_PROGRAM_SIZE 256

struct intcode_program {
  size_t size;
  unsigned long data[MAX_PROGRAM_SIZE];
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
  unsigned long parsed_number = strtoul(*cursor, &number_end, 10);
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

void print_program(const struct intcode_program *prog) {
  if (prog->size == 0) {
    printf("no input\n");
    return;
  }

  for (size_t i = 0; i < prog->size - 1; i++) {
    printf("%lu,", prog->data[i]);
  }
  printf("%lu\n", prog->data[prog->size - 1]);
}

enum op_code {
  OP_ADD = 1,
  OP_MUL = 2,
  OP_HALT = 99,
};

unsigned long prog_get(const struct intcode_program *p, size_t offset) {
  if (offset <= p->size) {
    return p->data[offset];
  } else {
    fprintf(stderr, "Attempted to read out-of-bounds offset: %lu\n", offset);
    exit(EXIT_FAILURE);
  }
}

void prog_set(struct intcode_program *p, size_t offset, unsigned long intcode) {
  if (offset <= p->size) {
    p->data[offset] = intcode;
  } else {
    fprintf(stderr, "Attempted to write out-of-bounds offset: %lu\n", offset);
    exit(EXIT_FAILURE);
  }
}

unsigned long run_with_inputs(const struct intcode_program *original,
                              unsigned long noun, unsigned long verb) {
  struct intcode_program prog = {.size = original->size};
  memcpy(&prog.data, &original->data,
         original->size * sizeof(original->data[0]));

  // Modify according to problem
  prog.data[1] = noun;
  prog.data[2] = verb;

  size_t offset = 0;
  bool halted = false;
  while (!halted) {
    unsigned long opcode = prog_get(&prog, offset++);
    switch (opcode) {
    case OP_ADD: {
      unsigned long a_offset = prog_get(&prog, offset++);
      unsigned long b_offset = prog_get(&prog, offset++);
      unsigned long result_offset = prog_get(&prog, offset++);
      prog_set(&prog, result_offset,
               prog_get(&prog, a_offset) + prog_get(&prog, b_offset));
      break;
    }
    case OP_MUL: {
      unsigned long a_offset = prog_get(&prog, offset++);
      unsigned long b_offset = prog_get(&prog, offset++);
      unsigned long result_offset = prog_get(&prog, offset++);
      prog_set(&prog, result_offset,
               prog_get(&prog, a_offset) * prog_get(&prog, b_offset));
      break;
    }
    case OP_HALT:
      halted = true;
      break;
    default:
      fprintf(stderr, "Invalid op code: %lu\n", opcode);
      exit(EXIT_FAILURE);
    }
  }

  return prog_get(&prog, 0);
}

void part1(const struct intcode_program *input) {
  unsigned long ans = run_with_inputs(input, 12, 2);
  printf("Part1: %lu\n", ans);
}

#define MAX_ARG 99
#define TARGET_OUTPUT 19690720

void part2(const struct intcode_program *input) {
  for (unsigned long noun = 0; noun <= MAX_ARG; noun++) {
    for (unsigned long verb = 0; verb <= MAX_ARG; verb++) {
      if (run_with_inputs(input, noun, verb) == TARGET_OUTPUT) {
        unsigned long ans = 100 * noun + verb;
        printf("Part2: %lu (%lu, %lu)\n", ans, noun, verb);
        return;
      }
    }
  }

  printf("Part2: no answer found\n");
}

int main(void) {
  struct intcode_program input = {.size = 0};
  read_input(&input);

  part1(&input);
  part2(&input);

  return 0;
}
