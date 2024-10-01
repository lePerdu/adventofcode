#include <assert.h>
#include <stddef.h>
#include <stdio.h>

#include "intcode.c"

void test(const struct intcode_program *input) {
  intcode output_buf[64] = {};
  size_t output_size = run_program(input, 0, NULL, 64, output_buf);
  for (size_t i = 0; i < output_size; i++) {
    printf("%ld\n", output_buf[i]);
  }
}

void part1(const struct intcode_program *input) {
  intcode input_buf[1] = {1};
  intcode output_buf[1];
  size_t output_size = run_program(input, 1, input_buf, 1, output_buf);
  assert(output_size == 1);
  printf("Part1: %ld\n", output_buf[0]);
}

void part2(const struct intcode_program *input) {
  intcode input_buf[1] = {2};
  intcode output_buf[1] = {};
  size_t output_size = run_program(input, 1, input_buf, 1, output_buf);
  assert(output_size == 1);
  printf("Part2: %ld\n", output_buf[0]);
}

int main(void) {
  struct intcode_program input;
  read_input(&input);
  // test(&input);
  // return 0;
  part1(&input);
  part2(&input);
  return 0;
}
