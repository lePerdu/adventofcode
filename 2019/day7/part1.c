#include "intcode.c"

void part1(const struct intcode_program *program) {
    intcode max_signal = 0;
    for (int perm_index = 0; perm_index < PERM_COUNT; perm_index++) {
      intcode signal = 0;
      for (int thruster = 0; thruster < THRUSTER_COUNT; thruster++) {
        intcode input[2] = {permutations[perm_index][thruster], signal};
        run_program(program, 2, input, 1, &signal);
      }

      if (signal > max_signal) {
        max_signal = signal;
      }
    }

    printf("Part1: %ld\n", max_signal);
  }

  int main(void) {
    struct intcode_program program;
    read_input(&program);
    part1(&program);

    return 0;
  }
