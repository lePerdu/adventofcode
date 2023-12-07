#include "intcode.c"

void part2(const struct intcode_program *program) {
  static struct machine thrusters[THRUSTER_COUNT];

  intcode max_signal = 0;
  for (int perm_index = 0; perm_index < PERM_COUNT; perm_index++) {
    // Initialize with phases (and run to fill in those phases)
    for (int thruster_index = 0; thruster_index < THRUSTER_COUNT;
         thruster_index++) {
      struct machine *thruster = &thrusters[thruster_index];
      machine_init(thruster, program);
      // Input offset phase value
      machine_input(thruster, permutations[perm_index][thruster_index] + 5);
      machine_run_til_wait(thruster);
    }

    intcode signal = 0;
    while (1) {
      bool any_halted = false;
      bool all_halted = true;

      for (int thruster_index = 0; thruster_index < THRUSTER_COUNT;
           thruster_index++) {
        struct machine *thruster = &thrusters[thruster_index];
        machine_input(thruster, signal);
        enum machine_state state = machine_run_til_wait(thruster);
        switch (state) {
        case WAIT_INPUT:
          // Expected state
          all_halted = false;
          break;
        case WAIT_OUTPUT:
          fprintf(stderr, "Unexpected pause state: WAIT_OUTPUT\n");
          exit(EXIT_FAILURE);
          break;
        case RUNNING:
          fprintf(stderr, "Unexpected pause state: WAIT_RUNNING\n");
          exit(EXIT_FAILURE);
          break;

        case HALTED:
          any_halted = true;
          break;
        }

        if (!machine_has_output(thruster)) {
          fprintf(stderr, "Thruster %d did not output", thruster_index);
          exit(EXIT_FAILURE);
        }

        signal = machine_output(thruster);
      }

      if (all_halted) {
        break;
      }

      if (any_halted) {
        fprintf(stderr, "Not all halted at the same iteration\n");
        exit(EXIT_FAILURE);
      }
    }

    if (signal > max_signal) {
      max_signal = signal;
    }
  }

  printf("Part2: %ld\n", max_signal);
}

int main(void) {
  struct intcode_program program;
  read_input(&program);
  part2(&program);

  return 0;
}
