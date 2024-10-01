#include <stdio.h>
#include <stdlib.h>

unsigned int compute_fuel(unsigned int mass) {
  unsigned int total_fuel = mass / 3 - 2;
  unsigned int iter_fuel = total_fuel;
  while (iter_fuel >= 6) {
    iter_fuel = iter_fuel / 3 - 2;
    total_fuel += iter_fuel;
  }
  return total_fuel;
}

void die(char *msg) {
  perror(msg);
  exit(1);
}

int main(void) {
  unsigned int sum = 0;
  while (1) {
    unsigned int mass;
    if (scanf("%u\n", &mass) < 1) {
      break;
    }

    sum += compute_fuel(mass);
  }

  printf("Part1: %u\n", sum);

  return 0;
}
