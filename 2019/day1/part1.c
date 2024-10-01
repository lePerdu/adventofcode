#include <stdio.h>
#include <stdlib.h>

unsigned int compute_fuel(unsigned int mass) { return mass / 3 - 2; }

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
