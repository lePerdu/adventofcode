#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct range {
  unsigned low;
  unsigned high;
};

#define MIN_RANGE_VALUE 100000
#define MAX_RANGE_VALUE 999999

bool passcode_in_range(unsigned passcode) {
  return MIN_RANGE_VALUE <= passcode && passcode <= MAX_RANGE_VALUE;
}

int parse_input(struct range *input, const char *buf) {
  char *endptr = NULL;
  input->low = strtoul(buf, &endptr, 10);
  if (buf == endptr) {
    return -1;
  }
  if (!passcode_in_range(input->low)) {
    return -1;
  }

  if (*endptr != '-') {
    return -1;
  }
  buf = endptr + 1;

  input->high = strtoul(buf, &endptr, 10);
  if (buf == endptr) {
    return -1;
  }
  if (!passcode_in_range(input->high)) {
    return -1;
  }

  return 0;
}

void read_input(struct range *input) {
  char *buf = NULL;
  size_t buf_len;
  if (getline(&buf, &buf_len, stdin) < 0) {
    perror("Failed to read input");
    exit(EXIT_FAILURE);
  }

  if (parse_input(input, buf) < 0) {
    fprintf(stderr, "Failed to parse input");
    exit(EXIT_FAILURE);
  }

  free(buf);
}

bool has_doubled_digits(unsigned passcode) {
  unsigned char digit1 = passcode % 10;
  unsigned char digit2;
  passcode /= 10;
  while (passcode > 0) {
    digit2 = digit1;
    digit1 = passcode % 10;
    passcode /= 10;

    if (digit1 == digit2) {
      return true;
    }
  }

  return false;
}

bool digits_never_decrease(unsigned passcode) {
  unsigned char digit1 = passcode % 10;
  unsigned char digit2;
  passcode /= 10;
  while (passcode > 0) {
    digit2 = digit1;
    digit1 = passcode % 10;
    passcode /= 10;

    if (digit1 > digit2) {
      return false;
    }
  }

  return true;
}

bool has_exactly_doubled_digits(unsigned passcode) {
  unsigned char group_digit = passcode % 10;
  passcode /= 10;
  unsigned char repeat_count = 1;
  while (passcode > 0) {
    unsigned digit = passcode % 10;
    passcode /= 10;

    if (digit == group_digit) {
      ++repeat_count;
    } else {
      if (repeat_count == 2) {
        return true;
      }

      group_digit = digit;
      repeat_count = 1;
    }
  }

  // Check for the final repetition
  return repeat_count == 2;
}

bool passcode_matches1(unsigned passcode) {
  return has_doubled_digits(passcode) && digits_never_decrease(passcode);
}

void part1(const struct range *input) {
  unsigned count = 0;
  for (unsigned passcode = input->low; passcode <= input->high; passcode++) {
    if (passcode_matches1(passcode)) {
      count++;
    }
  }

  printf("Part1: %u\n", count);
}

bool passcode_matches2(unsigned passcode) {
  return has_exactly_doubled_digits(passcode) &&
         digits_never_decrease(passcode);
}

void part2(const struct range *input) {
  unsigned count = 0;
  for (unsigned passcode = input->low; passcode <= input->high; passcode++) {
    if (passcode_matches2(passcode)) {
      count++;
    }
  }

  printf("Part2: %u\n", count);
}

int main(void) {
  struct range input;
  read_input(&input);
  part1(&input);
  part2(&input);
  return 0;
}
