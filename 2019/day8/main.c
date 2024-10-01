#include <ctype.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define perror_die(msg)                                                        \
  {                                                                            \
    perror(msg);                                                               \
    exit(EXIT_FAILURE);                                                        \
  }

#define die(...)                                                               \
  {                                                                            \
    fprintf(stderr, __VA_ARGS__);                                              \
    exit(EXIT_FAILURE);                                                        \
  }

#define MAX_IMAGE_SIZE 15000

struct int_list {
  size_t size;
  uint8_t data[MAX_IMAGE_SIZE];
};

void read_input(struct int_list *input) {
  char *buf = NULL;
  size_t line_size;
  if (getline(&buf, &line_size, stdin) < 0) {
    perror_die("Failed to read input");
  }

  for (unsigned i = 0; i < line_size; i++) {
    char c = buf[i];
    if (isspace(c)) {
      break;
    }

    if (isdigit(c)) {
      input->data[input->size++] = c - '0';
    } else {
      die("Invalid input char: %c\n", c);
    }
  }

  free(buf);
}

#define IMAGE_ROWS 6
#define IMAGE_COLS 25
#define LAYER_SIZE (IMAGE_ROWS * IMAGE_COLS)

void part1(const struct int_list *input) {
  unsigned layer_count = input->size / LAYER_SIZE;

  unsigned fewest_0s = LAYER_SIZE + 1;
  unsigned ans = 0;
  for (unsigned layer = 0; layer < layer_count; layer++) {
    unsigned count_0s = 0;
    unsigned count_1s = 0;
    unsigned count_2s = 0;
    for (unsigned i = 0; i < LAYER_SIZE; i++) {
      switch (input->data[layer * LAYER_SIZE + i]) {
      case 0:
        count_0s++;
        break;
      case 1:
        count_1s++;
        break;
      case 2:
        count_2s++;
        break;
      }
    }

    if (count_0s < fewest_0s) {
      ans = count_1s * count_2s;
      fewest_0s = count_0s;
    }
  }

  printf("Part1: %u\n", ans);
}

enum pixel {
  PX_BLACK = 0,
  PX_WHITE = 1,
  PX_TRANS = 2,
};

void print_image(const uint8_t *image) {
  putchar('+');
  for (unsigned i = 0; i < IMAGE_COLS; i++) {
    putchar('-');
  }
  putchar('+');
  putchar('\n');

  for (unsigned row = 0; row < IMAGE_ROWS; row++) {
    putchar('|');
    for (unsigned col = 0; col < IMAGE_COLS; col++) {
      char c;
      switch (image[row * IMAGE_COLS + col]) {
      case PX_BLACK:
        c = ' ';
        break;
      case PX_WHITE:
        c = '#';
        break;
      case PX_TRANS:
        c = '.';
        break;
      default:
        c = '?';
        break;
      }
      putchar(c);
    }
    putchar('|');
    putchar('\n');
  }

  putchar('+');
  for (unsigned i = 0; i < IMAGE_COLS; i++) {
    putchar('-');
  }
  putchar('+');
  putchar('\n');
}

void part2(const struct int_list *input) {
  unsigned layer_count = input->size / LAYER_SIZE;

  uint8_t final_image[LAYER_SIZE];
  memcpy(final_image, input->data, LAYER_SIZE);
  for (unsigned layer = 1; layer < layer_count; layer++) {
    for (unsigned pixel = 0; pixel < LAYER_SIZE; pixel++) {
      if (final_image[pixel] == 2) {
        final_image[pixel] = input->data[layer * LAYER_SIZE + pixel];
      }
    }
  }

  puts("Part2:");
  print_image(final_image);
}

int main(void) {
  static struct int_list input;
  read_input(&input);

  if (input.size % LAYER_SIZE != 0) {
    die("Incompatible image size %lu for layer size %d\n", input.size,
        LAYER_SIZE);
  }

  part1(&input);
  part2(&input);

  return 0;
}
