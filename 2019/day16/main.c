#include <ctype.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "common.h"

#define MAX_INPUT_SIZE 1024

struct sequence {
  size_t size;
  // Sequence stores positive, single-digit values
  uint8_t data[];
};

struct sequence *seq_alloc(size_t size) {
  struct sequence *s = malloc(sizeof(*s) + sizeof(s->data[0]) * size);
  s->size = size;
  return s;
}

void read_input(struct sequence *input) {
  input->size = 0;

  char *line_buf = NULL;
  size_t line_len;
  if (getline(&line_buf, &line_len, stdin) < 0) {
    perror_die("Failed to read input");
  }

  const char *cursor = line_buf;

  char c;
  while (isdigit(c = *cursor)) {
    if (input->size >= MAX_INPUT_SIZE) {
      die("Too much input\n");
    }

    input->data[input->size++] = c - '0';
    cursor++;
  }

  while (isspace(*cursor)) {
    cursor++;
  }

  if (*cursor != 0) {
    die("Unexpected input character: '%c'\n", *cursor);
  }

  free(line_buf);
}

#define PATTERN_SIZE 4

static const int8_t PATTERN[PATTERN_SIZE] = {1, 0, -1, 0};

uint8_t compute_output_digit(const struct sequence *s, unsigned output_index) {
  long int total = 0;
  for (unsigned i = output_index; i < s->size; i++) {
    unsigned pattern_index =
        ((i - output_index) / (output_index + 1)) % PATTERN_SIZE;
    total += s->data[i] * PATTERN[pattern_index];
  }

  // Extract first digit
  return labs(total) % 10;
}

void apply_fft_phase(struct sequence *output, struct sequence *input) {
  for (unsigned i = 0; i < output->size; i++) {
    uint8_t digit = compute_output_digit(input, i);
    // printf("[%u] = %u\n", i, digit);
    output->data[i] = digit;
  }
}

unsigned extract_n_digits(const struct sequence *s, uint8_t n,
                          unsigned offset) {
  unsigned result = 0;
  for (unsigned i = 0; i < n; i++) {
    result = 10 * result + s->data[offset + i];
  }
  return result;
}

unsigned extract_first_n(const struct sequence *s, uint8_t n) {
  return extract_n_digits(s, n, 0);
}

#define PHASE_COUNT 100

struct sequence *create_transformed_seq(const struct sequence *original_input) {
  // Double-buffer the sequences to avoid copying at each step

  struct sequence *input = seq_alloc(original_input->size);
  memcpy(input->data, original_input->data, original_input->size);

  // Data can be uninitialized since it's the buffer to be filled
  struct sequence *output = seq_alloc(original_input->size);

  for (unsigned i = 1; i <= PHASE_COUNT; i++) {
    apply_fft_phase(output, input);
    printf("Phase %3u: %08u\n", i, extract_first_n(output, 8));

    // Swap buffers
    struct sequence *tmp = input;
    input = output;
    output = tmp;
  }

  // Result is in input buffer since they are swapped in the loop
  free(output);
  return input;
}

void part1(const struct sequence *input) {
  struct sequence *result = create_transformed_seq(input);
  printf("Part1: %08u\n", extract_first_n(result, 8));
  free(result);
}

#define PART2_REPEAT_COUNT 10000

void apply_part2_fft_phase(struct sequence *output,
                           const struct sequence *input,
                           unsigned starting_offset) {
  unsigned running_total = 0;
  for (unsigned i = output->size - 1; i >= starting_offset; i--) {
    running_total += input->data[i];
    output->data[i] = running_total % 10;
  }
}

/**
 * The Part 2 version just turns into summation since the pattern turns into all
 * 1s after the first half of the sequence.
 */
struct sequence *create_transformed_part2(const struct sequence *original_input,
                                          unsigned starting_offset) {
  // Double-buffer the sequences to avoid copying at each step

  struct sequence *input = seq_alloc(original_input->size);
  memcpy(input->data, original_input->data, original_input->size);

  // Data can be uninitialized since it's the buffer to be filled
  struct sequence *output = seq_alloc(original_input->size);

  for (unsigned i = 1; i <= PHASE_COUNT; i++) {
    apply_part2_fft_phase(output, input, starting_offset);
    printf("Phase %3u: %08u\n", i,
           extract_n_digits(output, 8, starting_offset));

    // Swap buffers
    struct sequence *tmp = input;
    input = output;
    output = tmp;
  }

  // Result is in input buffer since they are swapped in the loop
  free(output);
  return input;
}

void part2(const struct sequence *input) {
  unsigned message_offset = extract_first_n(input, 7);

  // Repeat input signal
  struct sequence *repeated_seq = seq_alloc(input->size * PART2_REPEAT_COUNT);
  for (unsigned offset = 0; offset < PART2_REPEAT_COUNT; offset++) {
    memcpy(&repeated_seq->data[offset * input->size], input->data, input->size);
  }

  if (message_offset < repeated_seq->size / 2) {
    die("Offset must be past the midpoint for part 2\n");
  }

  struct sequence *result =
      create_transformed_part2(repeated_seq, message_offset);
  unsigned ans = extract_n_digits(result, 8, message_offset);
  printf("Part2: %08u\n", ans);

  free(result);
  free(repeated_seq);
}

int main(void) {
  struct sequence *input = seq_alloc(MAX_INPUT_SIZE);
  read_input(input);
  part1(input);
  part2(input);

  free(input);
  return 0;
}
