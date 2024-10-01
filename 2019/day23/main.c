#include <stdbool.h>
#include <stdio.h>

#include "common.h"
#include "intcode.h"

#define MACHINE_COUNT 50
#define NAT_ADDRESS 255
#define PACKET_BUF_SIZE 100

struct packet {
  intcode x;
  intcode y;
};

struct packet_queue {
  unsigned head;
  unsigned size;
  struct packet data[PACKET_BUF_SIZE];
};

void pq_init(struct packet_queue *q) {
  q->head = 0;
  q->size = 0;
}

void pq_push(struct packet_queue *q, struct packet p) {
  if (q->size >= PACKET_BUF_SIZE) {
    die("Packet queue full\n");
  }

  unsigned index = (q->head + q->size) % PACKET_BUF_SIZE;
  q->data[index] = p;
  q->size++;
}

struct packet pq_pop(struct packet_queue *q) {
  if (q->size == 0) {
    die("Packet queue empty\n");
  }

  struct packet p = q->data[q->head];
  q->head = (q->head + 1) % PACKET_BUF_SIZE;
  q->size--;
  return p;
}

bool pq_empty(const struct packet_queue *q) { return q->size == 0; }

void part1(const struct intcode_program *input) {
  // TODO Alloc machines with queues?
  struct machine *machines = malloc(sizeof(machines[0]) * MACHINE_COUNT);
  struct packet_queue *queues = malloc(sizeof(queues[0]) * MACHINE_COUNT);
  for (unsigned i = 0; i < MACHINE_COUNT; i++) {
    struct machine *m = &machines[i];
    machine_init(m, input);
    // Input network address
    machine_input_one(m, i);

    pq_init(&queues[i]);
  }

  bool found_target = false;
  struct packet target_packet;

  while (true) {
    for (unsigned i = 0; i < MACHINE_COUNT; i++) {
      struct machine *m = &machines[i];
      machine_run_til_wait(m);

      while (m->output_buf.size >= 3) {
        intcode out_buf[3];
        unsigned n_output = machine_output(m, out_buf, 3);
        if (n_output != 3) {
          die("Machine only has %u output instructions\n", n_output);
        }

        intcode address = out_buf[0];
        struct packet p = {.x = out_buf[1], .y = out_buf[2]};
        debugf("%d: send [%ld, x=%ld, y=%ld]\n", i, address, p.x, p.y);

        if (address == NAT_ADDRESS) {
          target_packet = p;
          found_target = true;
          goto LOOP_END;
        } else if (address < 0 || MACHINE_COUNT <= address) {
          die("Invalid address: %ld\n", address);
        }

        pq_push(&queues[address], p);
      }

      if (m->state == WAIT_INPUT) {
        struct packet_queue *q = &queues[i];
        if (pq_empty(q)) {
          machine_input_one(m, -1);
        } else {
          struct packet p = pq_pop(q);
          debugf("%d: recv [x=%ld, y=%ld]\n", i, p.x, p.y);
          machine_input_one(m, p.x);
          machine_input_one(m, p.y);
        }
      }
    }
  }
LOOP_END:
  free(queues);
  free(machines);

  if (found_target) {
    printf("Part1: %ld\n", target_packet.y);
  } else {
    die("Did not find packet for address: %d\n", NAT_ADDRESS);
  }
}

void part2(const struct intcode_program *input) {
  // TODO Alloc machines with queues?
  struct machine *machines = malloc(sizeof(machines[0]) * MACHINE_COUNT);
  struct packet_queue *queues = malloc(sizeof(queues[0]) * MACHINE_COUNT);
  for (unsigned i = 0; i < MACHINE_COUNT; i++) {
    struct machine *m = &machines[i];
    machine_init(m, input);
    // Input network address
    machine_input_one(m, i);

    pq_init(&queues[i]);
  }

  struct packet prev_nat_packet = {-1, -1};
  bool nat_has_packet = false;
  struct packet nat_packet = {-1, -1};

  while (true) {
    // Set to false if at least 1 machine sends or receives a packet.
    bool network_idle = true;

    for (unsigned i = 0; i < MACHINE_COUNT; i++) {
      struct machine *m = &machines[i];
      machine_run_til_wait(m);
      // debugf("%d: stopped with state: %s\n", i, machine_state_str(m->state));

      while (m->output_buf.size >= 3) {
        network_idle = false;
        intcode out_buf[3];
        unsigned n_output = machine_output(m, out_buf, 3);
        if (n_output != 3) {
          die("Machine only has %u output instructions\n", n_output);
        }

        intcode address = out_buf[0];
        struct packet p = {.x = out_buf[1], .y = out_buf[2]};
        debugf("%d: send [%ld, x=%ld, y=%ld]\n", i, address, p.x, p.y);

        if (address == NAT_ADDRESS) {
          nat_packet = p;
          nat_has_packet = true;
        } else if (0 <= address && address < MACHINE_COUNT) {
          pq_push(&queues[address], p);
        } else {
          die("Invalid address: %ld\n", address);
        }
      }

      if (m->state == WAIT_INPUT) {
        struct packet_queue *q = &queues[i];
        if (pq_empty(q)) {
          machine_input_one(m, -1);
        } else {
          network_idle = false;
          struct packet p = pq_pop(q);
          debugf("%d: recv [x=%ld, y=%ld]\n", i, p.x, p.y);
          machine_input_one(m, p.x);
          machine_input_one(m, p.y);
        }
      }
    }

    if (network_idle && nat_has_packet) {
      if (nat_packet.x == prev_nat_packet.x && nat_packet.y == prev_nat_packet.y) {
        // Stop once the y value is repeatedly sent
        break;
      }
      pq_push(&queues[0], nat_packet);
      prev_nat_packet = nat_packet;
    } else if (network_idle) {
      printf("Network idle without NAT packet\n");
    }
  }

  free(queues);
  free(machines);

  intcode repeated_y = nat_packet.y;
  printf("Part2: %ld\n", repeated_y);
}

int main(void) {
  struct intcode_program input;
  FILE *input_file = fopen("input.txt", "r");
  read_input_from_file(&input, input_file);
  fclose(input_file);

  part1(&input);
  part2(&input);
  return 0;
}
