CFLAGS = -Wall -Wextra -Werror -g -Og -I ../common
LDFLAGS = $(CFLAGS) -lm

all: main

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

main: main.o
	$(CC) $(LDFLAGS) -o $@ $^

part1: part1.o
	$(CC) $(LDFLAGS) -o $@ $^

part2: part2.o
	$(CC) $(LDFLAGS) -o $@ $^

run: main
	./$<

run1: part1
	./$<

run2: part2
	./$<

.PHONY: run run1 run2

clean:
	$(RM) main part1 part2 *.o
.PHONY: clean

# Project-wide targets

compile_flags.txt:
	@echo $(CFLAGS) >$@

clean-project:
	$(RM) common/*.o
	@for d in day*; do $(MAKE) -C $$d clean; done
.PHONY: clean-project

-include deps.mk
