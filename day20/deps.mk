main.o: ../common/common.h input.h
input.o: input.h
part1.o: input.h
part2.o: input.h

main: input.o part1.o part2.o
