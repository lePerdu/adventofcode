import itertools
import typing

import functools
from part1 import Packet, compare, read_input


DIVIDER_PACKETS: list[Packet] = [[[2]], [[6]]]


def read_input_joined() -> typing.Iterator[Packet]:
    input_packets = itertools.chain.from_iterable(read_input())
    return itertools.chain(DIVIDER_PACKETS, input_packets)


def main():
    packets = read_input_joined()
    ordered_packets = sorted(packets, key=functools.cmp_to_key(compare))
    for p in ordered_packets:
        print(p)

    divider_indices = [ordered_packets.index(p)+1 for p in DIVIDER_PACKETS]
    decoder_key = divider_indices[0] * divider_indices[1]
    print(decoder_key)


if __name__ == '__main__':
    main()
