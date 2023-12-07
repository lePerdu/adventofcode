import typing

Packet = int | list['Packet']

PacketPair = tuple[Packet, Packet]


def read_packet() -> Packet:
    return eval(input())


def read_input() -> typing.Iterator[PacketPair]:
    while True:
        try:
            yield read_packet(), read_packet()
            assert len(input().strip()) == 0, "Excepted blank line"
        except EOFError:
            return


def list_compare(left: list[Packet], right: list[Packet]) -> int:
    for l, r in zip(left, right):
        cmp = compare(l, r)
        if cmp != 0:
            return cmp

    # Based on length of list if all else equal
    return len(left) - len(right)


def compare(left: Packet, right: Packet) -> int:
    match left, right:
        case int(l), int(r):
            return l - r
        case list(l), list(r):
            return list_compare(l, r)
        case int(l), list(r):
            return compare([l], r)
        case list(l), int(r):
            return compare(l, [r])
        case _ as unreachable:
            assert False, f'Unreachable: {unreachable}'


def main():
    index_sum = 0
    for index, pair in enumerate(read_input()):
        res = compare(*pair)
        if res < 0:
            index_sum += index + 1
        print(pair, 'Compare: ', res)
    print('Result: ', index_sum)


if __name__ == '__main__':
    main()
