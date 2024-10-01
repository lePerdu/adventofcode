import sys


def read_input():
    return [int(l) for l in sys.stdin.readlines()]


def shift(data: list[int], indices: list[int], original_index: int):
    current_index = indices.index(original_index)
    # Wrap around 1 earlier since we will have removed 1 element before
    # inserting
    dst_index = (current_index + data[current_index]) % (len(data) - 1)

    def go(l: list[int]):
        value = l.pop(current_index)
        l.insert(dst_index, value)

    go(data)
    go(indices)

    # print(data)


def mix(data: list[int], indices):
    for i in range(len(data)):
        shift(data, indices, i)


def get_coords(data: list[int]):
    zero_index = data.index(0)
    def idx(offset): return (zero_index + offset) % len(data)
    return (data[idx(1000)], data[idx(2000)], data[idx(3000)])


def main():
    data = read_input()
    indices = list(range(len(data)))
    mix(data, indices)
    # print(data)
    coords = get_coords(data)
    print(coords)
    print(sum(coords))


if __name__ == '__main__':
    main()
