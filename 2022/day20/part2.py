from part1 import read_input, get_coords, mix

KEY = 811589153
ROUNDS = 10


def main():
    data = read_input()
    for i in range(len(data)):
        data[i] *= KEY

    indices = list(range(len(data)))

    # print('Initial')
    # print(data)
    # print()

    for round in range(ROUNDS):
        mix(data, indices)
        print(f'Round {round+1}')
        # print(data)
        # print()
    # print(data)
    coords = get_coords(data)
    print(coords)
    print(sum(coords))


if __name__ == '__main__':
    main()
