import typing


def all_different(data: str) -> bool:
    l = len(data)
    for i in range(l):
        for j in range(i+1, l):
            if data[i] == data[j]:
                return False
    return True


def find_start_marker(
    data: str, length: int = 4
) -> typing.Optional[tuple[str, int]]:
    for i in range(len(data)):
        sub = data[i:i+length]
        if all_different(sub):
            return sub, i+length
    return None


read_input = input


def main():
    print(find_start_marker(
        read_input(),
        14,  # 4 for part 1, 14 for part 2
    ))


if __name__ == '__main__':
    main()
