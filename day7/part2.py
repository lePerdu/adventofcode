from pathlib import PurePath

from part1 import process_input

TOTAL_CAPACITY = 70_000_000
REQUIRED_CAPACITY = 30_000_000


def main():
    sizes = process_input()
    used_size = sizes[PurePath('/')]

    def is_big_enough(size: int) -> bool:
        return TOTAL_CAPACITY - (used_size - size) >= REQUIRED_CAPACITY

    path, size = min(
        ((name, size) for name, size in sizes.items() if is_big_enough(size)),
        key=lambda t: t[1]
    )
    print()
    print(str(path), size)


if __name__ == '__main__':
    main()
