import itertools
import sys
import typing

from part1 import Elf, read_input


def main():
    """Find total calories of top 3 elves"""

    all_sorted = sorted(
        (sum(elf) for elf in read_input(sys.stdin)),
        reverse=True,
    )
    print(sum(itertools.islice(all_sorted, 3)))


if __name__ == '__main__':
    main()
