import sys
import typing


def get_priority(c: str) -> int:
    if c.islower():
        return ord(c) - ord('a') + 1
    elif c.isupper():
        return ord(c) - ord('A') + 27
    else:
        raise Exception(f'Invalid item: `{c}`')


def get_shared_items(left: str, right: str) -> str:
    return ''.join(
        c for c in left if c in right
    )


def read_input() -> typing.Generator[tuple[str, str], None, None]:
    for line in sys.stdin:
        mid = len(line) // 2
        yield (line[:mid], line[mid:])


def main():
    print(sum(get_priority(get_shared_items(
        l, r)[0]) for l, r in read_input()))


if __name__ == '__main__':
    main()
