import itertools
import sys
import typing

from part1 import get_priority

T = typing.TypeVar('T')


def read_input() -> typing.Generator[str, None, None]:
    yield from (l.strip() for l in sys.stdin)


def split_groups(
    iterable: typing.Iterable[T], n: int
) -> typing.Generator[tuple[T, ...], None, None]:
    it = iter(iterable)
    while True:
        chunk = tuple(itertools.islice(it, n))
        if not chunk:
            return
        yield chunk


def get_shared_items(a: typing.Iterable[str], b: typing.Iterable[str]) -> set[str]:
    return set(c for c in a if c in b)


def get_group_badge(g: tuple[str, str, str]) -> str:
    tmp = get_shared_items(g[0], g[1])
    s = get_shared_items(tmp, g[2])
    return s.pop()


def main():
    print(sum(
        get_priority(get_group_badge(g)) for g in split_groups(
            read_input(),
            3,
        )
    ))


if __name__ == '__main__':
    main()
