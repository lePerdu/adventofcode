from pathlib import PurePath
import sys
import typing

from dataclasses import dataclass


@dataclass
class RawCommand:
    command: str
    output: list[str]


@dataclass
class CdCommand:
    path: str

    def apply(self, cwd: PurePath) -> PurePath:
        match self.path:
            case '..': return cwd.parent
            case '/': return PurePath('/')
            case _: return cwd.joinpath(self.path)


@dataclass
class DirItem:
    name: str


@dataclass
class FileItem(DirItem):
    size: int


@dataclass
class LsCommand:
    items: list[DirItem]


Command = CdCommand | LsCommand


def read_raw_input() -> typing.Iterator[RawCommand]:
    command: typing.Optional[str] = None
    output: list[str] = []

    def end_command() -> typing.Iterator[RawCommand]:
        if command is not None:
            yield RawCommand(command, output)

    for line in sys.stdin:
        if line.startswith('$'):
            yield from end_command()
            # Reset to new command
            command = line[1:].strip()
            output = []
        else:
            output.append(line.strip())

    yield from end_command()


def read_input() -> typing.Iterator[Command]:
    return (parse_command(c) for c in read_raw_input())


def parse_ls_item(item: str) -> DirItem:
    type_or_size, name = item.split(maxsplit=1)
    if type_or_size == 'dir':
        return DirItem(name)
    else:
        return FileItem(name, int(type_or_size))


def parse_command(raw: RawCommand) -> Command:
    func, *args = raw.command.split()
    match func:
        case 'cd':
            assert len(args) == 1, f'Too many arguments to `cd`: {args}'
            return CdCommand(args[0])
        case 'ls':
            assert len(args) == 0, f'Too many arguments to `ls`: {args}'
            return LsCommand([parse_ls_item(i) for i in raw.output])
        case _:
            raise Exception(f'Invalid command: {raw.command}')


Listings = dict[PurePath, list[DirItem]]
Sizes = dict[PurePath, int]


def compute_listings(commands: typing.Iterator[Command]) -> Listings:
    listings: Listings = {}
    cwd = PurePath('/')
    for c in commands:
        match c:
            case CdCommand():
                cwd = c.apply(cwd)
            case LsCommand(items):
                listings[cwd] = items

    return listings


def compute_sizes(listings: Listings) -> Sizes:
    sizes: Sizes = {}

    def go(path: PurePath) -> int:
        """Compute sizes, memoized"""
        if path in sizes:
            return sizes[path]

        size = 0
        for item in listings[path]:
            if isinstance(item, FileItem):
                size += item.size
            else:
                # Recurse
                size += go(path.joinpath(item.name))

        sizes[path] = size
        return size

    go(PurePath('/'))
    return sizes


def process_input() -> Sizes:
    listings = compute_listings(read_input())
    for d, s in listings.items():
        print(d, s)

    sizes = compute_sizes(listings)

    print()
    for d, s in sizes.items():
        print(d, s)

    return sizes


def main():
    sizes = process_input()
    print(sum(s for s in sizes.values() if s <= 100_000))


if __name__ == '__main__':
    main()
