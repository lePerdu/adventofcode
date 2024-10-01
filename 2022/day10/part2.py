import typing

from part1 import Machine, read_input

Pixel = typing.Literal['#', '.']

ROWS = 6
COLS = 40

SPRITE_WIDTH = 1


class Crt:
    cycle = 0

    @property
    def position(self) -> int:
        return (self.cycle - 1) % COLS

    @property
    def at_eol(self) -> bool:
        return self.position == COLS - 1

    def clock_cycle(self, sprite_pos: int) -> Pixel:
        self.cycle += 1

        if abs(self.position - sprite_pos) <= SPRITE_WIDTH:
            return '#'
        else:
            return '.'


def main():
    instrs = read_input()
    machine = Machine()
    crt = Crt()

    for i in instrs:
        for state in machine.exec(i):
            pixel = crt.clock_cycle(state.register)
            end = '\n' if crt.at_eol else ''
            print(pixel, end=end)


if __name__ == '__main__':
    main()
