import sys
import typing

def parse_digit(c: str) -> int:
    match c:
        case '=': return -2
        case '-': return -1
        case '0': return 0
        case '1': return 1
        case '2': return 2
    raise ValueError(f'Invalid SNAFU digit: `{c}`')


def parse_snafu(s: str) -> int:
    res = 0
    for c in s.strip():
        d = parse_digit(c)
        res = 5*res + d
    return res


def mod_snafu(n: int) -> int:
    match n % 5:
        case 0: return 0
        case 1: return 1
        case 2: return 2
        case 3: return -2
        case 4: return -1
        case _: raise 'Impossible'


def to_snafu_digit(d: int) -> str:
    match d:
        case -2: return '='
        case -1: return '-'
        case 0: return '0'
        case 1: return '1'
        case 2: return '2'
        case _: raise ValueError(f'Invalid SNAFU digit')


def to_snafu_str(n: int) -> str:
    if n == 0:
        return '0'

    result = ''
    while n != 0:
        digit = mod_snafu(n)
        assert (n - digit) % 5 == 0
        n = (n - digit) // 5
        result = to_snafu_digit(digit) + result
    return result


def read_input() -> typing.Iterator[str]:
    return sys.stdin.readlines()


def main():
    fuel_reqs = read_input()
    fuel_total = sum(parse_snafu(s) for s in fuel_reqs)
    fuel_total_snafu = to_snafu_str(fuel_total)
    print(f'Total fuel: {fuel_total_snafu} ({fuel_total})')


if __name__ == '__main__':
    main()
