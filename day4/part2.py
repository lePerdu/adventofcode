from part1 import Range, read_input


def overlaps(a: Range, b: Range) -> bool:
    return not (b[1] < a[0] or a[1] < b[0])


def main():
    count = 0
    for ra, rb in read_input():
        if overlaps(ra, rb):
            count += 1
    print(count)


if __name__ == '__main__':
    main()
