import functools
from functools import reduce


def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res


def to_int_line(line):
    return [int(n) for n in line]


def get_candidates(lines, m, n):
    candidates = [ (m+dx, n+dy) for dx in [-1, 0, 1] for dy in [-1, 0, 1]]
    max_m = len(lines)
    max_n = len(lines[0])
    return [(x,y) for (x, y) in candidates if x >=0 and x < max_m and y >= 0 and y < max_n ]


def pretty_print(board):
    for line in board:
        print("".join([str(n) for n in line]))


def do_step(lines):
    seen = {}
    flash_count = 0

    def illuminate(m, n):
        nonlocal flash_count
        nonlocal seen

        if (m,n) in seen:
            return

        lines[m][n] = lines[m][n] + 1

        if lines[m][n] > 9:
            flash_count = flash_count + 1
            lines[m][n] = 0
            seen[(m,n)] = True

            candidates = get_candidates(lines, m, n)
            [illuminate(x,y) for (x,y) in candidates]

    for n in range(len(lines[0])):
         for m in range(len(lines)):
            lines[m][n] = lines[m][n] + 1

    for n in range(len(lines[0])):
         for m in range(len(lines)):
            if lines[m][n] > 9:
                illuminate(m, n)

    return (lines, flash_count, len(seen.keys()))



# max_m = 4 max_n = 9

raw_lines = [
"5483143223",
"2745854711",
"5264556173",
"6141336146",
"6357385478",
"4167524645",
"2176841721",
"6882881134",
"4846848554",
"5283751526"
]

raw_lines = read_file('input.txt')

lines = [ to_int_line(raw_line) for raw_line in raw_lines]

total_flash_count = 0

print(f"STEP 0")
pretty_print(lines)

for step in range(1, 900):
    (new_lines, new_flash_count, seen) = do_step(lines)
    total_flash_count = total_flash_count + new_flash_count
    lines = new_lines

    if len(lines) * len(lines[0]) == seen:
        print(step)
        break


print(f"total flashes {total_flash_count}")
