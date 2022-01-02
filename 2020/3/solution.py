def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res


def parse_lines(lines):
    trees = set()
    for x in range(len(lines[0])):
        for y in range(len(lines)):
            if lines[y][x] == "#":
                trees.add((x, y))

    return {
        "h": len(lines),
        "w": len(lines[0]),
        "trees": trees,
    }


board = parse_lines(read_file("input.txt"))
# board = parse_lines(read_file("sample.txt"))


def positions(board, dx, dy):
    steps = int(board["h"]/dy)
    return set([(dx*y % board["w"], y*dy) for y in range(0, steps)])


res = 1
for dx, dy in [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]:
    trees = len(positions(board, dx, dy) & board["trees"])
    print(f"{dx, dy}: {trees}")
    res = res * trees

print(f"solution: {res}")
