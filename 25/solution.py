def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res


def parse_lines(lines):
    downs = set()
    rights = set()
    empties = set()
    for x in range(len(lines[0])):
        for y in range(len(lines)):
            if lines[y][x] == ">":
                rights.add((x, y))
            if lines[y][x] == "v":
                downs.add((x, y))
            if lines[y][x] == ".":
                empties.add((x, y))
    return {
        "h": len(lines),
        "w": len(lines[0]),
        "downs": downs,
        "rights": rights,
        "empties": empties,
    }


def possible_right_moves(board):
    next_steps = {((x+1) % board["w"], y): (x, y)
                  for x, y in board["rights"]}
    realizible = next_steps.keys() & board["empties"]
    return {k: next_steps[k] for k in realizible}


def do_right_moves(board, moves):
    source = set(moves.values())
    target = moves.keys()

    return {
        **board,
        "rights": board["rights"] - source | target,
        "empties": board["empties"] - target | source
    }


def possible_down_moves(board):
    next_steps = {(x, (y+1) % board["h"]): (x, y)
                  for x, y in board["downs"]}
    realizible = next_steps.keys() & board["empties"]
    return {k: next_steps[k] for k in realizible}


def do_down_moves(board, moves):
    source = set(moves.values())
    target = moves.keys()

    return {
        **board,
        "downs": board["downs"] - source | target,
        "empties": board["empties"] - target | source
    }


def pretty_print_coord(board, x, y):
    if (x, y) in board["empties"]:
        return "."
    if (x, y) in board["rights"]:
        return ">"
    if (x, y) in board["downs"]:
        return "v"


def pretty_print(board):
    for y in range(board["h"]):
        for x in range(board["w"]):
            print(pretty_print_coord(board, x, y), end='')
        print()


lines = read_file("sample2.txt")
lines = read_file("input.txt")
board = parse_lines(lines)


def find_stationary_point(board):
    count = 1
    while True:
        pr = possible_right_moves(board)
        board = do_right_moves(board, pr)

        pd = possible_down_moves(board)
        board = do_down_moves(board, pd)

        if len(pr) + len(pd) == 0:
            return count
        count = count + 1


c = find_stationary_point(board)
print(f"Stopped after {c} moves")
