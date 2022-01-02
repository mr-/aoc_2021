import functools
from functools import reduce
import networkx as nx
from random import randrange

def read_file(filename):
    file1 = open(filename, 'r')
    return file1.read()


def to_num(x):
    return 1 if x == '#' else 0


def parse_file(content):
    algo, picture = content.split("\n\n")
    lines = [x for x in picture.split("\n") if len(x) > 1]
    res = {}
    for y in range(len(lines)):
        for x in range(len(lines[0])):
            res[(x,y)] = to_num(lines[y][x])

    return (algo, {"lines":res, "outside": 0})


def pretty_print(lines):
    res = ""
    min_x = min([x[0] for x in lines.keys()])
    min_y = min([x[1] for x in lines.keys()])

    max_x = max([x[0] for x in lines.keys()])
    max_y = max([x[1] for x in lines.keys()])


    for y in range(min_y, max_y+1):
        for x in range(min_x, max_x+1):
            print('#' if lines[(x,y)] == 1 else '.', end='')
        print()


def get_index(board, x, y):
    lines = board["lines"]
    outside = board["outside"]
    cs = [(x-1, y-1), (x, y-1), (x+1, y-1),
          (x-1, y),   (x,y),    (x+1, y),
          (x-1, y+1), (x, y+1), (x+1, y+1)
          ]
    return int(''.join([str(lines[c]) if c in lines else str(outside) for c in cs]), 2)


def get_all_points(board):
    lines = board["lines"]
    min_x = min([x[0] for x in lines.keys()])
    min_y = min([x[1] for x in lines.keys()])

    max_x = max([x[0] for x in lines.keys()])
    max_y = max([x[1] for x in lines.keys()])
    return [ (x,y) for x in range(min_x-1, max_x+2) for y in range(min_y-1, max_y+2)]


def apply_algo(algo, board, x, y):
    i = get_index(board, x,y)
    res = algo[i]
    return to_num(res)


def advance(algo, board):
    lines = {}
    for p in get_all_points(board):
        lines[p] = apply_algo(algo, board, p[0], p[1])
    new_outside = to_num(algo[9*board["outside"]])
    return {"lines": lines, "outside": new_outside}



# content = read_file("sample.txt")
content = read_file("input.txt")
algo, board = parse_file(content)

it = board
for i in range(50):
    if(i%5 == 0):
        print(i)
    it = advance(algo, it)

print(sum(it["lines"].values()))


# 2  runs:  5081
# 50 runs: 15088
