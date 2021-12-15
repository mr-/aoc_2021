import functools
from functools import reduce
import networkx as nx

def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res


def parse_lines(lines):
    res = {}
    for x in range(len(lines[0])):
        for y in range(len(lines)):
            res[(x,y)] = int(lines[y][x])
    return res


def get_candidates(max_x, max_y, x0, y0):
    candidates = [(x0 + 1, y0), (x0 -1, y0), (x0, y0 + 1), (x0, y0 -1)]

    return [(x,y) for (x, y) in candidates if x >=0 and x <= max_x and y >=0 and y <= max_y ]


def extend_grid(grid):
    res = {}
    max_x = max(x for x, y in grid.keys())
    max_y = max(y for x, y in grid.keys())


    for x in range( (max_x+1) * 5):
        for y in range( (max_y+1) * 5):
            block_nr = int(x/(max_x+1)) + int(y/(max_y+1))
            new_val = (grid[(x % (max_x+1), y % (max_y+1))] + block_nr)

            res[(x,y)] = new_val if new_val < 10 else ((new_val % 10)+1)

    return res


def pretty_print(grid):
    max_x = max(x for x, y in grid.keys())
    max_y = max(y for x, y in grid.keys())

    for y in range(max_y+1):
        for x in range(max_x+1):
            print(str(grid[(x,y)]),  end = '')
        print()

lines = [
"1163751742",
"1381373672",
"2136511328",
"3694931569",
"7463417111",
"1319128137",
"1359912421",
"3125421639",
"1293138521",
"2311944581"
]

lines = read_file('input.txt')

G = nx.DiGraph()
grid = parse_lines(lines)
grid = extend_grid(grid)

# pretty_print(grid)

max_x = max(x for x, y in grid.keys())
max_y = max(y for x, y in grid.keys())
for x, y in grid.keys():
        candidates = get_candidates(max_x, max_y, x, y)
        for cx, cy in candidates:
            G.add_edge((x,y), (cx, cy), weight=grid[(cx, cy)])



start = (0,0)
end = (max_x, max_y)
path = nx.shortest_path(G, (0,0), end, weight='weight')

print(sum([grid[(x,y)] for x, y in path[1:]]))
