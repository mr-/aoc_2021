import functools
from functools import reduce

def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res


def parse_line(line):
    points = line.split("|")
    left = [frozenset(sorted(list(n.strip()))) for n in  points[0].strip().split(" ")]
    right = [frozenset(sorted(list(n.strip()))) for n in  points[1].strip().split(" ")]
    return (left, right)

# be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe

def get_candidates(lines, m, n):
    candidates = [(m + 1, n), (m -1, n), (m, n + 1), (m, n -1)]
    max_m = len(lines)
    max_n = len(lines[0])
    return [(x,y) for (x, y) in candidates if x >=0 and x < max_m and y >=0 and y < max_n ]



def is_min(lines, m, n):
    to_consider = get_candidates(lines, m, n)

    really_min = int(lines[m][n]) < min(int(lines[x][y]) for (x,y) in to_consider)

    if really_min:
        return (m, n)

def closure(lines, m, n):
    to_consider = get_candidates(lines, m, n)
    possibilities = [int(lines[x][y]) for (x,y) in to_consider]

    val = int(lines[m][n])

    to_do = [(x,y) for (x,y) in to_consider if int(lines[x][y]) > val and int(lines[x][y]) < 9]
    return set([(m,n)]) | set([ element for (x,y) in to_do for element in closure(lines, x, y)])


# max_m = 4 max_n = 9

lines = [
    "2199943210",
    "3987894921",
    "9856789892",
    "8767896789",
    "9899965678"
]

lines = read_file('input.txt')

weird_mins = [is_min(lines, m, n) for n in range(len(lines[0])) for m in range(len(lines))]
mins = [ x for x in weird_mins if x is not None]

closures = list(reversed(sorted([closure(lines, m, n) for (m,n) in mins], key=len)))[:3]

print(reduce((lambda x, y: x*y), ([len(c) for c in closures])))
