import functools
from functools import reduce
import networkx as nx
from random import randrange

cos= {
    0: 1,
    90: 0,
    180: -1,
    270: 0
}
sin = {
    0: 0,
    90: 1,
    180: 0,
    270: -1
}

def minus(x,y):
    return (x[0] - y[0], x[1] - y[1], x[2] - y[2])

def mult(X, Y):
    result = [[0,0,0],
         [0,0,0],
         [0,0,0]]
    for i in range(len(X)):
        for j in range(len(Y[0])):
            for k in range(len(Y)):
                result[i][j] += X[i][k] * Y[k][j]
    return tuple([tuple(m) for m in result])


def apply(X, Y):
    result = [0,0,0]
    for i in range(len(X)):
        for j in range(len(Y)):
                result[i] += X[i][j] * Y[j]
    return tuple(result)


def find_rotations():
    def Rx(phi):
        return ((1, 0, 0),
         (0, cos[phi], -sin[phi]),
         (0, sin[phi], cos[phi])
         )
    def Ry(phi):
        return ((cos[phi], 0, sin[phi]),
         (0, 1, 0),
         (- sin[phi],0, cos[phi])
         )
    def Rz(phi):
        return ((cos[phi], -sin[phi], 0),
         (sin[phi], cos[phi], 0),
         (0, 0, 1)
         )
    res = []
    for phi1 in [0, 90, 180, 270]:
        for phi2 in [0, 90, 180, 270]:
            for phi3 in [0, 90, 180, 270]:
                res.append(mult(mult(Rx(phi1), Ry(phi2)), Rz(phi3)))

    return list(set(res))

rotations = find_rotations()


def multiset(l):
    res = {}
    for m in l:
        if m in res:
            res[m] = res[m] + 1
        else:
            res[m] = 1
    return res

def intersect(set1, set2):
    s = sum([ set1[x] + set2[x] for x in (set1.keys() & set2.keys())  ])
    return s if s is not None else 0

def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res

def parse_lines(lines):
    res = []
    current_block = {}
    for line in lines:
        if line.startswith("---"):
            # block start
            current_block = {}
        elif len(line) == 0:
            # block end
            res.append(current_block)
            current_block = {}
        else:
            current_block[tuple([ int(i) for i in line.split(",")])] = 1
    return res

def dist(p,q):
    return abs(p[0] - q[0]) + abs(p[1] - q[1]) + abs(p[2] - q[2])

def annotate_diffs(scanner):
    points = scanner.keys()
    return { p:multiset([dist(p, q) for q in points])  for p in points}

def check_mappings(scanner1, p1, scanner2, p2):
    for rot in rotations:
        transformed = {}
        translation = minus(apply(rot, p2), p1)

        for q in scanner2.keys():
            new_q = minus(apply(rot, q), translation)
            transformed[new_q] = scanner2[q]
        if len(scanner1.keys() & transformed.keys()) >= 11:
            # must keep the old "distances" so that the 12 points still make sense.
            return {"beacons": transformed, "scanner": minus((0,0,0), translation)}


# all_scanners = parse_lines(read_file("sample.txt"))
all_scanners = parse_lines(read_file("input.txt"))


def find_union(scanner0, scanner1):
    s_0 = annotate_diffs(scanner0)
    s_1 = annotate_diffs(scanner1)
    for p_0 in s_0.keys():
        for p_1 in s_1.keys():
            intersection_count = intersect(s_0[p_0], s_1[p_1])
            if intersection_count >= 12:
                transformed = check_mappings(s_0, p_0, s_1, p_1)
                if not transformed is None:
                    return transformed


def find_from_unions(unions, new_scanner):
    for sc in unions:
        transformed = find_union(sc, new_scanner)
        if not transformed is None:
            return transformed

origin_scanner = all_scanners[0]
scanners = [*all_scanners[1:]]

scanner_coords = []
print(len(scanners))
current_union = [origin_scanner]
while len(scanners) > 0:
    i = randrange(len(scanners))
    transformed = find_from_unions(current_union, scanners[i])
    if not transformed is None:
        current_union.append(transformed["beacons"])
        scanner_coords.append(transformed["scanner"])

        del scanners[i]

res = set()
for scanner in current_union:
    for x in scanner.keys():
        res.add(x)

print(f"There are {len(res)} beacons")

m = 0
for a in scanner_coords:
    for b in scanner_coords:
        n = dist(a, b)
        if n > m:
            m = n

print(f"The scanners are max {m} apart")
