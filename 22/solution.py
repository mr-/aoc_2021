import functools
from functools import reduce
import networkx as nx
from random import randrange


def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res


class Rect():
    def __init__(self, xs, ys, zs):
        self.xs = xs
        self.ys = ys
        self.zs = zs

        self.min_x = min(xs)
        self.min_y = min(ys)
        self.min_z = min(zs)

        self.max_x = max(xs)
        self.max_y = max(ys)
        self.max_z = max(zs)

    def is_left_of(self, r: "Rect"):
        return self.max_x < r.min_x

    def is_right_of(self, r: "Rect"):
        return self.min_x > r.max_x

    def is_below_of(self, r: "Rect"):
        return self.max_y < r.min_y

    def is_above_of(self, r: "Rect"):
        return self.min_y > r.max_y

    def is_infront_of(self, r: "Rect"):
        return self.min_z > r.max_z

    def is_behind_of(self, r: "Rect"):
        return self.max_z < r.min_z

    def vol(self):
        return (1+self.max_x - self.min_x) * (1+self.max_y - self.min_y) * (1+self.max_z - self.min_z)

    def __str__(self):
        return str((self.xs, self.ys, self.zs))


def to_num(x):
    return 1 if x == '#' else 0


def parse_line(line):
    command, coords = line.split(" ")
    xs, ys, zs = coords.split(",")

    def foo(ns):
        return [int(i) for i in ns[2:].split("..")]

    return (command, Rect(foo(xs), foo(ys), foo(zs)))


def parse_file(filename):
    lines = read_file(filename)
    return [parse_line(x) for x in lines if len(x) > 4]


def intersects(r1: Rect, r2: Rect):
    no_intersection = r1.is_above_of(r2) or r1.is_below_of(r2) or r1.is_left_of(r2) or r1.is_right_of(
        r2) or r1.is_infront_of(r2) or r1.is_behind_of(r2)
    return not no_intersection


def intersection(r1: Rect, r2: Rect):
    return Rect(
        [max(r1.min_x, r2.min_x), min(r1.max_x, r2.max_x)],
        [max(r1.min_y, r2.min_y), min(r1.max_y, r2.max_y)],
        [max(r1.min_z, r2.min_z), min(r1.max_z, r2.max_z)],
    )


def maybe_intersect(target, r1):
    if intersects(target, r1[1]):
        return (r1[0], intersection(target, r1[1]))
    return None


def find_count(parsed_lines):
    added = []
    removed = []
    for (command, rect) in parsed_lines:
        if command == "on":
            new_removed = []
            for a in added:
                if intersects(rect, a):
                    new_removed.append(intersection(rect, a))
            for a in removed:
                if intersects(rect, a):
                    added.append(intersection(rect, a))
            removed = removed + new_removed
            added.append(rect)

        if command == "off":
            new_removed = []

            for a in added:
                if intersects(rect, a):
                    new_removed.append(intersection(rect, a))
            for a in removed:
                if intersects(rect, a):
                    added.append(intersection(rect, a))
            removed = removed + new_removed

        # print(
        #     f"added: {[str(x) for x in added]}")
        # print(
        #     f"removed: {[str(x) for x in removed]}")
        # print(sum([x.vol() for x in added]) - sum([x.vol() for x in removed]))
        # print()

    return sum([x.vol() for x in added]) - sum([x.vol() for x in removed])


parsed_lines = parse_file("sample1.txt")
parsed_lines = parse_file("sample2.txt")


parsed_lines = parse_file("input.txt")
target = Rect([-50, 50], [-50, 50], [-50, 50])

intersections = [maybe_intersect(target, x) for x in parsed_lines]


print("Solution nr 1:")
print(find_count([x for x in intersections if not x is None]))

print("Solution nr 2:")
print(find_count(parsed_lines))
