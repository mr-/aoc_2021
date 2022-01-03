import re
import functools
import pprint


def read_file(filename):
    file1 = open(filename, 'r')
    return [line.strip() for line in file1]


def parse_inner(rest):
    m = re.findall(r"(\d+) (\w+ \w+)", rest.strip())
    return m


def parse_line(line):
    if "contain no other bags" in line:
        return None
    start, rest = line.split("contain")
    outer_parts = start.split(' ')
    outer = f"{outer_parts[0]} {outer_parts[1]}"
    inner = parse_inner(rest)
    return {"out": outer, "in": inner}


def create_tree(lines):
    tree = {}
    for line in lines:
        parsed = parse_line(line)
        if parsed is None:
            continue
        if parsed["out"] in tree:
            tree[parsed["out"]] = tree[parsed["out"]] + parsed["in"]
        else:
            tree[parsed["out"]] = parsed["in"]

    return tree


def sum_up(tree, start, depth=0):
    if not start in tree:
        return 0

    acc = 0
    for child in tree[start]:
        this = int(child[0])
        s = sum_up(tree, child[1], depth=depth+1)
        # print(f"{' '* depth }  {this} * {s} {child[1]}")
        acc = acc + this * (s+1)

    return acc


lines = read_file("input.txt")

tree = create_tree(lines)

# pprint.pprint(tree)
sol = sum_up(tree, "shiny gold")
print(f"{sol} bags")
