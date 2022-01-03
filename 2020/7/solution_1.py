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
        for inner in parsed["in"]:
            if inner[1] in tree:
                tree[inner[1]].append(parsed["out"])
            else:
                tree[inner[1]] = [parsed["out"]]
    return tree


def explore_from(tree, start):
    if not start in tree:
        return []
    from_there = [x for child in tree[start]
                  for x in [child, *explore_from(tree, child)]]
    return from_there


lines = read_file("input.txt")

tree = create_tree(lines)
sol = len(set(explore_from(tree, "shiny gold")))
print(f"{sol} bags")
