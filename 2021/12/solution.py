import functools
from functools import reduce

def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res


def parse_lines(lines):
    tree = {}
    for line in lines:
        (source, target) = line.split('-')
        if not source in tree:
            tree[source] = [target]
        else:
            tree[source].append(target)

        if not target in tree:
            tree[target] = [source]
        else:
            tree[target].append(source)

    return tree


def is_large(node):
    return node[0].isupper()

def is_small(node):
    return not is_large(node)


def add_to_seen(seen, node):
    if is_large(node):
        return {**seen, node: 1}

    if node in seen:
        return {**seen, node: 2}
    return {**seen, node: 1}

def can_visit(seen, node):
    if node == "start":
        return False

    if is_large(node):
        return True

    if not node in seen:
        return True

    if 2 in seen.values():
        return False

    return True


def paths_from(tree, node, seen):
    if (node == "end"):
        return [["end"]]
    new_seen = add_to_seen(seen, node)

    next_nodes = [n for n in tree[node] if can_visit(new_seen, n)]

    # print(f"{node} -> {next_nodes} from originally {tree[node]}")
    # print(f"       had seen {new_seen}")
    # print()
    next_paths = [p for next_node in next_nodes for p in paths_from(tree, next_node, new_seen)]

    return [[node] + path for path in next_paths]



lines = [
"start-A",
"start-b",
"A-c",
"A-b",
"b-d",
"A-end",
"b-end"
]
lines = [
"dc-end",
"HN-start",
"start-kj",
"dc-start",
"dc-HN",
"LN-dc",
"HN-end",
"kj-sa",
"kj-HN",
"kj-dc"
]
lines = [
"fs-end",
"he-DX",
"fs-he",
"start-DX",
"pj-DX",
"end-zg",
"zg-sl",
"zg-pj",
"pj-he",
"RW-he",
"fs-DX",
"pj-RW",
"zg-RW",
"start-pj",
"he-WI",
"zg-he",
"pj-fs",
"start-RW"
]

lines = read_file('input.txt')

START="start"
END="end"



tree = parse_lines(lines)
# print(tree)
# print(paths_from(tree, START, {}))
paths = paths_from(tree, START, {})

for path in paths:
    print(",".join(path))
print(len(paths))


# start,kj,dc,kj,dc,end

# seen = {}
# print(can_visit(seen, "kj"))
# seen = add_to_seen(seen, "kj")
# print(can_visit(seen, "dc"))
# seen = add_to_seen(seen, "dc")
# print(can_visit(seen, "kj"))
# seen = add_to_seen(seen, "kj")
# print(can_visit(seen, "dc"))
# seen = add_to_seen(seen, "dc")
