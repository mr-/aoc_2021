from os import stat
import re


def pp(thing):
    r = ""
    for i in reversed(range(max(thing.keys())+1)):
        if i in thing:
            r = r + (thing[i])
        else:
            r = r + ('0')

    return (r, int(r, 2))


def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res


def parse_thing(l):
    thing = {}
    for i in range(len(l)):
        j = len(l)-1 - i
        if l[i] == '0':
            thing[j] = '0'
        if l[i] == '1':
            thing[j] = '1'
    return thing


def parse_mask(line):
    if not line.startswith("mask"):
        return None
    l = line[7:]

    return parse_thing(l)


def parse_assignment(line):
    if not line.startswith("mem"):
        return None
    m = re.fullmatch(r"mem\[(\d+)\] = (\d+)", line)
    return {
        "d": m.group(1),
        "v": parse_thing(format(int(m.group(2)), 'b')),
    }


def apply_mask(reg, mask, assignment):
    res = {**assignment["v"],  **mask}
    return {**reg, assignment["d"]: res}


# def apply(state, expr):
    # if "d" in

def evaluate(reg):
    s = 0
    for v in reg.values():
        s = s + pp(v)[1]
    return s

lines = read_file("input.txt")
state = {"mask": None, "reg": {}}
for line in lines:
    mask = parse_mask(line)
    if mask is not None:
        state = {**state, "mask": mask}
    assignment = parse_assignment(line)
    if assignment is not None:
        new_reg = apply_mask(state["reg"], state["mask"], assignment)
        state = {**state, "reg": new_reg}
# print(state["reg"])
print(evaluate(state["reg"]))
