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


def nop(state):
    return {**state, "i": state["i"] + 1}


def jmp(state, j):
    return {**state, "i": state["i"] + j}


def acc(state, a):
    return {**state, "acc": state["acc"] + a, "i": state["i"] + 1}


def line_to_op(line):
    parts = line.split(" ")
    if parts[0] == "nop":
        return lambda state, switch=False: nop(state) if not switch else jmp(state, int(parts[1]))
    if parts[0] == "jmp":
        return lambda state, switch=False: jmp(state, int(parts[1])) if not switch else nop(state)
    if parts[0] == "acc":
        return lambda state, switch=False: acc(state, int(parts[1]))


lines = read_file("input.txt")
program = [line_to_op(line) for line in lines]


def apply(program, state):
    seen = set()
    while True:
        if state["i"] in seen:
            return ("loops", state["acc"])
        elif state["i"] == len(program):
            return ("terminates", state["acc"])
        else:
            seen.add(state["i"])
        state = program[state["i"]](state)


print(apply(program, {"i": 0, "acc": 0}))


# No need to be clever.. the program is short and "terminates" quickly
for i in range(len(program)):
    altered = program.copy()
    altered[i] = lambda state: program[i](state, switch=True)
    state = {"i": 0, "acc": 0}
    res = apply(altered, state)
    if res[0] == "terminates":
        print(res)
