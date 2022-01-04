def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res


def parse_line(line):
    cmd = line[0]
    par = int(line[1:])

    def apply(state):
        if cmd == "N":
            return {**state, "n": state["n"] + par}
        if cmd == "S":
            return {**state, "n": state["n"] - par}
        if cmd == "E":
            return {**state, "e": state["e"] + par}
        if cmd == "W":
            return {**state, "e": state["e"] - par}
        if cmd == "L":
            return {**state, "a": state["a"] + par}
        if cmd == "R":
            return {**state, "a": state["a"] - par}
        if cmd == "F":
            dirs = {0: (1, 0), 90: (0, 1), 180: (-1, 0), 270: (0, -1)}
            d = dirs[state["a"] % 360]
            return {**state, "n": state["n"] + d[1]*par, "e": state["e"] + d[0]*par}
    return apply


def apply(lines):
    state = {"n": 0, "e": 0, "a": 0}
    for line in lines:
        state = parse_line(line)(state)
    return state


lines = read_file("input.txt")

s = apply(lines)
print(f"Solution is {abs(s['n']) + abs(s['e'])}")
