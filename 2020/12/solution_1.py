def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res


def parse_line(line):
    cmd = line[0]
    par = int(line[1:])

    def apply(s):
        if cmd == "N":
            return {**s, "wn": s["wn"] + par}
        if cmd == "S":
            return {**s, "wn": s["wn"] - par}
        if cmd == "E":
            return {**s, "we": s["we"] + par}
        if cmd == "W":
            return {**s, "we": s["we"] - par}
        if cmd == "L":
            we = s["we"]
            wn = s["wn"]
            dirs = {0: (we, wn), 90: (-wn, we),
                    180: (-we, -wn), 270: (wn, -we)}
            d = dirs[par % 360]
            return {**s, "we": d[0], "wn": d[1]}
        if cmd == "R":
            we = s["we"]
            wn = s["wn"]
            dirs = {0: (we, wn), 90: (wn, -we),
                    180: (-we, -wn), 270: (-wn, we)}
            d = dirs[par % 360]
            return {**s, "we": d[0], "wn": d[1]}
        if cmd == "F":
            return {**s, "n": s["n"] + s["wn"]*par, "e": s["e"] + s["we"]*par}
    return apply


def apply(lines):
    state = {"n": 0, "e": 0, "wn": 1, "we": 10}
    for line in lines:
        state = parse_line(line)(state)
    return state


lines = read_file("input.txt")

s = apply(lines)
print(f"Solution is {abs(s['n']) + abs(s['e'])}")
