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


def parse_mask(line):
    if not line.startswith("mask"):
        return None
    return line[7:]


def parse_assignment(line):
    if not line.startswith("mem"):
        return None
    m = re.fullmatch(r"mem\[(\d+)\] = (\d+)", line)
    return {
        "d": format(int(m.group(1)), '036b'),
        "v": int(m.group(2)),
    }


def do_apply_mask(mask, s):
    r = ""
    for i in range(len(mask)):
        if mask[i] == '0':
            r = r + s[i]
        if mask[i] == '1':
            r = r + '1'
        if mask[i] == 'X':
            r = r + 'X'
    return r


# def ev_xs(xs):
#     print(len(xs))
#     if len(xs) == 0:
#         return 0
#     m = max(xs)
#     len_r = len(xs) - 1
#     r = ev_xs(xs - {m})

#     return 2*r + 2**len_r * 2**m


# def evaluate(reg):
#     s = 0
#     for v in reg.values():
#         print(s)
#         floats = ev_xs(v["floats"])
#         n = len(v["floats"])
#         s = s + 2**n * pp(v["v"])[1] + floats
#     return s


def intersects(l, r):
    for i in range(len(l)):
        if l[i] != "X" and r[i] != "X" and l[i] != r[i]:
            return False
    return True


def isSubset(l, r):
    for i in range(len(l)):
        if l[i] == "X" and r[i] != "X":
            return False
        if l[i] != "X" and r[i] != "X" and l[i] != r[i]:
            return False
    return True


def write(mem, addr, val):
    n_vars = addr.count("X")
    f = "".join(
        c if c != "X" else "{}" for c in addr)

    for n in range(2**n_vars):
        mem[int(f.format(*list(f"{n:0{n_vars}b}")), base=2)] = val


lines = read_file("input.txt")
current_mask = None
assignments = []
for line in lines:
    mask = parse_mask(line)
    if mask is not None:
        current_mask = mask
    assignment = parse_assignment(line)
    if assignment is not None:
        assignments.append(
            (do_apply_mask(current_mask, assignment["d"]), assignment["v"]))


mem = {}
for a in assignments:
    write(mem, a[0], a[1])


res = sum(mem.values())
print(f"Result is {res}")
