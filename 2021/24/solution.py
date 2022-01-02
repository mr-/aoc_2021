import functools
from functools import reduce


def read_file(filename):
    file1 = open(filename, 'r')
    return file1.read()


def get_blocks(content):
    return [("inp w\n" + x).split('\n') for x in content.split("inp w\n")]


def nint(x):
    try:
        return int(x)
    except:
        return 0


def mul(state, l, r):
    nl = state.get(l, nint(l))
    nr = state.get(r, nint(r))
    return {**state, l: nl*nr}


def add(state, l, r):
    nl = state.get(l, nint(l))
    nr = state.get(r, nint(r))
    return {**state, l: nl+nr}


def mod(state, l, r):
    nl = state.get(l, nint(l))
    nr = state.get(r, nint(r))
    return {**state, l: nl % nr}


def div(state, l, r):
    nl = state.get(l, nint(l))
    nr = state.get(r, nint(r))
    return {**state, l: int(nl / nr)}


def eql(state, l, r):
    nl = state.get(l, nint(l))
    nr = state.get(r, nint(r))
    return {**state, l: 1 if nl == nr else 0}


def inp(state, l):
    new = state["input"][0]
    return {**state, "input": state["input"][1:], l: new}


operations = {
    "mul": mul,
    "add": add,
    "mod": mod,
    "div": div,
    "eql": eql,
    "inp": inp,
}


def parse_line(line):
    line = line.strip()
    tokens = line.split(" ")
    if len(tokens) == 2:
        return lambda state: operations[tokens[0]](state, tokens[1])
    if len(tokens) == 3:
        return lambda state: operations[tokens[0]](state, tokens[1], tokens[2])


def parse_lines(lines):
    ops = [parse_line(line) for line in lines]
    return [op for op in ops if op is not None]


def apply(ops, state):
    new_state = state.copy()
    for op in ops:
        new_state = op(new_state)
    return new_state


def prog_orig(d, a1, a3, z, w):
    x = 0
    x = x + z
    x = x % 26
    z = int(z/d)
    x = x + a1
    x = 1 if x == w else 0
    x = 1 if x == 0 else 0
    y = 0
    y = y + 25
    y = y * x
    y = y + 1
    z = z * y
    y = 0
    y = y + w
    y = y + a3
    y = y * x
    z = z + y
    return z


def prog_reduced(d, in_shift, out_shift, z, w):
    if z % 26 + in_shift == w:
        return int(z/d)
    else:
        return int(z/d) * 26 + w + out_shift


def get_last_shift(z):
    if len(z.get("val", [])) > 0:
        return z["val"][-1]["num"]
    else:
        return 0


def get_last_shift_index(z):
    return z["val"][-1]["i"] if len(z.get("val", [])) > 0 else None


def can_satisfy(c):
    return len(set([c+i for i in range(1, 10)]) & set(range(1, 10))) > 0

# z has {"cstrs": [{"i1", "c", "i2"}], "val": [{"num", "i"] }
# where "num", "i" means w_i + num


def prog(i, d, in_shift, out_shift, z):
    const = get_last_shift(z) + in_shift
    unconstrained = z.get("val", []).copy()
    constrained = z.get("val", []).copy()
    constraints = z.get("cstrts", []).copy()
    if d == 26:
        if len(unconstrained) > 0:
            unconstrained.pop()

        unconstrained.append({"i": i, "num": out_shift})
    if d == 1:
        unconstrained.append({"i": i, "num": out_shift})
    if not can_satisfy(const):
        # can't satisfy thing..
        return [{"val": unconstrained, "cstrts": constraints}]
    else:
        new_constraints = constraints.copy()
        last_index = get_last_shift_index(z)

        constraints.append(
            {"i1": last_index,
                "c": const,
                "i2": i,
                "type": "!="})
        new_constraints.append(
            {"i1": last_index,
             "c": const,
             "i2": i,
             "type": "=="})
        if d == 26:
            if len(constrained) > 0:
                constrained.pop()
        return [
            {"val": unconstrained, "cstrts": constraints},
            {"val": constrained, "cstrts": new_constraints},
        ]


def progz(i, d, in_shift, out_shift, zs):
    return [x for z in zs for x in prog(i, d, in_shift, out_shift, z)]


def pp_constraints(constraints):
    res = []
    for c in constraints:
        res.append(f"w{c['i1']} + {c['c']} {c['type']} w{c['i2']}")
    return " && ".join(res)


def pp_val(val):
    res = []
    for i in val:
        r = str(i["num"])
        res.append(r)
    return "-".join(res)


def pp_z(z):
    cts = pp_constraints(z["cstrts"])
    if len(cts) > 0:
        print(cts)
    print(pp_val(z["val"]))


def pp_tree(tree):
    for i in tree:
        pp_z(i)


prog_config = [
    {"d": 1,
     "in_shift": 11,
     "out_shift": 5,
     },
    {"d": 1,
     "in_shift": 13,
     "out_shift": 5,
     },
    {"d": 1,
     "in_shift": 12,
     "out_shift": 1,
     },
    {"d": 1,
     "in_shift": 15,
     "out_shift": 15,
     },
    {"d": 1,
     "in_shift": 10,
     "out_shift": 2,
     },
    {"d": 26,
     "in_shift": -1,
     "out_shift": 2,
     },
    {"d": 1,
     "in_shift": 14,
     "out_shift": 5,
     },
    {"d": 26,
     "in_shift": -8,
     "out_shift": 8,
     },
    {"d": 26,
     "in_shift": -7,
     "out_shift": 14,
     },
    {"d": 26,
     "in_shift": -8,
     "out_shift": 12,
     },
    {"d": 1,
     "in_shift": 11,
     "out_shift": 7,
     },
    {"d": 26,
     "in_shift": -2,
     "out_shift": 14,
     },
    {"d": 26,
     "in_shift": -2,
     "out_shift": 13,
     },
    {"d": 26,
     "in_shift": -13,
     "out_shift": 6,
     },
]
tree = prog(0, prog_config[0]["d"], prog_config[0]
            ["in_shift"], prog_config[0]["out_shift"], {})
i = 1
for pc in prog_config[1:]:
    tree = progz(i, pc["d"], pc["in_shift"], pc["out_shift"], tree)
    i = i + 1


for z in tree:
    if len(z["val"]) == 0:
        print("FOUND CANDIDATE")
        pp_z(z)


content = read_file("input.txt")

programs = [parse_lines(block) for block in get_blocks(content)]


def pretty_print(z):
    res = []
    while z > 0:
        res.append(z % 26)
        z = int(z / 26)
    return list(reversed(res))


def apply_all(ps, inp):
    z = 0
    for i in range(len(ps)):
        state = {"w": 0, "x": 0, "y": 0, "z": z, "input": [inp[i]]}
        state = apply(ps[i], state)
        z_new = state["z"]
        z = z_new
    return z


print("MIN")
apply_all(programs, [9,	6,	9, 1,	8, 9,	9, 6,	9, 2,	4, 9,	9,	1])
print("".join(str(x) for x in [9,	6,	9, 1,	8, 9,	9, 6,	9, 2,	4, 9,	9,	1]))

print("MAX")
apply_all(programs, [9, 1, 8, 1, 1, 2, 4, 1, 9, 1, 1, 6, 4, 1])
print("".join(str(x) for x in [9, 1, 8, 1, 1, 2, 4, 1, 9, 1, 1, 6, 4, 1]))
