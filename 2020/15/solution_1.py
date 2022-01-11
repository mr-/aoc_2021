from os import stat
import re


def do_next(mem, step):
    last = mem["last"]
    if not last in mem["seen"]:
        mem["seen"][0] = [step]
        mem["last"] = 0
    elif len(mem["seen"][last]) == 1:
        mem["seen"][0].append(step)
        mem["last"] = 0
    else:
        new = mem["seen"][last][-1] - mem["seen"][last][-2]
        mem["last"] = new
        mem["seen"][new] = [step] if (not new in mem["seen"] or len(
            mem["seen"][new]) == 0) else [mem["seen"][new][-1], step]


mem = {
    "last": 6,
    "seen": {0: [0], 3: [1], 6: [2]}
}

mem = {
    "last": 18,
    "seen": {9: [0], 12: [1], 1: [2], 4: [3], 17: [4], 0: [5], 18: [6]}
}

for i in range(30000000-7):
    if i % 1000000 == 0:
        print(f"{i}: {len(mem['seen'].keys())}")
    do_next(mem, 7+i)

print(mem["last"])
