import re
import functools
import pprint


def read_file(filename):
    file1 = open(filename, 'r')
    return [int(line.strip()) for line in file1]


lines = sorted(read_file("input.txt"))
lines = [0] + lines + [max(lines)+3]
diffs = [b-a for a, b in zip(lines, lines[1:])]

seen = {}
for d in diffs:
    if d in seen:
        seen[d] = seen[d] + 1
    else:
        seen[d] = 1
print(f"Answer one is... {seen[1] * seen[3]}")


# need to figure out the lengths of the 1 chains.. we can only remove adapters from these.
consec_1s = "".join([str(n) for n in diffs]).split("3")


def f(n):
    if n == 1:
        return 2
    if n == 2:
        return 4
    # when adding a free 1, we basically get 2* what we had before.
    # -1 for the one combination (..., 0, 1) that we've counted before.
    return f(n-1)*2 - 1


# len - 1 to account for the 1 that's right next to a 3.. can never remove that.
things = [f(len(o)-1) for o in consec_1s if len(o) > 1]

answer = functools.reduce((lambda x, y: x * y), things)
print(f"answer number 2: {answer}")
