import functools
from functools import reduce
import networkx as nx
from random import randrange
import itertools
import pprint


# for each step we have 3*3*3 = 27 possibilities.
# The tree is at most 21 deep.
# phrased differently, consider all sequences of length 3*21 consisting of 1,2,3. Who wins in each?
# that's about 1144561273430837494885949696427 sequences

# each roll has sums of:
# l = [1,2,3]
# [ sum([a,b,c]) for a in l for b in l for c in l]
# [3, 4, 5, 4, 5, 6, 5, 6, 7, 4, 5, 6, 5, 6, 7, 6, 7, 8, 5, 6, 7, 6, 7, 8, 7, 8, 9]
# {9:1, 8:3, 7:6, 6:7, 5:6, 4:3, 3:1 }


# so we can, just as well look at sequences of length 21 with values in 9,8,7,6,5,4,3
# that's about 558545864083284007 sequences

# what's the longest sequence one can get?
# 1,4,1,4,1,4,1,4,1 gives 21, length 9
# Can we get lower? Is that low enough? Let's try to model that :)

throws = {9:1, 8:3, 7:6, 6:7, 5:6, 4:3, 3:1}

def n(x):
    rem = x%10
    if rem == 0:
        return 10
    return rem


def single_step(o):
    return [
        {"pos": n(o["pos"] + throw),
         "val": o["val"] + n(o["pos"] + throw),
         "cnt": o["cnt"] * throws[throw]
     }  for throw in throws.keys()]


def step(l):
    return [y for x in l for y in single_step(x)]


def do_step(thing, cutoff = 21):
    evaluated_step = step(thing)
    finished = [x for x in evaluated_step if x["val"] >= cutoff]
    rest = [x for x in evaluated_step if x["val"] < cutoff]

    return (finished, rest)


def cnts(l):
    return sum([ (x["cnt"]) for x in l])


def wins(initial_pos1, initial_pos2, cutoff = 21):
    thing1 = [{"pos": initial_pos1, "val": 0, "cnt": 1}]
    thing2 = [{"pos": initial_pos2, "val": 0, "cnt": 1}]
    todo2 = []
    wins = 0
    losses = 0
    for i in range(1, 100):
        done1, todo1 = do_step(thing1)

        wins = wins + cnts(done1) * cnts(todo2)
        done2, todo2 = do_step(thing2)
        losses = losses + cnts(todo1) * cnts(done2)
        if len(thing1) == 0:
            return (wins, losses)

        thing1 = todo1
        thing2 = todo2


print(wins(2,8))
