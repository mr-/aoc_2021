import functools
from functools import reduce
import networkx as nx
import math

def hits(velo, target):
    lr = target["lr"]
    ul = target["ul"]

    x = 0
    y = 0
    trace = [(0,0)]
    max_y = y
    hit = False
    while x <= lr[0] and y >= lr[1]:
        x = velo[0] + x
        y = velo[1] + y

        velo = (max(velo[0] - 1, 0), velo[1] - 1)
        trace.append((x,y))
        if y > max_y:
            max_y = y
        if x >= ul[0] and y <= ul[1] and x <= lr[0] and y >= lr[1]:
            hit = True
            break

    return (hit, max_y)




# example: target area: x=20..30, y=-10..-5
# target = {"ul": (20, -5), "lr": (30, -10)}


# puzzle: target area: x=195..238, y=-93..-67
target = {"ul": (195, -67), "lr": (238, -93)}

# max horizontal steps: "Exactly" get to 238: 253 = sum(23), so we get at most 23 steps to "fall down" until we
# fall "vertially". Then we need some margin to definitely hit a gap falling down from 0.
# We aways come down to 0 and we'll definitely miss it if the step is > 93 (the lower border of the target)

# A lower bound is -100, otherwise we've missed the target in the first step.
found_max_y = None
count = 0
for vx in range(0, 240):
    for vy in range(-100, 95):
        hit, max_y = hits((vx,vy), target)
        if hit:
            count = count + 1
            if found_max_y is None or found_max_y < max_y:
                # print(f"{(vx, vy)} {max_y}")

                found_max_y = max_y

print()
print(f"found_max_y: {found_max_y} count: {count}")

# found_max_y: 4278 count: 1994
