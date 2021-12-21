import functools
from functools import reduce
import networkx as nx
from random import randrange
import itertools


def foo():
    i = 1
    while True:
        yield (i, i+1, i+2)
        i = i+3

def get_sums():
    gen = foo()
    return (x+y+z for x,y,z in gen)

def n(x):
    rem = x%10
    if rem == 0:
        return 10
    return rem

def count_thing(sums, initial1, initial2):
    current_count_1 = 0
    current_pos_1 = initial1

    current_count_2 = 0
    current_pos_2 = initial2
    count=0
    while True:
        x = next(sums)
        count = count +1

        current_pos_1 = n(x + current_pos_1)
        current_count_1 = current_count_1 + current_pos_1
        if current_count_1 >= 1000:
            return (current_count_2, count*3)

        x = next(sums)
        count = count +1

        current_pos_2 = n(x + current_pos_2)
        current_count_2 = current_count_2 + current_pos_2
        if current_count_2 >= 1000:
            return (current_count_1, count*3)

player_1 = 2
player_2 = 8


p = count_thing(get_sums(), player_1, player_2)
print(p[0]*p[1])



