import re
import functools
import pprint


class Buffer():
    nrs = []
    sums = []

    def __init__(self, bound):
        self.bound = bound

    def add_number(self, nr):
        for i in range(len(self.sums)):
            self.sums[i].add(self.nrs[i] + nr)

        self.sums.append(set())
        self.nrs.append(nr)

        if len(self.sums) > self.bound:
            self.nrs = self.nrs[1:]
            self.sums = self.sums[1:]

    def add_numbers(self, nrs):
        for nr in nrs:
            self.add_number(nr)

    def check_number(self, nr):
        return any(nr in s for s in self.sums)


def read_file(filename):
    file1 = open(filename, 'r')
    return [int(line.strip()) for line in file1]


lines = read_file("input.txt")
bnd = 25
bfr = Buffer(bnd)
bfr.add_numbers(lines[:bnd])
for line in lines[bnd:]:
    if not bfr.check_number(line):
        print(line)
    bfr.add_number(line)
