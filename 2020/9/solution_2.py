import re
import functools
import pprint


class Buffer():
    nrs = []
    total = 0

    def __init__(self, bound):
        self.bound = bound

    def add_number(self, nr):
        self.nrs.append(nr)
        self.total = self.total + nr
        while self.total >= self.bound:
            if len(self.nrs) > 1 and self.total == self.bound:
                return self.nrs

            self.total = self.total - self.nrs[0]
            self.nrs = self.nrs[1:] if len(self.nrs) > 0 else []



def read_file(filename):
    file1 = open(filename, 'r')
    return [int(line.strip()) for line in file1]


lines = read_file("input.txt")
bnd = 14144619
bfr = Buffer(bnd)
for line in lines:
    r = bfr.add_number(line)
    if r is not None:
        print(max(r) + min(r))
        exit(0)
