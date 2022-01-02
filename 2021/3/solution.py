import functools


def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res

def parse_line(line):
    return [int(char) for char in line]

def add_line(l1, l2):
    return [ x + y for [x, y] in zip(l1, l2)]


def to_most_common(commands):
    length = len(commands)
    res = functools.reduce(lambda x, y : add_line(x, y) , commands)

    def det(c):
        if length %2 == 0:
            if c == length/2:
                print(f"WELL HELLO ")
            return 1 if c >= length/2 else 0
        else:
            return 1 if c > int(length/2) else 1

    return [ 0 if det(c) else 1 for c in res ]




def to_least_common(commands):
    length = len(commands)
    res = functools.reduce(lambda x, y : add_line(x, y) , commands)

    def det(c):
        if length %2 == 0:
            if c == length/2:
                print(f"WELL HELLO ")
            return 0 if c >= length/2 else 1
        else:
            return 0 if c > int(length/2) else 1

    return [ 0 if det(c) else 1 for c in res ]



commands = [parse_line(line) for line in read_file('input.txt')]

def filter_by(to_common, commands):
    str_len = len(commands[0])
    for i in range(str_len):
        to_keep = to_common(commands)[i]
        commands = list(filter( lambda x: x[i] == to_keep, commands))
        if len(commands) == 1:
            return int("".join([str(c) for c in commands[0]]),2)

print(filter_by(to_most_common, commands) * filter_by(to_least_common, commands))




# print(commands)


# 0 1 1
# 0 1 1
# 0 0 1
# 0 0 0
# 0 0 0

# 0 2 3

# int(5/2) = 2


# 1 1 1
# 0 1 1
# 0 0 1
# 0 0 0

# 1 2 3

# int(4/2) = 2
