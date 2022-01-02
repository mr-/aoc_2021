def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res


def parse_line(line):
    rule, passw = line.split(':')

    cnt, char = rule.split(' ')
    l, h = cnt.split('-')

    return {
        "l": int(l),
        "h": int(h),
        "char": char.strip(),
        "pass": passw.strip()
    }


def validate_line_1(line):
    c = line["pass"].count(line["char"])
    return c >= line["l"] and c <= line["h"]


def validate_line_2(line):
    c1 = 1 if line["pass"][line["l"]-1] == line["char"] else 0
    c2 = 1 if line["pass"][line["h"]-1] == line["char"] else 0

    return (c1 + c2) == 1


lines = read_file("input.txt")


solution = sum([1 for line in lines if validate_line_1(parse_line(line))])
print(solution)


solution = sum([1 for line in lines if validate_line_2(parse_line(line))])
print(solution)
