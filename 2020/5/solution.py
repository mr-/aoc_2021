
def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res


def find_pos(m, n, s):
    if len(s) == 0:
        return 0
    middle = int(m/2**n)
    if s[0] in ["F", "L"]:
        return find_pos(m, n+1, s[1:])
    else:
        return middle + find_pos(m, n+1, s[1:])


def index(s):
    return 8 * find_pos(128, 1, s[:-3]) + find_pos(8, 1, s[-3:])


lines = read_file("input.txt")
ids = sorted([index(s) for s in lines])
m = max(ids)
print(f"max is {m}")

for l, h in zip(ids, ids[1:]):
    if l+2 == h:
        print(f"found something between {l} and {h}: {l+1}")
