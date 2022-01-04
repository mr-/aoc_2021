from functools import reduce


def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res


def parse_lines(lines):
    ts = lines[1].split(',')
    timetable = [(int(ts[i]), i) for i in range(len(ts)) if ts[i] != "x"]
    return timetable


def egcd(a, b):
    if a == 0:
        return b, 0, 1
    else:
        gcd, x, y = egcd(b % a, a)
        return gcd, y - (b // a) * x, x


lines = read_file("input.txt")
timetable = parse_lines(lines)


print(timetable)
# so, we are solving:
# t = k0*i0
# t + d1 = k1*i1
# t + d2 = k2*i2
# ...
# Chinese remainder to the rescue?

N = reduce((lambda x, y: x*y), ([c[0] for c in timetable]))
# => 1797379356693401 big big..

# Not 100% sure that construction is going to work with the large offsets..
# we'll see.
s = 0
for ki, di in timetable:
    Ni = int(N/ki)
    gcd, Mi, mi = egcd(Ni, ki)
    s = s + (ki-di)*Ni*Mi

while s < 0:
    s = s + N
print(f"solution: {s}")

for ki, di in timetable:
    print(f"{s}%{ki} = {s % ki}, di={di}")
