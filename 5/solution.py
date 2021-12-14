import functools


def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res


def parse_line(line):
    points = line.split("->")
    left = tuple( int(n) for n in  points[0].split(","))
    right = tuple( int(n) for n in  points[1].split(","))
    return (left, right)



def explode_line(points):
    left = points[0]
    right = points[1]
    xs = range(min(left[0], right[0]), max(left[0], right[0])+1)
    ys = range(min(left[1], right[1]), max(left[1], right[1])+1)
    if(len(xs) > 1 and len(ys) > 1):
        if(left[0] < right[0] and left[1] < right[1]):
            xs = range(left[0], right[0]+1)
            ys = range(left[1], right[1]+1)
            return [(xs[i], ys[i]) for i in range(0, len(xs))]

        if(left[0] < right[0] and left[1] > right[1]):
            xs = range(left[0], right[0]+1)
            ys = list(reversed(range(right[1], left[1]+1)))
            return [(xs[i], ys[i]) for i in range(0, len(xs))]

        if(left[0] > right[0] and left[1] < right[1]):
            xs = list(reversed(range(right[0], left[0]+1)))
            ys = range(left[1], right[1]+1)
            return [(xs[i], ys[i]) for i in range(0, len(xs))]

        if(left[0] > right[0] and left[1] > right[1]):
            xs = list(reversed(range(right[0], left[0]+1)))
            ys = list(reversed(range(right[1], left[1]+1)))
            return [(xs[i], ys[i]) for i in range(0, len(xs))]

    return [(x,y) for x in xs for y in ys]


def count_dups(points):
    seen = {}
    dups = {}
    for point in points:
        if point in seen:
            dups[point] = 1
        seen[point] = 1
    return dups.keys()

def to_points(lines):
   return [ p  for line in lines for p in explode_line(parse_line(line))]

lines =  read_file("input.txt")
# lines = ["391,140 -> 391,160",  "391,147 -> 391,157", "391,147 -> 391,157"]#, "381,140 -> 392,140"]
# lines = [
# "0,9 -> 5,9",
# "8,0 -> 0,8",
# "9,4 -> 3,4",
# "2,2 -> 2,1",
# "7,0 -> 7,4",
# "6,4 -> 2,0",
# "0,9 -> 2,9",
# "3,4 -> 1,4",
# "0,0 -> 8,8",
# "5,5 -> 8,2"
# ]

points = to_points(lines)
# print(points)
count = len(count_dups(points))

print(count)

