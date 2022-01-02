def count_larger(lines):
    count = 0
    last = None
    for nr in lines:
        if last is None:
            last = nr
        else:
            if last < nr:
                count = count + 1
            last = nr
    return count

def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(int(line.strip()))

    return res


lines = read_file('input.txt')
triples = zip(lines, lines[1:], lines[2:])

sums = [a+b+c for [a,b,c] in triples]
print(count_larger(sums))
