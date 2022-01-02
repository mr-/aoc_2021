def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(int(line.strip()))

    return res


lines = read_file("input.txt")

solutions = [x*y for x in lines for y in lines if x+y == 2020]
print(solutions)

solutions = [
    x*y*z for x in lines for y in lines for z in lines if x+y+z == 2020]
print(solutions)
