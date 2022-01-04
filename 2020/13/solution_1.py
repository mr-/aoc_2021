def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res


def parse_lines(lines):
    t = int(lines[0])
    timetable = [int(i) for i in lines[1].split(',') if i != "x"]
    return {"t": t, "timetable": timetable}


lines = read_file("input.txt")
parsed = parse_lines(lines)
t = parsed["t"]
timetable = parsed["timetable"]

s = min([(i, i-(t % i)) for i in timetable], key=lambda p: p[1])
print(f"Sol is {s[0] * s[1]}")
