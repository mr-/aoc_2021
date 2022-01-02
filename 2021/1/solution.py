file1 = open('input.txt', 'r')
count = 0

count = 0
last = None
for line in file1:
    nr = int(line.strip())
    if last is None:
        last = nr
    else:
        if last < nr:
            count = count + 1
        last = nr

print(count)
