import functools
from functools import reduce


def read_file(filename):
    file1 = open(filename, 'r')
    res = []
    for line in file1:
        res.append(line.strip())

    return res


def parse_lines(lines):
    sheet = {}
    for line in lines:
        (x,y) = line.split(',')
        sheet[(int(x), int(y))] = 1

    return sheet


def fold_vertical(sheet, fold_y):
    new_sheet = {}
    for (x,y) in sheet.keys():
        if not (x,y) in sheet:
            continue
        if y < fold_y:
            new_sheet[(x,y)] = sheet[(x,y)]
        if y == fold_y:
            pass
        if y > fold_y:
            new_y = fold_y - (y-fold_y)
            new_sheet[(x,new_y)] = sheet[(x, y)]

    return new_sheet


def fold_horizontal(sheet, fold_x):
    new_sheet = {}
    for (x,y) in sheet.keys():
        if not (x,y) in sheet:
            continue
        if x < fold_x:
            new_sheet[(x,y)] = sheet[(x,y)]
        if x == fold_x:
            pass
        if x > fold_x:
            new_x = fold_x - (x - fold_x)
            new_sheet[(new_x, y)] = sheet[(x, y)]

    return new_sheet


def pretty_print(sheet):
    max_x = max([ x for (x,y) in sheet.keys()])
    max_y = max([ y for (x,y) in sheet.keys()])

    for y in range(max_y + 1):
        for x in range(max_x + 1):

            if (x,y) in sheet:
                print("#", end = '')
            else:
                print(".", end = '')
            if x == max_x:
                print()

lines = [
'6,10',
'0,14',
'9,10',
'0,3',
'10,4',
'4,11',
'6,0',
'6,12',
'4,1',
'0,13',
'10,12',
'3,4',
'3,0',
'8,4',
'1,10',
'2,14',
'8,10',
'9,0'
]

lines = read_file('input_sheet.txt')

sheet = parse_lines(lines)

# fold along x=655
sheet = fold_horizontal(sheet, 655)

#fold along y=447
sheet = fold_vertical(sheet, 447)

# fold along x=327
sheet = fold_horizontal(sheet, 327)

# fold along y=223
sheet = fold_vertical(sheet, 223)

# fold along x=163
sheet = fold_horizontal(sheet, 163)

# fold along y=111
sheet = fold_vertical(sheet, 111)

# fold along x=81
sheet = fold_horizontal(sheet, 81)

# fold along y=55
sheet = fold_vertical(sheet, 55)

# fold along x=40
sheet = fold_horizontal(sheet, 40)

# fold along y=27
sheet = fold_vertical(sheet, 27)

# fold along y=13
sheet = fold_vertical(sheet, 13)

# fold along y=6
sheet = fold_vertical(sheet, 6)

pretty_print(sheet)
