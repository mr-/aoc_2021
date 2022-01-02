import functools


def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res


def parse_line(line):
    points = line.split("|")
    left = [frozenset(sorted(list(n.strip()))) for n in  points[0].strip().split(" ")]
    right = [frozenset(sorted(list(n.strip()))) for n in  points[1].strip().split(" ")]
    return (left, right)

# be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe


lines = read_file('input.txt')

# lines = [
# "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
# "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
# "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
# "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
# "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
# "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
# "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
# "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
# "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
# "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
# ]

# lines = [
#     "bgdeca agdbe gfb fdbgce bf eafb dfgab efbgdca gebafd dgcaf | gfbdea gfb bacedg adcgf"
# ]

numbers = [parse_line(line) for line in lines ]


# Segments,   Candidates
# 2            1
# 3            7
# 4            4
# 5            5, 3, 2
# 6            0, 6, 9
# 7            8


def analyse_combinations(combinations):
    mapping = {}
    seven = [c for c in combinations if len(c) == 3][0]
    four = [c for c in combinations if len(c) == 4][0]
    one = [c for c in combinations if len(c) == 2][0]
    eight = [c for c in combinations if len(c) == 7][0]
    nine = [c for c in combinations if len(c) == 6 and (seven | four) < c][0]

    bottom = nine - (seven | four)
    three = [c for c in combinations if len(c) == 5 and (seven | bottom) < c][0]
    middle = three - (seven | bottom)
    zero = [c for c in combinations if len(c) == 6 and not middle < c][0]

    six = [c for c in combinations if len(c) == 6 and not (c == zero or c == nine)][0]
    upper_right = nine - six

    two = [c for c in combinations if len(c) == 5 and upper_right < c and not c == three][0]

    five = [c for c in combinations if len(c) == 5 and not (c == two or  c == three)][0]

    return {
        zero: 0,
        one: 1,
        two: 2,
        three: 3,
        four: 4,
        five: 5,
        six: 6,
        seven: 7,
        eight: 8,
        nine: 9
    }

s = 0
for raw_line in lines:
    line = parse_line(raw_line)

    mapping = analyse_combinations(line[0])
    nr = int("".join([str(mapping[c]) for c in line[1]]))
    s = s + nr


print(s)
