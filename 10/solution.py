import functools
from functools import reduce




def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res


matches = {
    '{': '}',
    '(': ')',
    '<': '>',
    '[': ']',
}
values = {
    '}': 3,
    ')': 1,
    '>': 4,
    ']': 2,
}

def eval_line(line):
    expectation_stack = []
    for c in line:
        if c in matches.keys():
            expectation_stack.append(matches[c])
        else:
            expected = expectation_stack.pop()
            if c != expected:
                return []
    return list(reversed(expectation_stack))

def evaluate(missing):
    value = 0
    for x in missing:
        value = value * 5 + values[x]
    return value



lines =[
"[({(<(())[]>[[{[]{<()<>>",
"[(()[<>])]({[<{<<[]>>(",
"{([(<{}[<>[]}>{[]{[(<()>",
"(((({<>}<{<{<>}{[]{[]{}",
"[[<[([]))<([[{}[[()]]]",
"[{[{({}]{}}([{[{{{}}([]",
"{<[[]]>}<{[{[{[]{()[[[]",
"[<(<(<(<{}))><([]([]()",
"<{([([[(<>()){}]>(<<{{",
"<{([{{}}[<[[[<>{}]]]>[]]"
]

lines = read_file("input.txt")

all_values = [evaluate(eval_line(line)) for line in lines]
valid_values = sorted([v for v in all_values if v > 0])
# print(valid_values)

print(valid_values[int(len(valid_values)/2 )])
