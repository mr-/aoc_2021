import re
import functools


def read_groups(filename):
    file1 = open(filename, 'r')
    content = file1.read()
    return content.split("\n\n")


def acc_groups_1(groups):
    answers = []
    for group in groups:
        answer = set(''.join(group.split('\n')))
        answers.append(answer)

    return answers


def acc_groups_2(groups):
    answers = []
    for group in groups:
        answer = [set(x) for x in group.split('\n') if len(x) > 0]
        sames = functools.reduce(lambda x, y: x & y, answer)
        answers.append(sames)

    return answers


groups = read_groups("input.txt")

total = sum([len(x) for x in acc_groups_1(groups)])
print(f"total answers: {total}")


total = sum([len(x) for x in acc_groups_2(groups)])
print(f"total common answers: {total}")
