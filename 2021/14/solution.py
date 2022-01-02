import functools
from functools import reduce


def read_file(filename):
    file1 = open(filename, 'r')
    res = []
    for line in file1:
        res.append(line.strip())

    return res


def parse_lines(lines):
    ruleset = {}
    for line in lines:
        (x,y) = line.split('->')
        ruleset[x.strip()] = y.strip()

    return ruleset


def split_template(template):
    res = {}
    for token in [ template[x] + template[y] for (x,y) in zip(range(len(template)), range(1, len(template)))]:
        if token in res:
            res[token] = 1 + res[token]
        else:
            res[token] = 1
    return res


def add_dicts(large, small):
    res = large.copy()
    for key in small.keys():
        if key in large:
            res[key] = res[key] + small[key]
        else:
            res[key] = small[key]
    return res

def mult_dict(k, dic):
    return { key:k*dic[key] for key in dic.keys()}

def insert_result(token, result):
    return token[0] + result + token[1]

def apply_rules(rules, tokens):
    result = {}
    for token in tokens.keys():
        if token in rules:
            applied = rules[token]
            result = add_dicts(result, mult_dict(tokens[token], split_template(insert_result(token, applied))))
        else:
            result = add_dicts(result, {token: tokens[token]})
    return result


def count_elements(tokens):
    counts = {}
    for token in tokens.keys():
        if '$' in token:
            continue

        element = token[1]
        if element in counts:
            counts[element] = tokens[token] + counts[element]
        else:
            counts[element] = tokens[token]
    return counts


lines = [
'CH -> B',
'HH -> N',
'CB -> H',
'NH -> C',
'HB -> C',
'HC -> B',
'HN -> C',
'NN -> C',
'BH -> H',
'NC -> B',
'NB -> B',
'BN -> B',
'BB -> N',
'BC -> B',
'CC -> N',
'CN -> C'
]
template = '^NNCB$'

template = '^CKFFSCFSCBCKBPBCSPKP$'
lines = read_file('input.txt')


rules = parse_lines(lines)

tokens = split_template(template)

for i in range(40):
    tokens = apply_rules(rules, tokens)




counts = count_elements(tokens)

print(max(counts.values()) - min(counts.values()))



# 10 in challenge: {'C': 1678, 'K': 956, 'N': 607, 'V': 1471, 'F': 705, 'O': 3162, 'P': 3854, 'H': 2778, 'B': 2953, 'S': 1293}
