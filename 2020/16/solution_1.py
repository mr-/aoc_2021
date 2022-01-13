from os import read, stat
import re
import pprint


def read_file(filename):
    file = open(filename, 'r')
    blocks = file.read().split('\n\n')

    rules = blocks[0].split('\n')
    tickets = blocks[2].split('\n')[1:]

    return {"tickets": [parse_ticket(t) for t in tickets if len(t) > 2], "rules": [parse_rule(r)for r in rules if len(r) > 2]}


def parse_ticket(line):
    return [int(n) for n in line.split(',')]


def parse_rule(line):
    m = re.fullmatch(r"([^:]+): (.+)", line)
    if m is None:
        return None
    name = m.groups()[0]
    ranges_string = m.groups()[1]
    ranges = ranges_string.split(' or ')
    res = []
    for r in ranges:
        lower, higher = r.split('-')
        res.append((int(lower), int(higher)))

    return {"name": name, "ranges": res}


def apply(rule, t):
    return any([r[0] <= t and t <= r[1] for r in rule["ranges"]])


def is_invalid(rules, nr):
    for rule in rules:
        if apply(rule, nr):
            return None
    return nr


def get_invalids(rules, ticket):
    invalids = [is_invalid(rules, t) for t in ticket]
    return [i for i in invalids if i is not None]


data = read_file('input.txt')
tickets = data["tickets"]
s = 0
valid_tickets = []
for ticket in tickets:
    ts = get_invalids(data["rules"], ticket)
    s = s + sum(ts)
    if len(ts) == 0:
        valid_tickets.append(ticket)

print(f"solution 1 is {s}")
print(f"{len(valid_tickets)} left of {len(tickets)}")


def apply_to_all(rule, ns):
    results = [apply(rule, n) for n in ns]
    return all(results)


def get_nth_entry(tickets, n):
    return [t[n] for t in tickets]


assignments = {}
for n in range(len(tickets[0])):
    for rule in data["rules"]:
        if apply_to_all(rule, get_nth_entry(valid_tickets, n)):
            # print(f"{rule['name']} works for {n}")
            if n in assignments:
                assignments[n].add(rule['name'])
            else:
                assignments[n] = set([rule['name']])
    if not n in assignments:
        print("WTF!")


def prune(assignments):
    singletons = set([list(rules)[0]
                     for rules in assignments.values() if len(rules) == 1])
    if len(singletons) == 0:
        return
    altered = False
    for i in assignments:
        if len(assignments[i]) > 1:
            altered = True
            assignments[i] = assignments[i] - singletons

    if altered:
        prune(assignments)


# optimization.. get the easy wins out of the way first: Remove obvious (singleton) choices we can't make
prune(assignments)

# The hero of this story later learns, that there is only one assignment left..
# so there's not much left to 'pick'. The pruning was done for performance optimizations to
# reduce the search space.. I guess that part worked out well.


def pick_assignment(assignments, max_index, index=0, seen={}):
    if index >= max_index:
        return []
    candidates = set(assignments[index]) - seen
    if len(candidates) == 0:
        return None

    result = []
    for candidate in candidates:
        rs = pick_assignment(assignments, max_index, index +
                             1, seen | set([candidate]))
        if rs is not None:
            if len(rs) == 0:
                result = result + [[candidate]]
            else:
                result = result + [[candidate] + r for r in rs]

    return None if len(result) == 0 else result


res = pick_assignment(assignments, len(tickets[0]), 0, set())[0]
print(res)


my_ticket = [73, 53, 173, 107, 113, 89, 59, 167, 137, 139,
             71, 179, 131, 181, 67, 83, 109, 127, 61, 79]

s = 1
for i in range(len(my_ticket)):
    if res[i].startswith("departure"):
        s = s * my_ticket[i]

print(f"solution 2 is {s}")
