import functools


def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res

def parse_command(line):
    [command, value]= line.split()
    return {"command": command, "value": int(value)}

def apply_command(state, command):

    if command["command"] == "up":
        return {
            "aim": state["aim"] -  command["value"],
            "x": state["x"],
            "depth": state["depth"]
        }
    if command["command"] == "down":
        return {
            "aim": state["aim"] + command["value"],
            "x": state["x"],
            "depth": state["depth"]
        }
    if command["command"] == "forward":
        return {
            "aim": state["aim"],
            "depth": state["depth"] + state["aim"] * command["value"],
            "x": state["x"] + command["value"]
        }


state = {"aim": 0, "depth": 0, "x": 0}
commands = [parse_command(line) for line in read_file('input.txt')]


res = functools.reduce(lambda x ,y : apply_command(x, y) , commands, state)

print(res["depth"] * res["x"])
