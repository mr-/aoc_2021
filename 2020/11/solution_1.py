from functools import cache


def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res


def parse_lines(lines):
    seats = set()
    for x in range(len(lines[0])):
        for y in range(len(lines)):
            if lines[y][x] == "L":
                seats.add((x, y))

    return {
        "h": len(lines),
        "w": len(lines[0]),
        "seats": seats,
    }


@cache
def candidates(w, h, x, y):
    def passable(nx, ny):
        if nx == x and ny == y:
            return False
        if nx > w or nx < 0:
            return False
        if ny > h or ny < 0:
            return False
        return True

    pot = [(x+dx, y+dy) for dx in [-1, 0, 1] for dy in [-1, 0, 1]]

    return set([p for p in pot if passable(p[0], p[1])])


def pretty_print(max_x, max_y, occupied, empty):

    for y in range(max_y):
        for x in range(max_x):
            if (x, y) in empty:
                print("L",  end='')
            elif (x, y) in occupied:
                print("#",  end='')
            else:
                print(".",  end='')
        print()


# If a seat is empty(L) and there are no occupied seats adjacent to it, the seat becomes occupied.
# If a seat is occupied(  # ) and four or more seats adjacent to it are also occupied, the seat becomes empty.
# Otherwise, the seat's state does not change.
def advance(w, h, occupied, empty):
    to_be_occupied = {}
    to_be_empty = {}
    for x in empty.keys():
        if len(empty[x] & occupied.keys()) == 0:
            to_be_occupied[x] = empty[x]
        else:
            to_be_empty[x] = empty[x]

    for x in occupied.keys():
        if len(occupied[x] & occupied.keys()) >= 4:
            to_be_empty[x] = occupied[x]
        else:
            to_be_occupied[x] = occupied[x]
    return (to_be_occupied, to_be_empty)


def get_candidates(board, p):
    return candidates(board["w"], board["h"], p[0], p[1])


def add_candidates(board):
    new_empties = {}
    for pos in board["seats"]:
        new_empties[pos] = get_candidates(board, pos)

    return new_empties


lines = read_file("input.txt")
board = parse_lines(lines)

initial_empty = add_candidates(board)
h = board["h"]
w = board["w"]

occupied = {}

state = (occupied, initial_empty)
c = 0
while True:
    new_state = advance(w, h, state[0], state[1])
    c = c + 1
    if (new_state[0].keys() == state[0].keys()):
        print(f"Done in step {c} with {len(new_state[0])} occupied seats")
        # pretty_print(w, h, new_state[0], new_state[1])
        exit(1)
    state = new_state
