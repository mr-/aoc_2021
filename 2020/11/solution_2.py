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
# If a seat is occupied(  # ) and five or more seats adjacent to it are also occupied, the seat becomes empty.
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
        if len(occupied[x] & occupied.keys()) >= 5:
            to_be_empty[x] = occupied[x]
        else:
            to_be_occupied[x] = occupied[x]
    return (to_be_occupied, to_be_empty)


def explore_dir(board, x, y, dx, dy):
    nx = x + dx
    ny = y + dy
    if nx < 0 or nx > board["w"]:
        return None
    if ny < 0 or ny > board["h"]:
        return None
    if (nx, ny) in board["seats"]:
        return (nx, ny)

    return explore_dir(board, nx, ny, dx, dy)


def get_candidates(board, p):
    dirs = [(dx, dy) for dx in [-1, 0, 1]
            for dy in [-1, 0, 1] if not (dx == 0 and dy == 0)]
    cand = [explore_dir(board, p[0], p[1], d[0], d[1]) for d in dirs]
    # print(f"{p}: {cand}")
    return set([c for c in cand if cand is not None])


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
