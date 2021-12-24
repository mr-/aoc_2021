from functools import reduce
import networkx as nx
from random import randrange
# G = nx.Graph()
from multiprocessing import Pool


costs = {"A": 1,
         "B": 10,
         "C": 100,
         "D": 1000,
         }
corridor = [(0, 0),
            (1, 0),
            (3, 0),
            (5, 0),
            (7, 0),
            (9, 0),
            (10, 0)]


room1 = [(2, 1), (2, 2)]
room2 = [(4, 1), (4, 2)]
room3 = [(6, 1), (6, 2)]
room4 = [(8, 1), (8, 2)]

rooms = {"A": room1,
         "B": room2,
         "C": room3,
         "D": room4,
         }


initial_board = {
    (0, 0): "",
            (1, 0): "",
    # (2, 0): "",
            (3, 0): "",
            # (4, 0): "",
            (5, 0): "",
            # (6, 0): "",
            (7, 0): "",
            # (8, 0): "",
            (9, 0): "",
            (10, 0): "",

            (2, 1): "B",
            (2, 2): "A",

            (4, 1): "C",
            (4, 2): "D",

            (6, 1): "B",
            (6, 2): "C",

            (8, 1): "D",
            (8, 2): "A",
}


end_board = {
    (0, 0): "",
            (1, 0): "",
            (3, 0): "",
            (5, 0): "",
            (7, 0): "",
            (9, 0): "",
            (10, 0): "",

            (2, 1): "A",
            (2, 2): "A",

            (4, 1): "B",
            (4, 2): "B",

            (6, 1): "C",
            (6, 2): "C",

            (8, 1): "D",
            (8, 2): "D",
}


weird_board = {
    (0, 0): "",
    (1, 0): "B",
    (3, 0): "A",
    (5, 0): "D",
    (7, 0): "",
    (9, 0): "D",
    (10, 0): "B",

    (2, 1): "",
    (2, 2): "A",

    (4, 1): "",
    (4, 2): "",

    (6, 1): "C",
    (6, 2): "C",

    (8, 1): "",
    (8, 2): "",
}

############
# .B#A#D#.#DB
###.#.#C#.##
###A#.#C#.##


#############
#...........#
###B#C#C#B###
#D#D#A#A#
#########

puzzle_input = {
    (0, 0): "",
            (1, 0): "",
            (3, 0): "",
            (5, 0): "",
            (7, 0): "",
            (9, 0): "",
            (10, 0): "",

            (2, 1): "B",
            (2, 2): "D",

            (4, 1): "C",
            (4, 2): "D",

            (6, 1): "C",
            (6, 2): "A",

            (8, 1): "B",
            (8, 2): "A",
}


def explore_paths(ps, piece, ptype):
    if piece in corridor:
        # print(f"{piece} in corridor")
        val = move_to_room(ps, piece, ptype)
        # print(f"which got moves {val}")
        return val
    elif piece in rooms[ptype]:
        # piece is in own room
        # print("In own room..")
        if piece[1] != 2:
            if ps[(piece[0], 2)] == "":
                return ([(piece[0], 2)], costs[ptype]),
            if ps[(piece[0], 2)] != ptype:
                return leave_room(ps, piece, ptype)

        return []
    else:
        return leave_room(ps, piece, ptype)


def move_to_room(ps, piece, ptype):
    # can only go to own room.
    x_room = rooms[ptype][0][0]
    first_empty = ps[rooms[ptype][0]] == ""
    second_empty = ps[rooms[ptype][1]] == ""
    second_same_type = ps[rooms[ptype][1]] == ptype

    if not first_empty:
        # print("first not empty..")
        return []

    if not second_empty and not second_same_type:
        return []

    if piece[0] > x_room:
        #   p
        # #####
        path = [(x, 0) for x in reversed(range(x_room, piece[0]))]
    if piece[0] < x_room:
        #   p     #
        ########  #
        path = [(x, 0) for x in range(piece[0]+1, x_room+1)]

    path_clear = len(
        [1 for step in path if step in ps and ps[step] != ""]) == 0
    if not path_clear:
        return []
    # path is clear.. let's go in.
    if second_empty:
        return [([path[0], rooms[ptype][1]], costs[ptype] * (len(path) + 2))]
    else:
        val = [([path[0], rooms[ptype][0]], costs[ptype] * (len(path) + 1))]
        return val


def leave_room(ps, piece, ptype):
    if piece[1] == 2:
        if ps[(piece[0], 1)] != "":
            return []

    xp = piece[0]
    right_paths = []
    for x in range(xp, 10+1):
        if (x, 0) in ps and ps[(x, 0)] != "":
            break
        if not (x, 0) in ps:
            continue
        if len(right_paths) == 0:
            right_paths = [[(x, 0)]]
        else:
            right_paths = right_paths + [[*right_paths[-1], (x, 0)]]

    left_paths = []
    for x in reversed(range(0, xp+1)):
        if (x, 0) in ps and ps[(x, 0)] != "":
            break
        if not (x, 0) in ps:
            continue
        if len(left_paths) == 0:
            left_paths = [[(x, 0)]]
        else:
            left_paths = left_paths + [[*left_paths[-1], (x, 0)]]

    paths_to_corridor = [([x[0], x[-1]], (piece[1] + abs(xp - x[-1][0])) * costs[ptype])
                         for x in (left_paths + right_paths)]

    continuations = [([p[0], q[-1]], pcost+qcost) for p, pcost in paths_to_corridor for q,
                     qcost in explore_paths(ps, p[-1], ptype)]
    return paths_to_corridor + continuations


def pieces(ps):
    return [(p, q) for p, q in ps.items() if q != ""]


def get_all_moves(ps):
    # moves = sorted([([p] + path[0], path[1]) for p, ptype in self.pieces()
    #                for path in self.explore_paths(p, ptype)], key=lambda x: x[0])

    moves = [([p] + path[0], path[1]) for p, ptype in pieces(ps)
             for path in explore_paths(ps, p, ptype)]

    prio_moves = [(path, costs)
                  for path, costs in moves if path[-1][1] > 0]
    rest = [(path, costs) for path, costs in moves if path[-1][1] == 0]

    return prio_moves + rest


def end_positions(ps) -> int:
    return len([1 for ptype, room in rooms.items() for pos in room if ps[pos] == ptype])


def do_move(board, move):
    ps = board.copy()
    ps[move[0][-1]] = ps[move[0][0]]
    ps[move[0][0]] = ""
    return ps


def pretty_print(ps):
    max_x = max([x for (x, y) in ps.keys()])
    max_y = max([y for (x, y) in ps.keys()])
    for y in range(-1, max_y + 1):
        for x in range(-1, max_x + 1):
            if (x, y) in ps:
                if ps[(x, y)] == "":

                    print(".", end='')
                else:
                    print(ps[(x, y)], end='')
            else:
                print("#", end='')
        print()


global_min = None


def memoize(f):
    memo = {}

    def helper(x):
        hashable = tuple(x.items())
        if hashable not in memo:
            memo[hashable] = f(x)
        return memo[hashable]
    return helper


def memoize2(f):
    memo = {}

    def helper(x, y=0, ):
        hashable = tuple(x.items())
        if hashable not in memo:
            memo[hashable] = f(x, y,)
        return memo[hashable]
    return helper


haha = memoize(get_all_moves)


def get_winning_costs(board, depth=0):
    good_positions = end_positions(board)
    if good_positions == 8:
        print("HERE")
        return 0
    moves = haha(board)
    if len([1 for path, c in moves if len(path) > 1]) == 0:
        return None

    res = []
    i = 0
    for move in moves:
        if depth == 0:
            print(f"{i}/{len(moves)}")
        if depth == 1:
            print(f"    {i}/{len(moves)}")
        i = i + 1

        new_board = do_move(board, move)
        new_cost = baha(new_board, depth+1)
        if new_cost is not None:
            res.append(new_cost + move[1])
            if depth == 0:
                print(f"COST {new_cost + move[1]}")

    return min(res) if len(res) > 0 else None


baha = memoize2(get_winning_costs)

# bar = Board(end_board, 0)
# print(bar.is_end_board())

foo = {"ps": puzzle_input}
# foo = {"ps": initial_board}

pretty_print(foo["ps"])

print(baha(foo["ps"]))


# b = Board(weird_board, 0)
# b.pretty_print()

# for p in b.get_all_moves():
#     new = b.move(p)
#     print(p)
#     new.pretty_print()
#     input()
