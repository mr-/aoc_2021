import functools

moves_input = "62,55,98,93,48,28,82,78,19,96,31,42,76,25,34,4,18,80,66,6,14,17,57,54,90,27,40,47,9,36,97,56,87,61,91,1,64,71,99,38,70,5,94,85,49,59,69,26,21,60,0,79,2,95,11,84,20,24,8,51,46,44,88,22,16,53,7,32,89,67,15,86,41,92,10,77,68,63,43,75,33,30,81,37,83,3,39,65,12,45,23,73,72,29,52,58,35,50,13,74"
moves = [int(m) for m in moves_input.split(',')]

def read_file(filename):
    file1 = open(filename, 'r')

    res = []
    for line in file1:
        res.append(line.strip())

    return res


def chunk_lines(lines):
    chunks = []
    current_chunk = []
    for line in lines:
        split = line.split()
        if len(split) < 2:
            chunks.append(current_chunk)
            current_chunk=[]
        else:
            current_chunk.append([int(n) for n in split])
    return chunks


def explode_chunk(chunk):
    copy = [c for c in chunk]
    for i in range(len(chunk)):
        copy.append([line[i] for line in chunk])
    return copy

def act_on_board(board, act):
    return [[n for n in line if n != act] for line in board]

def is_winning_board(board):
    return len([line for line in board if len(line) == 0]) > 0

def evaluate_board(board):
    return sum([c for line in board[:5] for c in line])

boards = [explode_chunk(chunk) for chunk in chunk_lines(read_file('input.txt'))]

winning_board_values = []
new_boards = []

for move in moves:
    for board in boards:
        acted = act_on_board(board, move)

        if(is_winning_board(acted)):
            value = evaluate_board(acted) * move
            winning_board_values.append(value)
        else:
            new_boards.append(acted)

    boards = new_boards
    new_boards = []
print(winning_board_values[0])

print(winning_board_values[len(winning_board_values)-1])
