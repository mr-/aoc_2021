import functools
from functools import reduce
import networkx as nx
import math
from itertools import dropwhile


puzzle_input = [ [[[[6,3],7],0],[[7,0],0]] ,[[[4,7],[6,[6,5]]],[4,[[6,5],[9,1]]]] ,[[[[3,7],[6,9]],[3,[4,1]]],8] ,[[[0,[2,0]],3],2] ,[[[[1,3],4],9],[[[1,8],[9,3]],[0,7]]] ,[[[5,9],1],[[[4,8],[4,8]],[[9,7],[3,6]]]] ,[[[0,7],4],[[[0,4],2],[4,2]]] ,[[5,1],[2,5]] ,[[[[4,8],[8,3]],[6,[2,3]]],[[6,0],[1,3]]] ,[[[[1,7],[8,1]],[2,[3,4]]],[[7,[8,7]],[[8,6],5]]] ,[[9,[[8,9],[0,6]]],[[[8,4],5],[0,[1,7]]]] ,[[[[9,8],[6,9]],[[3,5],[6,2]]],[[[7,8],5],8]] ,[[[[7,1],8],[0,2]],[3,5]] ,[[[1,9],3],[8,[8,[7,8]]]] ,[[[9,[8,5]],[1,[9,0]]],5] ,[9,[[[1,1],8],[[3,5],9]]] ,[[[1,[7,8]],2],[8,0]] ,[7,[8,[[6,1],[7,9]]]] ,[[7,[[4,7],7]],[[8,[5,2]],2]] ,[[[[1,7],[9,0]],3],[8,[4,[2,0]]]] ,[[4,3],[[9,[7,7]],7]] ,[[[[9,5],3],[[8,5],5]],5] ,[[[[4,9],2],[[5,6],[9,0]]],[[2,[2,2]],1]] ,[[[[7,9],[6,0]],[1,[5,8]]],[8,8]] ,[[[[0,5],2],[4,[5,7]]],[8,[[8,1],[6,7]]]] ,[9,[7,[[3,7],[6,4]]]] ,[[[[5,3],[5,2]],[[5,0],0]],[[[4,6],[6,4]],[8,8]]] ,[1,[[8,0],0]] ,[[3,[7,9]],[[[1,2],[1,6]],[[9,2],[2,8]]]] ,[[[2,8],[3,[8,3]]],[3,[[9,8],[5,6]]]] ,[[[[2,0],3],[5,1]],[[[2,9],7],[[0,4],[1,4]]]] ,[[3,[[0,0],[9,0]]],[[6,[0,5]],[[5,4],[5,2]]]] ,[[8,[[9,4],[6,8]]],[3,[[8,1],4]]] ,[[[[4,1],[5,1]],[4,[9,1]]],4] ,[2,[[[0,5],[8,7]],[[6,1],[1,0]]]] ,[7,[8,8]] ,[[[[7,3],5],9],[[[5,9],[3,5]],[[9,4],8]]] ,[[[[4,2],[6,1]],[[6,6],7]],[2,2]] ,[[[1,1],4],[[[4,8],0],[[6,7],7]]] ,[[[[5,9],[0,1]],[9,7]],2] ,[[[[6,4],[5,5]],[[2,8],0]],9] ,[[7,[4,9]],[5,[[0,2],2]]] ,[[2,[9,9]],[[8,5],8]] ,[3,[8,[6,[2,8]]]] ,[[[0,1],[[8,5],[8,9]]],0] ,[[[[1,8],[1,4]],8],[[6,8],[8,[5,7]]]] ,[[[[7,1],[2,0]],[2,4]],[[[3,7],[2,2]],1]] ,[[[[0,3],[8,0]],[2,6]],[[6,2],[0,4]]] ,[[[7,1],[[6,8],[3,2]]],[[8,[3,2]],[1,[0,1]]]] ,[[[8,[5,7]],4],[[1,[1,9]],[[4,9],[4,2]]]] ,[[[8,5],3],[[9,8],[[4,4],[7,2]]]] ,[[5,[5,[8,7]]],[[8,0],[[6,3],6]]] ,[[[8,4],[[5,2],[7,0]]],[[[9,7],[8,9]],7]] ,[[5,[3,[3,0]]],5] ,[[[[0,1],[0,0]],[4,[3,7]]],[8,0]] ,[5,[[[8,3],8],5]] ,[[[5,[0,6]],4],[4,[3,[6,3]]]] ,[[[[4,1],[3,5]],[[5,0],7]],[[[7,9],[8,8]],[[8,0],[5,8]]]] ,[[[7,[6,6]],[[6,2],2]],8] ,[1,[4,[[2,9],[3,8]]]] ,[[[9,[5,4]],[[2,5],7]],[[4,2],9]] ,[[9,1],[[0,7],[[4,1],[5,6]]]] ,[[[7,[8,6]],[[4,5],1]],[[[2,1],[6,3]],[4,[0,3]]]] ,[[0,[2,[7,5]]],[[[5,0],5],3]] ,[[[0,[4,1]],7],[[[3,7],5],[[5,9],7]]] ,[[4,[[1,3],1]],[[9,6],[[6,2],3]]] ,[3,[[[8,5],6],4]] ,[[2,7],[[[3,9],3],[[2,1],2]]] ,[[4,[5,[9,9]]],[[[3,0],[4,1]],[[6,4],9]]] ,[[[7,9],[[5,7],[2,1]]],[[[1,4],6],[3,3]]] ,[[[[4,0],3],[3,[6,3]]],[[[6,2],4],5]] ,[[9,[[2,4],3]],[[9,[1,0]],[[5,8],[7,1]]]] ,[8,[1,5]] ,[[[5,5],3],[[[8,9],3],4]] ,[[1,[[8,8],[7,4]]],[[[9,1],[9,6]],[7,8]]] ,[[[7,2],[[0,1],5]],7] ,[[9,3],[7,[1,[9,2]]]] ,[[[[8,2],[8,0]],[[7,1],6]],[0,[[9,4],1]]] ,[[[[1,3],2],[6,[0,0]]],[[[7,8],[4,3]],[[9,4],1]]] ,[[[[2,1],[7,1]],[[5,4],[9,2]]],[[3,0],0]] ,[[[8,6],[[2,1],[9,1]]],[[3,[1,8]],[3,3]]] ,[[[[6,2],[3,6]],3],[5,9]] ,[[[[7,6],7],[[4,2],1]],[[0,4],[[9,8],6]]] ,[[8,[[7,6],[8,5]]],[[2,7],[[7,0],[0,0]]]] ,[[[0,[0,0]],4],[[[6,1],8],[[5,9],[6,5]]]] ,[[[8,[9,9]],9],[[3,[2,8]],[[9,5],[2,9]]]] ,[[[7,5],[[0,7],[3,3]]],[[8,[1,5]],9]] ,[[[6,[9,0]],[[0,7],[7,5]]],[[[4,9],0],[[2,3],7]]] ,[[8,[8,[0,1]]],[[2,6],7]] ,[1,[[9,3],1]] ,[[[[6,2],[0,8]],5],[[[9,4],7],[[6,3],6]]] ,[[[8,[4,2]],[4,9]],[7,[[9,0],8]]] ,[1,[[7,3],2]] ,[[[3,[5,9]],[7,[0,4]]],[[[3,0],7],4]] ,[[[1,[8,3]],[4,[2,3]]],[[3,[1,6]],[[1,6],5]]] ,[[[[1,3],2],9],[5,[4,9]]] ,[[[7,[0,6]],2],[[[1,5],[0,7]],[4,9]]] ,[[[5,0],[0,[1,1]]],5] ,[[[[9,3],[0,0]],[[1,3],[7,3]]],[[5,7],[[6,6],[6,5]]]] ,[[[8,0],[9,0]],[[[4,7],4],[5,[2,6]]]] ]

def from_nested_lists(l):
    if isinstance(l, list):
         if len(l) > 2:
             print("ERROR: More than two children...")
         return {
             "type": "inner",
             "c": [from_nested_lists(l[0]), from_nested_lists(l[1])]
         }
    else:
        return {
            "type": "leaf",
            "value": int(l)
            }

def pretty_print(tree):
    if tree["type"] == "inner":
        l_res = pretty_print(tree["c"][0])
        r_res = pretty_print(tree["c"][1])
        return f"[{l_res}, {r_res}]"
    else:
        return str(tree["value"])

def find_pair(tree, prop, depth=0, part_pair=False):
    if tree["type"] == "inner":
        is_pair = (not isinstance(tree["c"][0], list)) and (not isinstance(tree["c"][1], list))
        if prop([tree["c"][0], tree["c"][1]], depth):
            return []

        l_res = find_pair(tree["c"][0], prop, depth+1)
        if not l_res is None:
            return [0] + l_res

        r_res = find_pair(tree["c"][1],prop, depth+1)
        if not r_res is None:
            return [1] + r_res
    else:
        return None

def find_gt_9(tree):
    if tree["type"] == "inner":
        l_res = find_gt_9(tree["c"][0])
        if not l_res is None:
            return [0] + l_res

        r_res = find_gt_9(tree["c"][1])
        if not r_res is None:
            return [1] + r_res
    else:
        if tree["value"] > 9:
            return []

def magnitude(tree):
    if tree["type"] == "inner":
        l_res = magnitude(tree["c"][0])
        r_res = magnitude(tree["c"][1])

        return 3*l_res + 2*r_res
    else:
        return tree["value"]

def follow_direction(tree, direction):
    if tree["type"] == "inner":
        return [direction] + follow_direction(tree["c"][direction], direction)
    else:
        return []

def manipulate(tree, path, func):
    if len(path) == 0:
        new_tree_part = func(tree)
        tree["type"] = new_tree_part["type"]
        tree["c"] = new_tree_part.get("c")
        tree["value"] = new_tree_part.get("value")
    else:
        manipulate(tree["c"][path[0]], path[1:], func)


def get_via_path(tree, path):
    if len(path) == 0:
        return tree
    else:
        return get_via_path(tree["c"][path[0]], path[1:])

def find_left_of(tree, path):
    prefix_to_left = list(reversed(list(dropwhile(lambda step: step == 0, reversed(path)))))

    if len(prefix_to_left) == 0:
        return None
    prefix_to_left[-1] = 0

    right_from_here = get_via_path(tree, prefix_to_left)
    right_most = follow_direction(right_from_here, 1)
    path_to_left_element = prefix_to_left + right_most

    return path_to_left_element


def find_right_of(tree, path):
    prefix_to_right = list(reversed(list(dropwhile(lambda step: step == 1, reversed(path)))))

    if len(prefix_to_right) == 0:
        return None
    prefix_to_right[-1] = 1

    right_from_here = get_via_path(tree, prefix_to_right)
    right_most = follow_direction(right_from_here, 0)
    path_to_left_element = prefix_to_right + right_most

    return path_to_left_element


def explode(tree):
    path = find_pair(tree, (lambda v, l: l == 4))
    if path is None:
        return False
    pair = get_via_path(tree, path)

    left = find_left_of(tree, path)
    right = find_right_of(tree, path)
    if not left is None:
        manipulate(tree, left, lambda x: {"type": "leaf", "value": pair["c"][0]["value"] + x["value"]})
    if not right is None:
        manipulate(tree, right, lambda x: {"type": "leaf", "value": pair["c"][1]["value"] + x["value"]})

    manipulate(tree, path, lambda x: {"type": "leaf", "value": 0})
    return True

def split(tree):
    path = find_gt_9(tree)
    if path is None:
        return False
    manipulate(tree, path, lambda x: {
        "type": "inner",
        "c": [
            {
                "type": "leaf",
                "value": int(x["value"]/2)
            },
            {
                "type": "leaf",
                "value": math.ceil(x["value"]/2)
            },
        ]

            })
    return True

def reduce_tree(tree):
    cont = True
    while cont:
        exploded = explode(tree)
        if not exploded:
            cont = split(tree)

    return tree

def add(t1, t2):
    return {"type": "inner", "c": [t1,t2]}




# examples = [
#     [[[[[9,8],1],2],3],4],
#     [7,[6,[5,[4,[3,2]]]]],
#     [[6,[5,[4,[3,2]]]],1],
#     [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]],
#     [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
# ]

# for lists in examples:
#     print(lists)
#     tree = from_nested_lists(lists)
#     explode(tree)
#     print(pretty_print(tree))
#     print()


print(magnitude(from_nested_lists([[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]])))
acc = from_nested_lists(puzzle_input[0])
for l in puzzle_input[1:]:
    tree = add(acc, from_nested_lists(l))
    reduce_tree(tree)
    acc = tree

print(magnitude(acc))


maximum = 0
for l in puzzle_input:
    for r in puzzle_input:
        if l != r:
            tree = add(from_nested_lists(l), from_nested_lists(r))
            reduce_tree(tree)
            mag = magnitude(tree)
            if mag > maximum:
                maximum = mag

print(f"maximum is {maximum}")
