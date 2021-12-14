import functools

def memoize(f):
    memo = {}
    def helper(x):
        if x not in memo:
            memo[x] = f(x)
        return memo[x]
    return helper

def calc(n, start_iteration = 0):
    m = start_iteration + n
    if m <= 0:
        return 0
    if m <= 9:
        return 1
    return mem_calc(m-1) + mem_calc(m-7) - mem_calc(m-8) + mem_calc(m-9) - mem_calc(m-10)

mem_calc = memoize(calc)

def calc_from_start(n, start_iteration =0):
    return mem_calc(n + start_iteration)

def calc_from_timer(n, timer):
    return calc_from_start(n+1, start_iteration = (9-timer-1))


#  0
#  1  8
#  2  7
#  3  6
#  4  5
#  5  4
#  6  3
#  7  2
#  8  1
#  9  0
# 10  68
# 11  57
# 12  46
# 13  35
# 14  24
# 15  13
# 16  02
# 17  618
# 18  507
# 19  4668

# parameters: 7, 9
# a_n = 0 for n <= 0
# a_n = 1 for n <= 9
# a_n = a_{n-1} + a_{n-7} - a_{n-8} + a_{n-9} - a_{n-10}




start_timer = 2

# timers = [3,4,3,1,2]
timers = [4,1,1,1,5,1,3,1,5,3,4,3,3,1,3,3,1,5,3,2,4,4,3,4,1,4,2,2,1,3,5,1,1,3,2,5,1,1,4,2,5,4,3,2,5,3,3,4,5,4,3,5,4,2,5,5,2,2,2,3,5,5,4,2,1,1,5,1,4,3,2,2,1,2,1,5,3,3,3,5,1,5,4,2,2,2,1,4,2,5,2,3,3,2,3,4,4,1,4,4,3,1,1,1,1,1,4,4,5,4,2,5,1,5,4,4,5,2,3,5,4,1,4,5,2,1,1,2,5,4,5,5,1,1,1,1,1,4,5,3,1,3,4,3,3,1,5,4,2,1,4,4,4,1,1,3,1,3,5,3,1,4,5,3,5,1,1,2,2,4,4,1,4,1,3,1,1,3,1,3,3,5,4,2,1,1,2,1,2,3,3,5,4,1,1,2,1,2,5,3,1,5,4,3,1,5,2,3,4,4,3,1,1,1,2,1,1,2,1,5,4,2,2,1,4,3,1,1,1,1,3,1,5,2,4,1,3,2,3,4,3,4,2,1,2,1,2,4,2,1,5,2,2,5,5,1,1,2,3,1,1,1,3,5,1,3,5,1,3,3,2,4,5,5,3,1,4,1,5,2,4,5,5,5,2,4,2,2,5,2,4,1,3,2,1,1,4,4,1,5]

print(sum([calc_from_timer(256, t) for t in timers]))
