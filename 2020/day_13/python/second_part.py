# import math
earliest = int(input())
shuttles = [(int(s),-i) for i,s in enumerate(input().split(',')) if s != 'x']


def euclide_algo(a, b):
    r, u, v = abs(a), 1, 0
    r2, u2, v2 = abs(b), 0, 1

    while r2 != 0:
        q = r // r2
        r, u, v, r2, u2, v2 = r2, u2, v2, r - q*r2, u - q*u2, v - q*v2
     
    return r, u, v

def solve_diophantine_equation(a, b, c):
    g, x0, y0 = euclide_algo(a, b)
    if c % g != 0:
        raise Exception("No solution")
    return (-b, c*x0 % b), (a, -(c*y0 % a))

def solve_problem(shuttles):
    result = solve_diophantine_equation(
        shuttles[0][0], 
        -shuttles[1][0], 
        -shuttles[0][1] + shuttles[1][1]
    )

    next_equation = (
        result[0][0] * shuttles[0][0],
        result[0][1] * shuttles[0][0] + shuttles[0][1]
    )

    if len(shuttles) == 2:
        return next_equation
    else:
        return solve_problem([next_equation] + shuttles[2:])

solution = solve_problem(shuttles)
print("solution", solution[0] + solution[1])