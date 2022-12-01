from typing import Tuple

cos = {
    0: 1,
    90: 0,
    180: -1,
    270: 0
}

sin = {
    0: 0,
    90: 1,
    180: 0,
    270: -1
}

def mat_rot(deg):
    return (
        (cos[deg], -sin[deg]),
        (sin[deg], cos[deg]),
    )

def rotate(d: Tuple[int, int], r: int) -> Tuple[int, int]:
    rot = mat_rot(r)
    return (
        d[0] * rot[0][0] + d[1] * rot[0][1], 
        d[0] * rot[1][0] + d[1] * rot[1][1]
    )

actions = {
    'N': lambda val, p, d: ((p[0], p[1] - val), d),
    'S': lambda val, p, d: ((p[0], p[1] + val), d),
    'E': lambda val, p, d: ((p[0] + val, p[1]), d),
    'W': lambda val, p, d: ((p[0] - val, p[1]), d),
    'L': lambda val, p, d: (p, rotate(d, 360-val)),
    'R': lambda val, p, d: (p, rotate(d, val)),
    'F': lambda val, p, d: ((p[0] + d[0] * val, p[1] + d[1] * val), d),
}

pos = (0, 0)
direction = (1, 0)
instruction = input()
while instruction != '-':
    action = instruction[0]
    val = int(instruction[1:])
    pos, direction = actions[action](val, pos, direction)
    print(action, val, pos, direction)
    instruction = input()

print("solution:", abs(pos[0]) + abs(pos[1]))