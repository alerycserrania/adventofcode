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

def mat_rot(deg: int):
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
    'N': lambda val, p, wp: (p, (wp[0], wp[1] - val)),
    'S': lambda val, p, wp: (p, (wp[0], wp[1] + val)),
    'E': lambda val, p, wp: (p, (wp[0] + val, wp[1])),
    'W': lambda val, p, wp: (p, (wp[0] - val, wp[1])),
    'L': lambda val, p, wp: (p, rotate(wp, 360-val)),
    'R': lambda val, p, wp: (p, rotate(wp, val)),
    'F': lambda val, p, wp: ((p[0] + wp[0] * val, p[1] + wp[1] * val), wp),
}

pos = (0, 0)
waypoint = (10, -1)
instruction = input()
while instruction != '-':
    action = instruction[0]
    val = int(instruction[1:])
    pos, waypoint = actions[action](val, pos, waypoint)
    instruction = input()

print("solution:", abs(pos[0]) + abs(pos[1]))