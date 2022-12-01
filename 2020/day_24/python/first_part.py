from typing import Set, Tuple, List

DIR_TO_OFFSET = {
    'e': (1, 1),
    'se': (0, 1),
    'sw': (-1, 0),
    'w': (-1, -1),
    'nw': (0, -1),
    'ne': (1, 0),
}


def parse_path(entry: str) -> List[str]:
    path = []
    i = 0
    while i < len(entry):
        if entry[i] in {'e', 'w'}:
            path.append(entry[i])
        else:
            path.append(entry[i]+entry[i+1])
            i += 1
        i += 1
    return path


black_tiles: Set[Tuple[int, int]] = set()


entry = input()
while entry != '-':
    path = parse_path(entry)
    ref = (0, 0)
    for d in path:
        offset = DIR_TO_OFFSET[d]
        ref = (ref[0] + offset[0], ref[1] + offset[1]) 

    if ref in black_tiles:
        black_tiles.remove(ref)
    else:
        black_tiles.add(ref)

    entry = input()

print("solutions:", len(black_tiles))
