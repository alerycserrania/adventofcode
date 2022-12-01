from typing import Set, Tuple, List

DIR_TO_OFFSET = {
    'e': (1, 1),
    'se': (0, 1),
    'sw': (-1, 0),
    'w': (-1, -1),
    'nw': (0, -1),
    'ne': (1, 0),
}

NB_DAYS = 100


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


for _ in range(NB_DAYS):
    tiles_to_check: Set[Tuple[int, int]] = {
        (t[0] + o[0], t[1] + o[1])
        for t in black_tiles
        for o in list(DIR_TO_OFFSET.values()) + [(0, 0)]
    }

    tiles_to_add: Set[Tuple[int, int]] = set()
    tiles_to_remove: Set[Tuple[int, int]] = set()

    for t in tiles_to_check:
        nb_black_tiles = sum(
            1 for o in DIR_TO_OFFSET.values()
            if (t[0] + o[0], t[1] + o[1]) in black_tiles
        )

        if t in black_tiles:
            if nb_black_tiles == 0 or nb_black_tiles > 2:
                tiles_to_remove.add(t)
        else:
            if nb_black_tiles == 2:
                tiles_to_add.add(t)

    black_tiles.update(tiles_to_add)
    black_tiles.difference_update(tiles_to_remove)


print("solutions:", len(black_tiles))
