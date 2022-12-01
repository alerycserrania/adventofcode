import itertools
from collections import defaultdict
from typing import Set, Dict, Tuple


Coordinates = Tuple[int, ...]


NB_DIMS = 4
NEIGHBOURS = set(itertools.product((-1, 0, 1), repeat=NB_DIMS)) - {(0,) * NB_DIMS}
NB_CYCLES = 6


def add_coordinate(c1: Coordinates, c2: Coordinates) -> Coordinates:
    return tuple(map(sum, zip(c1, c2)))


actives: Set[Coordinates] = set()

y = 0
entry = input()
while entry != '-':
    actives.update({
        (x, y,) + ((0,) * (NB_DIMS - 2)) for x, c in enumerate(entry)
        if c == '#'
    })
    y += 1
    entry = input()

for _ in range(NB_CYCLES):
    to_check: Set[Coordinates] = set()
    nb_active_neighbours: Dict[Coordinates, int] = defaultdict(lambda: 0)
    actives_to_remove: Set[Coordinates] = set()
    actives_to_add: Set[Coordinates] = set()

    for coords in actives:
        for jump in NEIGHBOURS:
            n_coords = add_coordinate(coords, jump)
            nb_active_neighbours[n_coords] += 1
            to_check.add(n_coords)
        to_check.add(coords)

    for coords in to_check:
        if coords in actives:
            if not (2 <= nb_active_neighbours[coords] <= 3):
                actives_to_remove.add(coords)
        else:
            if nb_active_neighbours[coords] == 3:
                actives_to_add.add(coords)
    
    actives.update(actives_to_add)
    actives.difference_update(actives_to_remove)

print("solutions:", len(actives))