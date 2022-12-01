import math
from functools import reduce
from typing import List


TREE = '#'
OPEN_SQUARE = '.'


class Location:

    def __init__(self, x: int, y: int):
        self._x = x
        self._y = y

    @property
    def x(self) -> int:
        return self._x

    @property
    def y(self) -> int:
        return self._y

    def __add__(self, other: 'Location') -> 'Location':
        return Location(self.x + other.x, self.y + other.y)

    def __repr__(self) -> str:
        return f"(Location: x={self.x}, y={self.y})"


class Slope(Location):
    
    def __init__(self, right: int, down: int):
        super().__init__(right, down)

    @property
    def right(self) -> int:
        return self.x

    @property
    def down(self) -> int:
        return self.y


class Toboggan:

    def __init__(self, tree_map: 'TreeMap', slope: Slope, location: Location):
        self._tree_map = tree_map
        self._location = location
        self._slope = slope

    def __iter__(self):
        return self

    def __next__(self) -> str:
        self._location += self._slope
        if self._location.y >= len(self._tree_map.raw_map):
            raise StopIteration()
        self._location = Location(
            self._location.x % len(self._tree_map.raw_map[self._location.y]),
            self._location.y
        )
        return self._tree_map.raw_map[self._location.y][self._location.x]


class TreeMap:

    def __init__(self, raw_map: List[str]):
        self._raw_map = raw_map

    @property
    def raw_map(self):
        return self._raw_map

    def toboggan(self, slope: Slope, location: Location = Location(0, 0)) -> Toboggan:
        return Toboggan(self, slope, location)


def nb_trees_with_slope(tree_map: TreeMap, slope: Slope):
    return sum(1 for block in tree_map.toboggan(slope) if block == TREE)


entries = []

entry = input()
while entry != '-':
    entries.append(entry)
    entry = input()

SLOPES = (
    Slope(1, 1),
    Slope(3, 1),
    Slope(5, 1),
    Slope(7, 1),
    Slope(1, 2),
)

tree_map = TreeMap(entries)

solution = math.prod(
    (nb_trees_with_slope(tree_map, slope) for slope in SLOPES)
)

print("solution:", solution)