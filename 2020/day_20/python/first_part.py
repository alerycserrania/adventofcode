from enum import Enum
from typing import List, Dict, Tuple, Set
import itertools
import math

class Tile(Enum):
    OFF = '.'
    ON = '#'

    def __repr__(self):
        return self.value


class Rotation(Enum):
    ZERO = 0
    ONE = 1
    TWO = 2
    THREE = 3


class Flip(Enum):
    NONE = 0
    HORIZONTAL = 1
    VERTICAL = 2


TRANSFORMATIONS = list(itertools.product(Flip, Rotation))

RawImage = List[List[Tile]]
ImageID = Tuple[int, Flip, Rotation]


class Image:

    def __init__(
        self,
        idx: int,
        image: List[str],
        flip: Flip,
        rotation: Rotation
    ):
        self.idx = idx
        self.flip = flip
        self.rotation = rotation
        self.size = len(image)
        self.image = self._transform_img(image, flip, rotation)

    def bottom_matches(self, other: 'Image') -> bool:
        return self.image[self.size-1] == other.image[0]

    def top_matches(self, other: 'Image') -> bool:
        return self.image[0] == other.image[other.size-1]

    def right_matches(self, other: 'Image') -> bool:
        for row in range(self.size):
            if self.image[row][self.size-1] != other.image[row][0]:
                return False
        return True

    def left_matches(self, other: 'Image') -> bool:
        for row in range(self.size):
            if self.image[row][0] != other.image[row][self.size-1]:
                return False
        return True

    def _transform_img(
        self,
        image: List[str],
        flip: Flip,
        rotation: Rotation,
    ) -> RawImage:
        new_image = [[Tile(c) for c in row] for row in image]
        self._rotate_img(new_image, rotation)
        self._flip_img(new_image, flip)
        return new_image

    def _flip_img(
        self,
        image: RawImage,
        flip: Flip
    ) -> None:

        if flip == Flip.NONE:
            return

        if flip == Flip.HORIZONTAL:
            for col in range(self.size // 2):
                for row in range(self.size):
                    image[row][col], image[row][self.size - col - 1] = \
                        image[row][self.size - col - 1], image[row][col]

        if flip == Flip.VERTICAL:
            for row in range(self.size // 2):
                for col in range(self.size):
                    image[row][col], image[self.size - row - 1][col] = \
                        image[self.size - row - 1][col], image[row][col]

    def _rotate_img(
        self,
        image: RawImage,
        rotation: Rotation,
    ) -> None:
        for i in range(rotation.value):
            self._transpose_img(image)
            self._flip_img(image, Flip.HORIZONTAL)

    def _transpose_img(
        self,
        image: RawImage
    ) -> None:
        for row in range(self.size-1):
            for col in range(row+1, self.size):
                image[row][col], image[col][row] = image[col][row], image[row][col]

    def __repr__(self):
        return f"Tile {self.idx} ({self.flip}, {self.rotation}):\n{self.image}"


images: Dict[int, Dict[Tuple[Flip, Rotation], Image]] = dict()
all_images: List[Image] = list()
img_next_possibles: Dict[ImageID, Dict[str, Set[ImageID]]] = dict()

entry = input()
while entry != '-':
    idx = int(entry[5:-1])
    lines: List[str] = list()
    entry = input()
    while entry != '':
        lines.append(entry)
        entry = input()

    images[idx] = {
        (flip, rotation): Image(idx, lines, flip, rotation)
        for (flip, rotation) in TRANSFORMATIONS
    }

    all_images.extend([img for img in images[idx].values()])

    entry = input()

square_size = int(len(images) ** 0.5)


# filter by possible tile next to each other
for img in all_images:
    img_id = (img.idx, img.flip, img.rotation)
    img_next_possibles[img_id] = {
        "top": set(),
        "bottom": set(),
        "left": set(),
        "right": set(),
    }

    for other in all_images:
        other_id = (other.idx, other.flip, other.rotation)
        if img.idx != other.idx:
            if img.bottom_matches(other):
                img_next_possibles[img_id]["bottom"].add(other_id)
            elif img.top_matches(other):
                img_next_possibles[img_id]["top"].add(other_id)
            elif img.right_matches(other):
                img_next_possibles[img_id]["right"].add(other_id)
            elif img.left_matches(other):
                img_next_possibles[img_id]["left"].add(other_id)



possibilities: List[List[Set[int]]] = [
    [set() for _ in range(square_size)]
    for _ in range(square_size)
]

for row in range(square_size):
    for col in range(square_size):
        for img_id, n in img_next_possibles.items():
            ok = True
            if row == 0:
                ok &= len(n['top']) == 0
            else:
                ok &= len(n['top']) > 0

            if row == square_size - 1:
                ok &= len(n['bottom']) == 0
            else:
                ok &= len(n['bottom']) > 0

            if col == 0:
                ok &= len(n['left']) == 0
            else:
                ok &= len(n['left']) > 0

            if col == square_size - 1:
                ok &= len(n['right']) == 0
            else:
                ok &= len(n['right']) > 0

            if ok:
                possibilities[row][col].add(img_id[0])


state_changed = True
verified = [
    [False for _ in range(square_size)]
    for _ in range(square_size)
]


while state_changed:
    state_changed = False
    tiles_to_remove: Set[Tuple[Tuple[int, int], int]] = set()
    for row in range(square_size):
        for col in range(square_size):
            if len(possibilities[row][col]) == 1 and not verified[row][col]:
                idx = next(iter(possibilities[row][col]))
                tiles_to_remove.add(((row, col), idx))
                state_changed = True
                verified[row][col] = True

    for row in range(square_size):
        for col in range(square_size):
            for (pos, idx) in tiles_to_remove:
                if pos != (row, col):
                    possibilities[row][col].discard(idx)

# Because you can rotate and flip any tiles,
# there must be 4 valid positions for each corner
# corresponding to the 4 tile id in the corners
print("solution:", math.prod(possibilities[0][0]))
