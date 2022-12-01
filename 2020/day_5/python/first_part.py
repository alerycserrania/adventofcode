from typing import List


class Seat:
    
    def __init__(self, row: int, column: int):
        self._row = row
        self._column = column

    @property
    def row(self) -> int:
        return self._row

    @property
    def column(self) -> int:
        return self._column

    def __repr__(self) -> str:
        return f"(Seat row={self.row}, column={self.column})"


class Plane:

    ROW_MAPPING = {
        "F": 0,
        "B": 1,
    }

    COLUMN_MAPPING = {
        "L": 0,
        "R": 1
    }

    def __init__(self, nb_bits_row: int, nb_bits_column: int):
        self._nb_bits_row = nb_bits_row
        self._nb_bits_column = nb_bits_column

    def get_seat_from_partition(self, partition: str) -> Seat:
        return Seat(
            sum(self.ROW_MAPPING[c] * (1 << (self._nb_bits_row-i-1)) for i, c in enumerate(partition[:self._nb_bits_row])),
            sum(self.COLUMN_MAPPING[c] * (1 << (self._nb_bits_column-i-1)) for i, c in enumerate(partition[self._nb_bits_row:]))
        )

    def get_seat_id(self, seat: Seat) -> int:
        return seat.row * (1 << self._nb_bits_column) + seat.column


def get_highest_seat_id(plane: Plane, seats: List[Seat]) -> int:
    return plane.get_seat_id(max(seats, key=lambda seat: plane.get_seat_id(seat)))


NB_BITS_ROW = 7
NB_BITS_COLUMN = 3

plane = Plane(NB_BITS_ROW, NB_BITS_COLUMN)
entries: List[Seat] = []

entry = input()
while entry != '-':
    entries.append(plane.get_seat_from_partition(entry))
    entry = input()


print("solution:", get_highest_seat_id(plane, entries))