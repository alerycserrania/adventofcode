from typing import List, NamedTuple, Generator, Callable, Iterator
from enum import Enum


class Seat(str, Enum):
    FLOOR = '.'
    EMPTY = 'L'
    OCCUPIED = '#'

    def __repr__(self) -> str:
        return self.value


class Location:

    def __init__(self, row: int, col: int):
        self._col = col
        self._row = row

    @property
    def col(self):
        return self._col

    @property
    def row(self):
        return self._row

    def __add__(self, location: 'Location') -> 'Location':
        return Location(row=self._row+location._row, col=self._col+location._col)


class Ferry:

    ADJACENT_LOCATIONS = (
        Location(col=-1, row=0),
        Location(col=1, row=0),
        Location(col=0, row=-1),
        Location(col=0, row=1),
        Location(col=-1, row=1),
        Location(col=1, row=-1),
        Location(col=-1, row=-1),
        Location(col=1, row=1),
    )

    def __init__(self, layout: List[List[Seat]]):
        self._layout = [
            [Seat(el) for el in row] 
            for row in layout
        ]
        self._is_stabilized = False

    def nb_rows(self) -> int:
        return len(self._layout)

    def nb_columns(self, row: int) -> int:
        return len(self._layout[row])
    
    def get_nb_seat_type(self, seat_type: Seat) -> int:
        return sum(sum(1 for seat in row if seat == seat_type) for row in self._layout)

    def _get_seat(self, location: Location) -> Seat:
        return self._layout[location.row][location.col]

    def _get_adjacent_seats(self, location: Location) -> List[Seat]:
        return [self._get_seat(location + l) for l in self._get_adjacent_locations(location)]

    def _get_adjacent_locations(self, location: Location) -> List[Location]:
        return [l for l in self.ADJACENT_LOCATIONS if self._is_location_within_limits(location + l)]

    def _is_location_within_limits(self, location: Location) -> bool:
        if location.row < 0 or location.row >= self.nb_rows():
            return False
        if location.col < 0 or location.col >= self.nb_columns(location.row):
            return False
        return True

    def __iter__(self):
        while not self._is_stabilized:
            self._is_stabilized = True
            yield self._layout
            self._layout = [
                [self._next_seat_status(Location(row=row, col=col)) for col in range(self.nb_columns(row))]
                for row in range(self.nb_rows())
            ]

    def _next_seat_status(self, location: Location) -> Seat:
        seat = self._get_seat(location)
        if seat == Seat.FLOOR:
            return seat
        adj_seats = self._get_adjacent_seats(location)
        nb_occupied_seats = adj_seats.count(Seat.OCCUPIED)
        if seat == Seat.EMPTY and nb_occupied_seats == 0:
            self._is_stabilized = False
            return Seat.OCCUPIED
        elif seat == Seat.OCCUPIED and nb_occupied_seats >= 4:
            self._is_stabilized = False
            return Seat.EMPTY
        else:
            return seat

def parse_row(row: str) -> List[Seat]:
    return [Seat(el) for el in row]


layout: List[List[Seat]] = list()
row = input()
while row != '-':
    layout.append(parse_row(row))
    row = input()

ferry = Ferry(layout)
for layout in ferry: 
    pass
print(ferry.get_nb_seat_type(Seat.OCCUPIED))