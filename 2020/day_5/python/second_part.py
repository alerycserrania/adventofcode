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

    def get_nb_seats(self) -> int:
        return 1 << (self._nb_bits_column + self._nb_bits_row)


# def find_my_seat_id(plane: Plane, seats: List[Seat]) -> int:
#     """
#     First solution: create an array of used seats by id
#       - id x -> index x in used_seats array
#     Advantage: linear time complexity
#     Disadvantage: exponential use of memory 
#       - size of used_seat depends on the number of bits
#       - for this problem, the number of seats is low therefore we can afford to use
#       this solution
#     """
#     used_seats = [False] * plane.get_nb_seats()
#     for seat in seats:
#         used_seats[plane.get_seat_id(seat)] = True

#     for i in range(1, len(used_seats)-1):
#         if used_seats[i-1] and not used_seats[i] and used_seats[i+1]:
#             return i

#     raise ValueError()

# def find_my_seat_id(plane: Plane, seats: List[Seat]) -> int:
#     """
#     Second solution: order seats by id
#     Advantage: memory complexity is linear
#     Disadvantage: Sorting = quasi-linear time complexity at best
#     """
#     seats = sorted(seats, key=lambda seat: plane.get_seat_id(seat))

#     for i in range(0, len(seats)):
#         first_seat_id = plane.get_seat_id(seats[i])
#         second_seat_id = plane.get_seat_id(seats[i+1])
#         if abs(first_seat_id - second_seat_id) == 2:
#             return min(first_seat_id, second_seat_id) + 1

#     raise ValueError()

def find_my_seat_id(plane: Plane, seats: List[Seat]) -> int:
    """
    Third solution: use the fact that the flight is full
    Only my seat is missing -> only need to find the missing id
    between the min and max ids.
    Time complexity is linear (search for min and max) 
    """ 
    seat_ids = [plane.get_seat_id(seat) for seat in seats]
    return set(range(min(seat_ids), max(seat_ids)+1)).difference(seat_ids).pop()

NB_BITS_ROW = 7
NB_BITS_COLUMN = 3

plane = Plane(NB_BITS_ROW, NB_BITS_COLUMN)
entries: List[Seat] = []

entry = input()
while entry != '-':
    entries.append(plane.get_seat_from_partition(entry))
    entry = input()

print("solution:", find_my_seat_id(plane, entries))