from collections import deque
from itertools import combinations
from typing import Deque, Iterable, Optional, List
from typing import overload


PREAMBLE_LENGTH = 25


class XmasCode():
    _invalid_number: Optional[int]

    def __init__(self, data: Iterable[int]):
        self._deque = deque(data)
        self._code = list(data)
        self._invalid_number = None

    @property
    def invalid_number(self) -> int:
        if self._invalid_number is None:
            raise Exception("no invalid number")
        else:
            return self._invalid_number

    def add(self, number: int) -> int:
        if self._invalid_number is None and not self._is_number_valid(number):
            self._invalid_number = number
        self._deque.append(number)
        self._code.append(number)
        return self._deque.popleft()

    def find_contiguous_set_slice(self) -> slice:
        if self._invalid_number is None:
            raise Exception("no invalid number")

        for first_index, last_index in combinations(range(len(self._code)), 2):
            contiguous_set = self._code[first_index:last_index+1]
            if sum(contiguous_set) == self._invalid_number:
                return slice(first_index, last_index)

        raise Exception("no contiguous set found")

    def _is_number_valid(self, number: int) -> bool:
        for combination in combinations(self._deque, 2):
            if sum(combination) == new_number:
                return True
        return False

    def __getitem__(self, i):
        return self._code[i]


code = XmasCode(
    (int(input()) for i in range(PREAMBLE_LENGTH))
)

new_number = int(input())
while new_number != -1:
    code.add(new_number)
    new_number = int(input())

contiguous_set_slice = code.find_contiguous_set_slice()
contiguous_set = code[contiguous_set_slice]
print("solution:", min(contiguous_set) + max(contiguous_set))