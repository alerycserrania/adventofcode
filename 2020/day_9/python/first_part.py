from collections import deque
from itertools import combinations
from typing import Deque, Iterable, Optional


PREAMBLE_LENGTH = 25


class XmasCode():
    _invalid_number: Optional[int]

    def __init__(self, data: Iterable[int]):
        self._deque = deque(data)
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
        return self._deque.popleft()

    def _is_number_valid(self, number: int) -> bool:
        for combination in combinations(self._deque, 2):
            if sum(combination) == new_number:
                return True
        return False


code = XmasCode(
    (int(input()) for i in range(PREAMBLE_LENGTH))
)

new_number = int(input())
while new_number != -1:
    code.add(new_number)
    new_number = int(input())

print("solution:", code.invalid_number)