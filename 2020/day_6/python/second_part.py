import operator
import string
from typing import List


class Passenger:

    def __init__(self, answers: List[bool]):
        self._answers = answers

    @property
    def answers(self) -> List[bool]:
        return self._answers

    def __getitem__(self, key: str) -> bool:
        if not (ord('a') <= ord(key) <= ord('z')):
            raise KeyError(key)
        return self._answers[ord(key) - ord('a')]
    
    def __repr__(self) -> str:
        return f"(Person answers={{{', '.join(char + ': ' + str(self._answers[index]) for index, char in enumerate(string.ascii_lowercase))}}})"


class Group:
    
    def __init__(self, passengers: List[Passenger]):
        self._passengers = passengers

    def get_answers(self) -> List[bool]:
        answers = map(lambda _: True, range(26))
        for passenger in self._passengers:
            answers = map(operator.and_, answers, passenger.answers)
        return list(answers)

    def __repr__(self) -> str:
        answers = self.get_answers()
        return f"(Group answers={{{', '.join(char + ': ' + str(answers[index]) for index, char in enumerate(string.ascii_lowercase))}}})"


def parse_passenger(entry: str) -> Passenger:
    answers = [False] * 26
    for answer in entry:
        answers[ord(answer) - ord('a')] = True
    return Passenger(answers)


def count_nb_yes(groups: List[Group]) -> int:
    return sum(sum(1 for answer in group.get_answers() if answer) for group in groups)


groups: List[Group] = []
entry = input()
while entry != '-':
    passengers: List[Passenger] = []
    while entry != '':
        passengers.append(parse_passenger(entry))
        entry = input()
    groups.append(Group(passengers))
    entry = input() 

print("solution:", count_nb_yes(groups))