import re
from typing import List


PATTERN = re.compile(r'(?P<mini>\d+)-(?P<maxi>\d+)\s(?P<letter>\w):\s(?P<password>\w+)')

class Policy:
    def __init__(self, letter: str, first_pos: int, second_pos: int):
        self.letter = letter
        self.first_pos = first_pos
        self.second_pos = second_pos

    def verify(self, password: str) -> bool:
        len_password = len(password)
        return (self.first_pos <= len_password and self.letter == password[self.first_pos-1]) ^ (self.second_pos <= len_password and self.letter == password[self.second_pos-1]) 

    def __repr__(self) -> str:
        return f"(Policy: letter='{self.letter}', first_pos={self.first_pos}, second_pos={self.second_pos})"

class Entry:
    def __init__(self, password: str, policy: Policy):
        self.password = password
        self.policy = policy

    def is_pwd_valid(self) -> bool:
        return self.policy.verify(self.password)

    def __repr__(self) -> str:
        return f"(Entry: password='{self.password}', policy={self.policy}, valid={self.is_pwd_valid()})"


    @classmethod
    def parse_entry(cls, entry: str) -> 'Entry':
        match = PATTERN.search(entry)
        if match:
            policy = Policy(match.group('letter'), int(match.group('mini')), int(match.group('maxi')))
            return Entry(match.group('password'), policy)
        else:
            raise Exception(entry)


entries: List[Entry] = []

entry = input()
while entry != '-':
    entries.append(Entry.parse_entry(entry))
    entry = input()

nb_valids = sum(1 for entry in entries if entry.is_pwd_valid())

print("solution:", nb_valids)