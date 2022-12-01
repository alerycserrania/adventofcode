import re
from typing import List


PATTERN = re.compile(r'(?P<mini>\d+)-(?P<maxi>\d+)\s(?P<letter>\w):\s(?P<password>\w+)')

class Policy:
    def __init__(self, letter: str, mini: int, maxi: int):
        self.letter = letter
        self.mini = mini
        self.maxi = maxi

    def verify(self, password: str) -> bool:
        return self.mini <= password.count(self.letter) <= self.maxi

    def __repr__(self) -> str:
        return f"(Policy: letter='{self.letter}', mini={self.mini}, maxi={self.maxi})"

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