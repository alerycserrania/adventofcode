import inspect
from typing import Callable, Dict, Iterable, List, Set

import rules


Rule = Dict[str, Callable[[str], bool]]


REQUIRED_FIELDS = {'byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid'}
RULES = dict(inspect.getmembers(rules, lambda m: inspect.isfunction(m)))


class Passport(dict):
    
    @classmethod
    def parse_entry(cls, entry: List[str]) -> 'Passport':
        return cls({ 
            data.split(':')[0]: data.split(':')[1]
            for line in entry
            for data in line.split()
        })
            


class PassportVerifier:
    _required_fields: Set[str]
    _rules: Rule
    
    def __init__(self, 
                 required_fields: Iterable[str],
                 rules: Iterable[Rule]):
        self._required_fields = set(required_fields)
        self._rules = rules

    def is_valid(self, passport: Passport) -> bool:
        missing_fields = set(self._required_fields)
        for field, value in passport.items():
            if field in RULES and not RULES[field](value):
                return False
            missing_fields.discard(field)
        return len(missing_fields) == 0


def nb_passports_valid(passports: List[Passport], verifier: PassportVerifier) -> int:
    return sum(1 for passport in passports if verifier.is_valid(passport))


entries = []
line = input()
while line != '-':
    entry = []
    while line != '':
        entry.append(line)
        line = input()
    entries.append(Passport.parse_entry(entry))
    line = input()

passport_verifier = PassportVerifier(REQUIRED_FIELDS, RULES)

print("solution:", nb_passports_valid(entries, passport_verifier))
