from typing import Iterable, List, Set


REQUIRED_FIELDS = {'byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid'}


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

    def __init__(self, required_fields: Iterable[str]):
        self._required_fields = set(required_fields)

    def is_valid(self, passport: Passport) -> bool:
        missing_fields = set(self._required_fields)
        for field in passport.keys():
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

passport_verifier = PassportVerifier(REQUIRED_FIELDS)

print("solution:", nb_passports_valid(entries, passport_verifier))
