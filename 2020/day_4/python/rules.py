import re


HCL_PATTERN = re.compile(r'^#[0-9a-f]{6}$')
ECL_PATTERN = re.compile(r'^(amb)|(blu)|(brn)|(gry)|(grn)|(hzl)|(oth)$')
PID_PATTERN = re.compile(r'^[0-9]{9}$')


def byr(s: str) -> bool:
    try:
        return 1920 <= int(s) <= 2002
    except ValueError:
        return False


def iyr(s: str) -> bool:
    try:
        return 2010 <= int(s) <= 2020
    except ValueError:
        return False


def eyr(s: str) -> bool:
    try:
        return 2020 <= int(s) <= 2030
    except ValueError:
        return False
        

def hgt(s: str) -> bool:
    try:
        if s.endswith('cm'):
            return 150 <= int(s[:-2]) <= 193
        elif s.endswith('in'):
            return 59 <= int(s[:-2]) <= 76
        else:
            return False
    except ValueError:
        return False


def hcl(s: str) -> bool:
    return HCL_PATTERN.match(s)


def ecl(s: str) -> bool:
    return ECL_PATTERN.match(s)


def pid(s: str) -> bool:
    return PID_PATTERN.match(s)