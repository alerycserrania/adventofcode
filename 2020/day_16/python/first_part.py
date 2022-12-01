import re
from typing import Dict, List, Tuple

Ticket = List[int]
Rule = List[Tuple[int, int]]
Rules = Dict[str, Rule]


RULE_PATTERN = re.compile(r'(?P<field>.+):\s(?P<f_min>\d+)\-(?P<f_max>\d+)\sor\s(?P<s_min>\d+)\-(?P<s_max>\d+)')


def ticket_value_is_valid(value: int, rule: Rule) -> bool:
    return any(mini <= value and value <= maxi for mini, maxi in rule)


def ticket_error_rate(ticket: Ticket, rules: Rules) -> int:
    return sum(
        v for v in ticket 
        if not any(ticket_value_is_valid(v, rule) for rule in rules.values())
    )


rules: Rules = dict()
my_ticket: Ticket = list()
nearby_ticket: List[Ticket] = list()

# parse rules
entry = input()
while entry != '':
    match = re.match(RULE_PATTERN, entry)
    if match:
        rules[match.group('field')] = [
            (int(match.group('f_min')), int(match.group('f_max'))),
            (int(match.group('s_min')), int(match.group('s_max'))),
        ]
    else:
        raise Exception("parsing rules exceptions: " + entry)
    entry = input()

# parse my ticket
input()  # 'your ticket:'
entry = input()
my_ticket = [int(v) for v in entry.split(',')]
input()

# parse nearby tickets
input()  # 'nearby tickets'
entry = input()
while entry != '-':
    nearby_ticket.append([int(v) for v in entry.split(',')])
    entry = input()

# calc scanning error rate
error_rate = sum(
    ticket_error_rate(ticket, rules) for ticket in nearby_ticket
)

print("solution:", error_rate)