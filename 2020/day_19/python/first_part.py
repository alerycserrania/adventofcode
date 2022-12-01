from typing import Union, List, Dict, Tuple


CharRule = str
class FollowRule(List[int]): pass
Rule =  List[Union[CharRule, FollowRule]]


def parse_rule(entry: str) -> Tuple[int, Rule]:
    idx, rule = entry.split(': ')
    subrules = rule.split(' | ')
    
    parsed_rule: Rule = list()
    for subrule in subrules:
        if '"' in subrule:
            parsed_rule.append(subrule[1])
        else:
            parsed_rule.append(FollowRule([int(i) for i in subrule.split(' ')]))

    return int(idx), parsed_rule


def message_match(message: str, index: int, rule_id: int, rules: Dict[int, Rule], must_end = False) -> List[int]:
    possibilities: List[int] = []
    if len(message) == index:
        return possibilities

    for subrule in rules[rule_id]:
        if isinstance(subrule, CharRule) and message[index] == subrule:
            new_index = index + 1
            if not must_end or new_index == len(message):
                possibilities.append(new_index)
        elif isinstance(subrule, FollowRule):
            sb: List[int] = subrule
            pss: List[int] = message_match(message, index, sb[0], rules)
            sb = sb[1:]
            # pss = toutes les positions viables
            # donc pour chaque position viable, 
            # il faut regarder quelles sont les positions viables avec la règle suivante
            while len(sb) > 0:
                pss = [p for ps in pss for p in message_match(message, ps, sb[0], rules)]
                sb = sb[1:]
         
            possibilities.extend([ps for ps in pss if not must_end or len(message) == ps])
                
    return possibilities

rules: Dict[int, Rule] = dict()
messages: List[str] = list()

entry = input()
while entry != '':
    idx, rule = parse_rule(entry)
    rules[idx] = rule
    entry = input()

entry = input()
while entry != '-':
    messages.append(entry)
    entry = input()

good_messages = [
    m for m in messages 
    if message_match(m, 0, 0, rules, must_end=True)
]

print(len(good_messages))