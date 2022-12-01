import re
from functools import reduce
from typing import Dict, List, Tuple, NamedTuple


class BagContents(Dict[str, int]):
    pass


class Bag(NamedTuple):
    name: str
    contents: BagContents


class Bags(Dict[str, Bag]):
    pass


CONTENANT_PATTERN = re.compile(r'^(?P<contenant>.+)\sbags\scontain\s(?P<contents>.+)\.$')
CONTENTS_PATTERN = re.compile(r'^(?P<count>\d+)\s(?P<name>.+)\sbag(s?)')


def parse_entry(entry: str) -> Bag:
    contenant_match = CONTENANT_PATTERN.match(entry)
    if contenant_match is None:
        raise ValueError()

    contenant = contenant_match.group('contenant')
    contents = contenant_match.group('contents')
    if contents == 'no other bags':
        return Bag(name=contenant, contents=BagContents())

    bag_contents = BagContents()
    for contents_entry in contents.split(', '):
        contents_match = CONTENTS_PATTERN.match(contents_entry)
        if contents_match is None:
            raise ValueError()
        
        bag_name = contents_match.group('name')
        bag_count = int(contents_match.group('count'))
        bag_contents[bag_name] = bag_count 

    return Bag(name=contenant, contents=bag_contents)

    
def count_bags_inside(bag_name: str, bags: Bags) -> int:
    bag_contents = bags[bag_name].contents
    return sum(count * (1 + count_bags_inside(bag_inside, bags)) for bag_inside, count in bags[bag_name].contents.items()) 


BAG_TO_COUNT = 'shiny gold'

bags: Bags = Bags()
entry = input()
while entry != '-':
    bag = parse_entry(entry)
    bags[bag.name] = bag
    entry = input()

print("solution:", count_bags_inside(BAG_TO_COUNT, bags))