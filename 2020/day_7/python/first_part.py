import re
from functools import reduce
from typing import Dict, List, NamedTuple
import math


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


def can_hold_bag_to_search(bag_name: str, bag_to_search: str, bags: Bags) -> bool:
    bag_contents = bags[bag_name].contents
    if bag_to_search in bag_contents:
        return True
    else:
        return any(can_hold_bag_to_search(bag_inside, bag_to_search, bags) for bag_inside in bag_contents)

    
def count_bags_which_can_hold_bag_to_search(bag_to_search: str, bags: Bags) -> int:
    return sum(1 for bag_name in bags.keys() if can_hold_bag_to_search(bag_name, bag_to_search, bags))


BAG_TO_SEARCH = 'shiny gold'

bags: Bags = Bags()
entry = input()
while entry != '-':
    bag = parse_entry(entry)
    bags[bag.name] = bag
    entry = input()

print("solution:", count_bags_which_can_hold_bag_to_search(BAG_TO_SEARCH, bags))