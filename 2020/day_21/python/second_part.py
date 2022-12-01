from typing import List, Dict, Tuple, Set
import re


def respect_rule(
    ingredients_allergens: Dict[str, Set[str]],
    rule: Tuple[Set[str], Set[str]]
) -> bool:
    allergens: Set[str] = set()
    for ingredient in rule[0]:
        allergens.update(ingredients_allergens[ingredient])
    return rule[1].issubset(allergens)


def may_contains_allergen(
    ingredient: str,
    allergen: str,
    ingredients_allergens: Dict[str, Set[str]],
    rules: List[Tuple[Set[str], Set[str]]]
) -> bool:
    ingredients_allergens = {
        key: set(value).difference({allergen})
        for key, value in ingredients_allergens.items()
    }
    ingredients_allergens[ingredient].add(allergen)
    for rule in rules:
        if not respect_rule(ingredients_allergens, rule):
            return False
    return True


def parse_food(entry: str) -> Tuple[Set[str], Set[str]]:
    ingredients, allergens = entry.split('(')
    return (
        set(ingredients[:-1].split(' ')),
        set(allergens[9:-1].split(', '))
    )


list_ingredients: List[str] = list()
ingredients_allergens: Dict[str, Set[str]] = dict()
rules: List[Tuple[Set[str], Set[str]]] = list()
entry = input()
while entry != '-':
    ingredients, allergens = parse_food(entry)
    rules.append((ingredients, allergens))
    for ingredient in ingredients:
        if ingredient not in ingredients_allergens:
            ingredients_allergens[ingredient] = set(allergens)
        else:
            ingredients_allergens[ingredient].update(allergens)
    list_ingredients.extend(ingredients)
    entry = input()

state_changed = True
while state_changed:
    state_changed = False
    allergens_to_discard = set()
    for ingredient, possible_allergens in ingredients_allergens.items():
        for possible_allergen in possible_allergens:
            if not may_contains_allergen(ingredient, possible_allergen, ingredients_allergens, rules):
                allergens_to_discard.add((ingredient, possible_allergen))
                state_changed = True

    for ingredient, allergen_to_discard in allergens_to_discard:
        ingredients_allergens[ingredient].discard(allergen_to_discard)

for ingredient in list_ingredients:
    if len(ingredients_allergens[ingredient]) == 1:
        allergen = next(iter(ingredients_allergens[ingredient]))
        for i in list_ingredients:
            if i != ingredient:
                ingredients_allergens[i].discard(allergen)

ingredients_with_allergens = [
    (ingredient, next(iter(allergens)))
    for ingredient, allergens in ingredients_allergens.items()
    if len(allergens) > 0
]

ingredients_with_allergens.sort(key=lambda t: t[1])

print("solution:", ','.join(
    i for i, a in ingredients_with_allergens
))
