import itertools
import math

SUM_TO_FIND = 2020

entries = []

entry = int(input())
while entry != -1:
    entries.append(entry)
    entry = int(input())

for comb in itertools.combinations(entries, 2):
    if sum(comb) == SUM_TO_FIND:
        print("solution:", math.prod(comb))
        break
        


