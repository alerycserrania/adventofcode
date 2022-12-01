from typing import List


MAX_DIFF = 3

adapters: List[int] = [0]
adapter = int(input())
while adapter != -1:
    adapters.append(adapter)
    adapter = int(input())
adapters.append(max(adapters) + MAX_DIFF)
adapters.sort()

nb_arrangements = [0] * len(adapters)
nb_arrangements[-1] = 1

for i in range(len(adapters)-1, -1, -1):
    j = 1
    while i-j >= 0 and adapters[i-j] + MAX_DIFF >= adapters[i]:
        nb_arrangements[i-j] += nb_arrangements[i]
        j += 1

print("solution:", nb_arrangements[0])

