from typing import List


MAX_DIFF = 3

differences = [0] * MAX_DIFF
adapters = [0]
adapter = int(input())
while adapter != -1:
    adapters.append(adapter)
    adapter = int(input())

adapters.append(max(adapters) + MAX_DIFF)
adapters.sort()

for i in range(0, len(adapters)-1):
    differences[adapters[i+1] - adapters[i] - 1] += 1

print("solution:", differences[0] * differences[2]) 
