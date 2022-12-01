from typing import List

first_player: List[int] = []
second_player: List[int] = []

entry = input()  # Player 1:
entry = input()
while entry != '':
    first_player.append(int(entry))
    entry = input()

entry = input()  # Player 2:
entry = input()
while entry != '':
    second_player.append(int(entry))
    entry = input()

while len(first_player) > 0 and len(second_player) > 0:
    card_first = first_player.pop(0)
    card_second = second_player.pop(0)
    if card_first > card_second:
        first_player.extend([card_first, card_second])
    else:
        second_player.extend([card_second, card_first])

if len(first_player) > 0:
    print("solution:", sum([(i+1)*c for i, c in enumerate(first_player[::-1])]))
else:
    print("solution:", sum([(i+1)*c for i, c in enumerate(second_player[::-1])]))