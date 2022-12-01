from typing import Dict

NB_TURN = 30000000
DEFAULT_NUMBER = 0


game: Dict[int, int] = dict()
next_number: int = 0
current_turn: int = 1

numbers = [int(n) for n in input().split(',')]
for number in numbers[:-1]:
    game[number] = current_turn
    current_turn += 1
    
    
next_number = numbers[-1]
while current_turn < NB_TURN:
    if next_number in game:
        game[next_number], next_number = current_turn, current_turn - game[next_number]
    else:
        game[next_number], next_number = current_turn, DEFAULT_NUMBER
    current_turn += 1


print("solutions:", next_number)
