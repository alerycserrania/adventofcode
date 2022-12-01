from typing import List, Tuple, Set, Dict


def play_game(first_player: List[int], second_player: List[int], game: int) -> int:
    rounds: Set[Tuple[Tuple[int,...], Tuple[int,...]]] = set()
    rnd = 1

    # print(f"=== Game {game} ===")
    # print()

    while len(first_player) > 0 and len(second_player) > 0:
        t_first_player, t_second_player = tuple(first_player), tuple(second_player)
        if (t_first_player, t_second_player) in rounds:
            # prevents infinite games
            return 1
            
        rounds.add((t_first_player, t_second_player))

        card_first = first_player.pop(0)
        card_second = second_player.pop(0)
        

        if len(first_player) >= card_first and len(second_player) >= card_second:
            # playing subgame
            winner = play_game(first_player[:card_first],
                               second_player[:card_second], game+1)
            if winner == 1:
                first_player.extend([card_first, card_second])
            else:
                second_player.extend([card_second, card_first])
        else:
            # playing normally
            if card_first > card_second:
                first_player.extend([card_first, card_second])
            else:
                second_player.extend([card_second, card_first])

        
        rnd += 1

    return 2 if len(first_player) == 0 else 1


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

winner = play_game(first_player, second_player, 1)

if winner == 1:
    print("solution:", sum(
        [(i+1)*c for i, c in enumerate(first_player[::-1])]))
else:
    print("solution:", sum(
        [(i+1)*c for i, c in enumerate(second_player[::-1])]))
