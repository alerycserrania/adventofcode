MAX_VALUE = 20201227
SUBJECT_NUMBER = 7


def transform(subject_number: int, loop_size: int) -> int:
    value = 1
    for _ in range(loop_size):
        value *= subject_number
        value %= MAX_VALUE
    return value


pk_card = int(input())
pk_door = int(input())


card_subject_number = 1
door_subject_number = 1
loop_size = 0
while card_subject_number != pk_card and door_subject_number != pk_card:
    card_subject_number *= SUBJECT_NUMBER
    door_subject_number *= SUBJECT_NUMBER
    card_subject_number %= MAX_VALUE
    door_subject_number %= MAX_VALUE
    loop_size += 1

if card_subject_number == pk_card:
    print("solution:", transform(pk_door, loop_size))
else:
    print("solution:", transform(pk_card, loop_size))
