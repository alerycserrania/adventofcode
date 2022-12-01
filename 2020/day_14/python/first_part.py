from typing import Dict, Tuple
import re


MASK_SIZE = 36

TABLE = {
    ('0', '0'): '0',
    ('1', '0'): '0',
    ('0', '1'): '1',
    ('1', '1'): '1',
    ('0', 'X'): '0',
    ('1', 'X'): '1',
}

def parse_mem(mem: str) -> Tuple[int, int]:
    match = re.match(r'mem\[(?P<address>\d+)\]\s=\s(?P<value>\d+)', mem)
    if match:
        return int(match.group('address')), int(match.group('value'))
    else:
        raise Exception("parsing memory error")

def apply_mask(value: int, mask: str) -> int:
    v_str = str(bin(value)[2:]).zfill(MASK_SIZE)
    return int(''.join(TABLE[(v_str[i], mask[i])] for i in range(MASK_SIZE)), 2)



mask = 'X' * MASK_SIZE
memory: Dict[int, int] = dict()
entry = input()
while entry != '-':
    if entry.startswith('mask'):
        mask = entry[7:]
    elif entry.startswith('mem'):
        address, value = parse_mem(entry)
        memory[address] = apply_mask(value, mask)
    else:
        raise Exception(f"unexpected input: {entry}")
    entry = input()

print(sum(memory.values()))
