from typing import Dict, Tuple, List
import itertools
import re


MASK_SIZE = 36

TABLE = {
    ('0', '0'): '0',
    ('1', '0'): '1',
    ('0', '1'): '1',
    ('1', '1'): '1',
    ('0', 'X'): 'X',
    ('1', 'X'): 'X',
}

def parse_mem(mem: str) -> Tuple[int, int]:
    match = re.match(r'mem\[(?P<address>\d+)\]\s=\s(?P<value>\d+)', mem)
    if match:
        return int(match.group('address')), int(match.group('value'))
    else:
        raise Exception("parsing memory error")

def apply_mask(address: int, mask: str) -> List[str]:
    v_str = bin(address)[2:].zfill(MASK_SIZE)
    q_address = [TABLE[(v_str[i], mask[i])] for i in range(MASK_SIZE)]
    x_pos =  [i for i, c in enumerate(q_address) if c == 'X']
    addresses: List[str] = []
    for comb in itertools.product(['0', '1'], repeat=len(x_pos)):
        tmp_address = list(q_address)
        for i, bit in enumerate(comb):
            tmp_address[x_pos[i]] = bit
        addresses.append(''.join(tmp_address))
    
    return addresses


mask = 'X' * MASK_SIZE
memory: Dict[str, int] = dict()
entry = input()
while entry != '-':
    if entry.startswith('mask'):
        mask = entry[7:]
    elif entry.startswith('mem'):
        address, value = parse_mem(entry)
        memory.update({address: value for address in apply_mask(address, mask)})
    else:
        raise Exception(f"unexpected input: {entry}")
    entry = input()

print(sum(memory.values()))
