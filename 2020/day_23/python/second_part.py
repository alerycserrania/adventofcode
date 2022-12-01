from typing import Callable, Optional, List
from collections import deque

NB_MOVES = 10000000
NEXT = 3
NB_CUPS = 1000000

class CircularListElem(object):

    def __init__(self, data: int, n: Optional['CircularListElem'], b: Optional['CircularListElem']):
        self.data = data
        self.next = n if n is not None else self
        self.to_elem_below = b

    def __repr__(self):
        return f"(CLE data={self.data} next={self.next.data} below={self.to_elem_below.data})"

class CircularList(object):
    pointer: CircularListElem

    def __init__(self, data: List[int], rest_in_order: List[int]):
        cl = [CircularListElem(d, None, None) for d in data]
        self.pointer = cl[0]
        
        for i in range(0, len(cl)):
            if i != len(cl) - 1:
                cl[i].next = cl[i+1]
            t = [j for j, ce in enumerate(cl) if ce.data == cl[i].data - 1]
            if len(t) == 1:
                cl[i].to_elem_below = cl[t[0]]
            else:
                cl[i].to_elem_below = cl[[j for j, ce in enumerate(cl) if ce.data == max(data)][0]]

        if len(rest_in_order) > 0:
            cl[i].next = CircularListElem(rest_in_order[0], None, cl[[j for j, ce in enumerate(cl) if ce.data == max(data)][0]])
            p = cl[i].next
            for r in rest_in_order[1:]:
                p.next = CircularListElem(r, None, p)
                p = p.next
            p.next = cl[0]
            cl[[j for j, ce in enumerate(cl) if ce.data == min(data)][0]].to_elem_below = p
        else:
            cl[i].next = cl[0]


    def insert_below(self, data: List[CircularListElem], destination: int):
        pointer = self.pointer

        while pointer.data != destination:
            pointer = pointer.to_elem_below

        n = pointer.next
        i = 0
        while i < len(data) - 1:
            pointer.next = data[i]
            pointer = pointer.next
            i += 1
        
        data[i].next = n
        pointer.next = data[i]
        

    def pop_next(self) -> CircularListElem:
        popped = self.pointer.next
        self.pointer.next = self.pointer.next.next
        return popped

    def move_next(self):
        self.pointer = self.pointer.next

    def peek(self) -> CircularListElem:
        return self.pointer

    def __iter__(self):
        start = self.pointer
        yield start.data
        n = start.next
        while n != start:
            yield n.data
            n = n.next

    def __repr__(self):
        return f"[{', '.join(map(str, self))}]"
        
cups = [int(n) for n in input()]
min_cups = min(cups)
max_cups = max(cups)
cl = CircularList(cups, [max_cups + 1 + i for i in range(NB_CUPS-len(cups))])
max_cups = NB_CUPS

for i in range(NB_MOVES):
    
    # pickup next
    pickup = []
    for _ in range(NEXT):
        pickup.append(cl.pop_next())
    pickup_elem = [d.data for d in pickup]

    # find destination
    destination = cl.peek().data - 1
    if destination < min_cups:
        destination = max_cups
    while destination in pickup_elem:
        destination -= 1
        if destination < min_cups:
            destination = max_cups
    
    # insert pickup after destination
    cl.insert_below(pickup, destination)

    # move pointer to next destination
    cl.move_next()

iter_cl = iter(cl)
while next(iter_cl) != 1: continue
print("solution:", next(iter_cl) * next(iter_cl))
