from typing import List

PRIORITIES = {
    '+': 2,
    '*': 1
}

OPERATORS = {
    '+': (lambda a, b: a + b),
    '*': (lambda a, b: a * b),
}

NUMBERS = set([str(n) for n in range(10)])

def evaluatePostfix(postfix: List[str]) -> int:
    stack: List[int] = []
    for c in postfix:
        if c in NUMBERS:
            stack.append(int(c))
        elif c in OPERATORS:
            stack.append(OPERATORS[c](stack.pop(), stack.pop()))
        else:
            raise Exception("Unrecognize token: " + c)
    
    if len(stack) != 1:
        raise Exception("Stack size not equals to 1: " + str(stack))

    return stack[0]

        

def infixToPostfix(infix: List[str]) -> List[str]:
    """Simplified Shunting-yard algorithm
    
    Source: https://en.wikipedia.org/wiki/Shunting-yard_algorithm
    """
    postfix: List[str] = []
    operators: List[str] = []
    for c in infix:
        if c in NUMBERS:
            postfix.append(c)
        elif c in OPERATORS:
            while (
                (len(operators) > 0 and operators[-1] in OPERATORS) 
                and (PRIORITIES[operators[-1]] >= PRIORITIES[c])
                and (operators[-1] != '(')
            ):
                postfix.append(operators.pop())
            operators.append(c)
        elif c == '(':
            operators.append(c)
        elif c == ')':
            while operators[-1] != '(':
                postfix.append(operators.pop())
            operators.pop()
        else:
            raise Exception("Unrecognize token: " + c)
    
    while len(operators) > 0:
        postfix.append(operators.pop())

    return postfix



expressions: List[str] = []

entry = input()
while entry != '-':
    expressions.append(entry.replace(' ', ''))
    entry = input()

print("solutions:", sum([evaluatePostfix(infixToPostfix(list(expression))) for expression in expressions]))