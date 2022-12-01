from typing import List, NamedTuple, Set


class ProgramState(NamedTuple):
    accumulator: int
    line: int
        

class Instruction:

    def __init__(self, operation: str, argument: int):
        self._operation = operation
        self._argument = argument

    @property
    def operation(self) -> str:
        return self._operation

    @property
    def argument(self) -> int:
        return self._argument

    def __repr__(self) -> str:
        return f"(Instruction: operation='{self._operation}', argument={self.argument})"

    def nop(self, state: ProgramState) -> ProgramState:
        return ProgramState(
            accumulator=state.accumulator, 
            line=state.line+1
        )

    def acc(self, state: ProgramState) -> ProgramState:
        return ProgramState(
            accumulator=state.accumulator + self.argument, 
            line=state.line+1
        )

    def jmp(self, state: ProgramState) -> ProgramState:
        return ProgramState(
            accumulator=state.accumulator, 
            line=state.line + self.argument
        )


class Instructions(List[Instruction]):
    pass


class Program:

    def __init__(self, instructions: Instructions):
        self._instructions = instructions

    @property
    def instructions(self) -> Instructions:
        return self._instructions


    def __iter__(self):
        state = ProgramState(accumulator=0, line=0)
        yield state

        while state.line < len(self._instructions):
            state = self._execute_instruction(self._instructions[state.line], state)
            yield state


    def _execute_instruction(self, instruction: Instruction, state: ProgramState) -> ProgramState:
        return getattr(instruction, instruction.operation)(state)
    

def parse_instruction(entry: str) -> Instruction:
    splitted_entry = entry.split()
    return Instruction(operation=splitted_entry[0], argument=int(splitted_entry[1]))


def get_accumulator_before_infinite_loop(program: Program):
    visited_lines: Set[int] = set()
    for state in program:
        if state.line in visited_lines:
            return state.accumulator
        else:
            visited_lines.add(state.line)
    raise ValueError('program end')


instructions = Instructions()
entry = input()
while entry != 'end':
    instructions.append(parse_instruction(entry))
    entry = input()

program = Program(instructions)

print("solution:", get_accumulator_before_infinite_loop(program))