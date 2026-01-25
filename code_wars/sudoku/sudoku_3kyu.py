import time
from typing import Optional


def is_valid_put(puzzle: list[list[int]], n: int, x: int, y: int) -> bool:
    if any(n == a for a in puzzle[x]):
        return False
    if any(n == puzzle[i][y] for i in range(0, 9)):
        return False
    block_start_row = x // 3 * 3
    block_start_col = y // 3 * 3
    for i in range(0, 3):
        for j in range(0, 3):
            if puzzle[block_start_row + i][block_start_col + j] == n:
                return False
    return True


def put(puzzle: list[list[int]], n: int, x: int, y: int) -> bool:
    if not is_valid_put(puzzle, n, x, y):
        return False

    puzzle[x][y] = n
    return True


def all_set(puzzle: list[list[int]]) -> bool:
    return all(puzzle[i][j] != 0 for i in range(0, 9) for j in range(0, 9))


def solve_from_cell(
    puzzle: list[list[int]], x: int, y: int
) -> Optional[list[list[int]]]:
    if y >= 9:
        return solve_from_row(puzzle, x + 1)
    if puzzle[x][y] != 0:
        return solve_from_cell(puzzle, x, y + 1)
    for n in range(1, 10):
        if put(puzzle, n, x, y):
            result = solve_from_cell(puzzle, x, y + 1)
            puzzle[x][y] = 0  # this unset is unnecessary
            if result:
                return result
    return None


def solve_from_row(puzzle: list[list[int]], x: int) -> Optional[list[list[int]]]:
    if x >= 9:
        return [puzzle[i].copy() for i in range(9)]

    return solve_from_cell(puzzle, x, 0)


def sudoku_solver(puzzle: list[list[int]]) -> list[list[int]]:
    answers = solve_from_row(puzzle, 0)
    assert answers is not None
    for ans in answers:
        print(ans)
    return answers



grid = [
    [8, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 3, 6, 0, 0, 0, 0, 0],
    [0, 7, 0, 0, 9, 0, 2, 0, 0],
    [0, 5, 0, 0, 0, 7, 0, 0, 0],
    [0, 0, 0, 0, 4, 5, 7, 0, 0],
    [0, 0, 0, 1, 0, 0, 0, 3, 0],
    [0, 0, 1, 0, 0, 0, 0, 6, 8],
    [0, 0, 8, 5, 0, 0, 0, 1, 0],
    [0, 9, 0, 0, 0, 0, 4, 0, 0],
]

start_time = time.time()
result = sudoku_solver(grid)
end_time = time.time()

print(end_time - start_time)
