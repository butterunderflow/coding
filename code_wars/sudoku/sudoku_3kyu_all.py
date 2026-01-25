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


def solve_from_cell(puzzle: list[list[int]], x: int, y: int) -> list[list[list[int]]]:
    if y >= 9:
        return solve_from_row(puzzle, x + 1)
    if puzzle[x][y] != 0:
        return solve_from_cell(puzzle, x, y + 1)
    result = []
    for n in range(1, 10):
        if put(puzzle, n, x, y):
            result += solve_from_cell(puzzle, x, y + 1)
            puzzle[x][y] = 0  # this unset is unnecessary
    return result


def solve_from_row(puzzle: list[list[int]], x: int) -> list[list[list[int]]]:
    if x >= 9:
        return [[puzzle[i].copy() for i in range(9)]]

    return solve_from_cell(puzzle, x, 0)


def sudoku_solver(puzzle: list[list[int]]) -> list[list[int]]:
    answers = solve_from_row(puzzle, 0)
    assert answers is not None
    for ans in answers:
        print(ans)
    assert len(answers) == 1
    return answers[0]


puzzle = [
    [2, 0, 0, 0, 0, 0, 0, 0, 0],
    [7, 0, 9, 0, 0, 6, 5, 0, 0],
    [0, 8, 0, 7, 2, 0, 0, 0, 0],
    [6, 0, 0, 0, 0, 2, 0, 5, 9],
    [0, 0, 0, 0, 0, 0, 8, 0, 1],
    [8, 0, 0, 0, 0, 3, 2, 6, 0],
    [0, 2, 3, 0, 9, 0, 0, 0, 0],
    [4, 0, 0, 8, 0, 7, 0, 0, 5],
    [0, 0, 0, 0, 0, 4, 0, 0, 0],
]


print(sudoku_solver(puzzle))
