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
    update_affected_cell_difficulty(puzzle, x, y)
    return True


def unset(puzzle: list[list[int]], x: int, y: int) -> None:
    puzzle[x][y] = 0
    update_affected_cell_difficulty(puzzle, x, y)


def all_set(puzzle: list[list[int]]) -> bool:
    return all(puzzle[i][j] != 0 for i in range(0, 9) for j in range(0, 9))


def solve_from_cell(
    puzzle: list[list[int]], x: int, y: int
) -> Optional[list[list[int]]]:
    for n in range(1, 10):
        if put(puzzle, n, x, y):
            result = solve(puzzle)
            unset(puzzle, x, y)
            if result:
                return result
    return None


cell_difficulty = [[10] * 9 for _ in range(9)]


def update_cell_difficulty(puzzle: list[list[int]], x: int, y: int) -> None:
    options = sum(1 for n in range(1, 10) if is_valid_put(puzzle, n, x, y))
    cell_difficulty[x][y] = options


def update_affected_cell_difficulty(puzzle: list[list[int]], x: int, y: int) -> None:
    for i in range(9):
        if puzzle[x][i] == 0:
            update_cell_difficulty(puzzle, x, i)
        if puzzle[i][y] == 0:
            update_cell_difficulty(puzzle, i, y)
    block_start_row = x // 3 * 3
    block_start_col = y // 3 * 3
    for i in range(3):
        for j in range(3):
            cell_x = block_start_row + i
            cell_y = block_start_col + j
            if puzzle[cell_x][cell_y] == 0:
                update_cell_difficulty(puzzle, cell_x, cell_y)


def init_cell_difficulty(puzzle: list[list[int]]) -> None:
    for i in range(9):
        for j in range(9):
            if puzzle[i][j] == 0:
                update_cell_difficulty(puzzle, i, j)


def find_the_easiest_cell(puzzle: list[list[int]]) -> Optional[tuple[int, int]]:
    best_cell: Optional[tuple[int, int]] = None
    best_difficulty = 11
    for i in range(9):
        for j in range(9):
            if puzzle[i][j] == 0:
                difficulty = cell_difficulty[i][j]
                if difficulty < best_difficulty:
                    best_difficulty = difficulty
                    best_cell = (i, j)
    return best_cell


def solve(puzzle: list[list[int]]) -> Optional[list[list[int]]]:
    best_cell = find_the_easiest_cell(puzzle)
    if best_cell is None:
        return [puzzle[i].copy() for i in range(9)]
    x, y = best_cell
    return solve_from_cell(puzzle, x, y)


def check_validity(puzzle: list[list[int]]):
    if len(puzzle) != 9 or any(len(row) != 9 for row in puzzle):
        assert False
    for i in range(9):
        for j in range(9):
            n = puzzle[i][j]
            if not (0 <= n <= 9):
                assert False


def sudoku_solver(puzzle: list[list[int]]) -> list[list[int]]:
    check_validity(puzzle)
    init_cell_difficulty(puzzle)
    answers = solve(puzzle)
    assert answers is not None
    return answers


grid = [
    [8, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 3, 6, 0, 0, 0, 0, 0],
    [0, 7, 3, 0, 9, 0, 2, 0, 0],
    [0, 5, 3, 0, 0, 7, 0, 0, 0],
    [0, 0, 0, 0, 4, 5, 7, 0, 0],
    [0, 0, 0, 1, 0, 0, 0, 3, 0],
    [0, 0, 1, 0, 0, 0, 0, 6, 8],
    [0, 0, 8, 5, 0, 0, 0, 1, 8],
    [0, 9, 0, 0, 0, 0, 4, 0, 8],
]

start_time = time.time()
result = sudoku_solver(grid)
end_time = time.time()

print(end_time - start_time)
