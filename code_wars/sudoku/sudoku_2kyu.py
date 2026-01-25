import sys
import time
from typing import Optional


class Solver:
    def __init__(self):
        self.row_pockets = [[False for _ in range(9)] for _ in range(9)]
        self.col_pockets = [[False for _ in range(9)] for _ in range(9)]
        self.block_pockets = [[False for _ in range(9)] for _ in range(9)]
        self.cell_difficulty: list[list[list[int]]] = [
            [[] for _ in range(9)] for _ in range(9)
        ]

    def is_valid_put(self, n: int, x: int, y: int) -> bool:
        if self.row_pockets[x][n - 1]:
            return False
        if self.col_pockets[y][n - 1]:
            return False
        block_index = get_block_index(x, y)
        if self.block_pockets[block_index][n - 1]:
            return False
        return True

    def put(self, puzzle: list[list[int]], n: int, x: int, y: int) -> bool:
        if not self.is_valid_put(n, x, y):
            return False
        puzzle[x][y] = n
        self.row_pockets[x][n - 1] = True
        self.col_pockets[y][n - 1] = True
        block_index = get_block_index(x, y)
        self.block_pockets[block_index][n - 1] = True
        self.update_affected_cell_difficulty(puzzle, x, y)
        return True

    def unset(self, puzzle: list[list[int]], x: int, y: int) -> None:
        n = puzzle[x][y]
        puzzle[x][y] = 0
        self.row_pockets[x][n - 1] = False
        self.col_pockets[y][n - 1] = False
        block_index = get_block_index(x, y)
        self.block_pockets[block_index][n - 1] = False
        self.update_affected_cell_difficulty(puzzle, x, y)

    def all_set(self, puzzle: list[list[int]]) -> bool:
        return all(puzzle[i][j] != 0 for i in range(0, 9) for j in range(0, 9))

    def solve_from_cell(
        self, puzzle: list[list[int]], x: int, y: int
    ) -> Optional[list[list[int]]]:
        results = None
        options = self.cell_difficulty[x][y]
        for n in options:
            if self.put(puzzle, n, x, y):
                tesm_res = self.solve(puzzle)
                self.unset(puzzle, x, y)
                if tesm_res:
                    if results and results != tesm_res:
                        assert False
                    results = tesm_res
        return results

    def update_cell_difficulty(self, puzzle: list[list[int]], x: int, y: int) -> None:
        options = [n for n in range(1, 10) if self.is_valid_put(n, x, y)]
        self.cell_difficulty[x][y] = options

    def update_affected_cell_difficulty(
        self, puzzle: list[list[int]], x: int, y: int
    ) -> None:
        for i in range(9):
            if puzzle[x][i] == 0:
                self.update_cell_difficulty(puzzle, x, i)
            if puzzle[i][y] == 0:
                self.update_cell_difficulty(puzzle, i, y)
        block_start_row = x // 3 * 3
        block_start_col = y // 3 * 3
        for i in range(3):
            for j in range(3):
                cell_x = block_start_row + i
                cell_y = block_start_col + j
                if puzzle[cell_x][cell_y] == 0:
                    self.update_cell_difficulty(puzzle, cell_x, cell_y)

    def init_cell_difficulty(self, puzzle: list[list[int]]) -> None:
        for i in range(9):
            for j in range(9):
                if puzzle[i][j] == 0:
                    self.update_cell_difficulty(puzzle, i, j)

    def find_the_easiest_cell(
        self, puzzle: list[list[int]]
    ) -> Optional[tuple[int, int]]:
        best_cell: Optional[tuple[int, int]] = None
        best_difficulty = 11
        for i in range(9):
            for j in range(9):
                if puzzle[i][j] == 0:
                    difficulty = len(self.cell_difficulty[i][j])
                    if difficulty < best_difficulty:
                        best_difficulty = difficulty
                        best_cell = (i, j)
        return best_cell

    def solve(self, puzzle: list[list[int]]) -> Optional[list[list[int]]]:
        best_cell = self.find_the_easiest_cell(puzzle)
        if best_cell is None:
            return [puzzle[i].copy() for i in range(9)]
        x, y = best_cell
        return self.solve_from_cell(puzzle, x, y)

    def init_pockets(self, puzzle: list[list[int]]) -> None:
        for i in range(9):
            for j in range(9):
                n = puzzle[i][j]
                if n != 0:
                    assert self.row_pockets[i][n - 1] == False
                    assert self.col_pockets[j][n - 1] == False
                    block_index = get_block_index(i, j)
                    assert self.block_pockets[block_index][n - 1] == False
                    self.row_pockets[i][n - 1] = True
                    self.col_pockets[j][n - 1] = True
                    self.block_pockets[block_index][n - 1] = True


def check_validity(puzzle: list[list[int]]):
    if len(puzzle) != 9 or any(len(row) != 9 for row in puzzle):
        assert False
    has_zero = False
    for i in range(9):
        for j in range(9):
            n = puzzle[i][j]
            if not (0 <= n <= 9):
                assert False
            if n == 0:
                has_zero = True
    assert has_zero


def get_block_index(x: int, y: int) -> int:
    return (x // 3) * 3 + (y // 3)


def sudoku_solver(puzzle: list[list[int]]) -> list[list[int]]:
    # print(puzzle, file=sys.stderr)
    check_validity(puzzle)
    solver = Solver()
    solver.init_cell_difficulty(puzzle)
    solver.init_pockets(puzzle)
    answers = solver.solve(puzzle)
    assert answers is not None
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
