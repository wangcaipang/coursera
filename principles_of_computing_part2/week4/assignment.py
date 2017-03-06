"""
Loyd's Fifteen puzzle - solver and visualizer
Note that solved configuration has the blank (zero) tile in upper left
Use the arrows key to swap this tile with its neighbors
"""

import poc_fifteen_gui

class Puzzle:
    """
    Class representation for the Fifteen puzzle
    """

    def __init__(self, puzzle_height, puzzle_width, initial_grid=None):
        """
        Initialize puzzle with default height and width
        Returns a Puzzle object
        """
        self._height = puzzle_height
        self._width = puzzle_width
        self._grid = [[col + puzzle_width * row
                       for col in range(self._width)]
                      for row in range(self._height)]

        if initial_grid != None:
            for row in range(puzzle_height):
                for col in range(puzzle_width):
                    self._grid[row][col] = initial_grid[row][col]

    def __str__(self):
        """
        Generate string representaion for puzzle
        Returns a string
        """
        ans = ""
        for row in range(self._height):
            ans += str(self._grid[row])
            ans += "\n"
        return ans

    #####################################
    # GUI methods

    def get_height(self):
        """
        Getter for puzzle height
        Returns an integer
        """
        return self._height

    def get_width(self):
        """
        Getter for puzzle width
        Returns an integer
        """
        return self._width

    def get_number(self, row, col):
        """
        Getter for the number at tile position pos
        Returns an integer
        """
        return self._grid[row][col]

    def set_number(self, row, col, value):
        """
        Setter for the number at tile position pos
        """
        self._grid[row][col] = value

    def clone(self):
        """
        Make a copy of the puzzle to update during solving
        Returns a Puzzle object
        """
        new_puzzle = Puzzle(self._height, self._width, self._grid)
        return new_puzzle

    ########################################################
    # Core puzzle methods

    def current_position(self, solved_row, solved_col):
        """
        Locate the current position of the tile that will be at
        position (solved_row, solved_col) when the puzzle is solved
        Returns a tuple of two integers
        """
        solved_value = (solved_col + self._width * solved_row)

        for row in range(self._height):
            for col in range(self._width):
                if self._grid[row][col] == solved_value:
                    return (row, col)
        assert False, "Value " + str(solved_value) + " not found"

    def update_puzzle(self, move_string):
        """
        Updates the puzzle state based on the provided move string
        """

        zero_row, zero_col = self.current_position(0, 0)
        for direction in move_string:
            if direction == "l":
                assert zero_col > 0, "move off grid: " + direction
                self._grid[zero_row][zero_col] = self._grid[zero_row][zero_col - 1]
                self._grid[zero_row][zero_col - 1] = 0
                zero_col -= 1
            elif direction == "r":
                assert zero_col < self._width - 1, "move off grid: " + direction
                self._grid[zero_row][zero_col] = self._grid[zero_row][zero_col + 1]
                self._grid[zero_row][zero_col + 1] = 0
                zero_col += 1
            elif direction == "u":
                assert zero_row > 0, "move off grid: " + direction
                self._grid[zero_row][zero_col] = self._grid[zero_row - 1][zero_col]
                self._grid[zero_row - 1][zero_col] = 0
                zero_row -= 1
            elif direction == "d":
                assert zero_row < self._height - 1, "move off grid: " + direction
                self._grid[zero_row][zero_col] = self._grid[zero_row + 1][zero_col]
                self._grid[zero_row + 1][zero_col] = 0
                zero_row += 1
            else:
                assert False, "invalid direction: " + direction


    ##################################################################
    # Phase one methods
    def lower_row(self, target_row):
        """
        lower_row
        """
        for row in range(target_row, self.get_height()):
            for col in range(self.get_width()):
                num_row,num_col = self.current_position(row, col)
                if num_row != row or num_col != col:
                    return False
        return True
    def righter_col(self, target_row, target_col):
        """
        righter_col
        """
        for col in range(target_col + 1, self.get_width()):
            num_row, num_col = self.current_position(target_row, col)
            if num_row != target_row or num_col != col:
                return False
        return True
    def lower_row_invariant(self, target_row, target_col):
        """
        Check whether the puzzle satisfies the specified invariant
        at the given position in the bottom rows of the puzzle (target_row > 1)
        Returns a boolean
        """
        # replace with your code
        zero_row, zero_col = self.current_position(0, 0)
        if zero_row != target_row or zero_col != target_col:
            return False
        return self.lower_row(target_row + 1) and self.righter_col(target_row, target_col)

    def position_zero(self, target_pos, horizon_first):
        """
        position zero
        """

        target_row = target_pos[0]
        target_col = target_pos[1]
        zero_row, zero_col = self.current_position(0, 0)
        cur_string = ''
        vertical_moves = ''
        hozizontal_moves = ''
        vertical_move = 'u' if zero_row > target_row else 'd'
        horizontal_move = 'l' if zero_col > target_col else 'r'
        for dummy_idx in range(abs(zero_row - target_row)):
            vertical_moves += vertical_move
        for dummy_idx in range(abs(zero_col - target_col)):
            hozizontal_moves += horizontal_move
        cur_string = vertical_moves + hozizontal_moves
        if horizon_first == True:
            cur_string = hozizontal_moves + vertical_moves
        self.update_puzzle(cur_string)
        return cur_string

    def position_tile(self, cur_pos, target_pos):
        """
        position tile
        """
        cur_row = cur_pos[0]
        cur_col = cur_pos[1]
        target_row = target_pos[0]
        target_col = target_pos[1]
        if cur_row == target_row and cur_col == target_col:
            return ''
        move_number = self.get_number(cur_row, cur_col)
        calc_row = move_number // self.get_width()
        calc_col = move_number % self.get_width()
        total_string = self.position_zero(target_pos, False) + self.position_zero(cur_pos, False)
        cur_row, cur_col = self.current_position(calc_row, calc_col)
        while move_number != self.get_number(target_row, target_col):
            if cur_col < target_col:
                cur_string = 'urrdl'
                if cur_row == 0:
                    cur_string = 'drrul'
            elif cur_col > target_col:
                cur_string = 'ulldr'
                if cur_row == 0:
                    cur_string = 'dllur'
            elif cur_row < target_row:
                zero_row, zero_col = self.current_position(0, 0)
                if zero_row == cur_row and zero_col < cur_col:
                    cur_string = 'dru'
                elif zero_row == cur_row and zero_col > cur_col:
                    cur_string = 'dlu'
                else:
                    cur_string = 'lddru'
            self.update_puzzle(cur_string)
            total_string += cur_string
            cur_row, cur_col = self.current_position(calc_row, calc_col)

        assert move_number == self.get_number(target_row, target_col)
        return total_string

    def solve_interior_tile(self, target_row, target_col):
        """
        Place correct tile at target position
        Updates puzzle and returns a move string
        """
        # replace with your code
        assert self.lower_row_invariant(target_row, target_col)
        total_string = self.position_tile(self.current_position(target_row, target_col),(target_row, target_col))
        total_string += self.position_zero((target_row, target_col - 1), True)
        assert self.lower_row_invariant(target_row, target_col - 1), str(self)
        return total_string

    def solve_col0_tile(self, target_row):
        """
        Solve tile in column zero on specified row (> 1)
        Updates puzzle and returns a move string
        """
        assert self.lower_row_invariant(target_row, 0)
        fix_move = "ruldrdlurdluurddlur";
        total_moves = self.position_zero((target_row - 1, 0), False)
        if self.get_number(target_row, 0) != target_row * self.get_width():
            total_moves += self.position_tile(self.current_position(target_row, 0), (target_row - 1, 1))
            total_moves += self.position_zero((target_row - 1, 0), True)
            self.update_puzzle(fix_move)
            total_moves += fix_move
        total_moves += self.position_zero((target_row - 1, self.get_width() - 1), False)
        assert self.lower_row_invariant(target_row - 1, self.get_width() - 1)
        # replace with your code
        return total_moves

    #############################################################
    # Phase two methods

    def row0_invariant(self, target_col):
        """
        Check whether the puzzle satisfies the row zero invariant
        at the given column (col > 1)
        Returns a boolean
        """
        # replace with your code
        target_row = 0
        zero_row,zero_col = self.current_position(0, 0)
        if zero_row != target_row or zero_col != target_col:
            return False
        return self.lower_row(2) and self.righter_col(0, target_col) and self.righter_col(1, target_col - 1)

    def row1_invariant(self, target_col):
        """
        Check whether the puzzle satisfies the row one invariant
        at the given column (col > 1)
        Returns a boolean
        """
        return self.lower_row_invariant(1, target_col)

    def solve_row0_tile(self, target_col):
        """
        Solve the tile in row zero at the specified column
        Updates puzzle and returns a move string
        """
        # replace with your code
        assert self.row0_invariant(target_col)
        fix_move = "urdlurrdluldrruld"
        total_moves = self.position_zero((1, target_col -1), True)
        if target_col != self.get_number(0, target_col):
            total_moves += self.position_tile(self.current_position(0, target_col), (1, target_col - 1))
            total_moves += self.position_zero((1, target_col - 2), True)
            self.update_puzzle(fix_move)
            total_moves += fix_move
        return total_moves

    def solve_row1_tile(self, target_col):
        """
        Solve the tile in row one at the specified column
        Updates puzzle and returns a move string
        """
        # replace with your code
        assert self.row1_invariant(target_col)
        total_moves = self.position_tile(self.current_position(1, target_col), (1, target_col))
        total_moves += self.position_zero((0, target_col), False)
        assert self.row0_invariant(target_col)
        return total_moves

    ###########################################################
    # Phase 3 methods

    def solve_2x2(self):
        """
        Solve the upper left 2x2 part of the puzzle
        Updates the puzzle and returns a move string
        """
        fix_move = 'rdlu'
        total_moves = self.position_zero((0,0), False)
        while self.get_number(0, 0) != 0 or self.get_number(0, 1) != 1:
            self.update_puzzle(fix_move)
            total_moves += fix_move

        return total_moves

    def solve_puzzle(self):
        """
        Generate a solution string for a puzzle
        Updates the puzzle and returns a move string
        """
        total_moves = self.position_zero((self.get_height() -1, self.get_width() - 1), False)
        for row in range(self.get_height()-1, 1, -1):
            for col in range(self.get_width() - 1, -1, -1):
                if col != 0:
                    total_moves += self.solve_interior_tile(row, col)
                else:
                    total_moves += self.solve_col0_tile(row)
        for col in range(self.get_width() - 1, 1, -1):
            total_moves += self.solve_row1_tile(col)
            total_moves += self.solve_row0_tile(col)
        total_moves += self.solve_2x2()
        return total_moves

# Start interactive simulation
