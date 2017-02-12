"""
Clone of 2048 game.
"""

import poc_2048_gui
import random
# Directions, DO NOT MODIFY
UP = 1
DOWN = 2
LEFT = 3
RIGHT = 4

# Offsets for computing tile indices in each direction.
# DO NOT MODIFY this dictionary.
OFFSETS = {UP: (1, 0),
           DOWN: (-1, 0),
           LEFT: (0, 1),
           RIGHT: (0, -1)}

def merge(line):
    """
    Function that merges a single row or column in 2048.
    """
    new_line = []
    for num in line:
        if num != 0:
            new_line.append(num)
    for dummy_idx in range(len(line) - len(new_line)):
        new_line.append(0)
    for idx in range(len(new_line) - 1):
        if new_line[idx] == 0:
            return new_line
        else:
            if new_line[idx] == new_line[idx+1]:
                new_line[idx] = new_line[idx] * 2
                new_line.pop(idx+1)
                new_line.append(0)
    return new_line

class TwentyFortyEight:
    """
    Class to run the game logic.
    """

    def __init__(self, grid_height, grid_width):
        self._grid_height = grid_height
        self._grid_width = grid_width
        self.reset()
        self._get_entry_map()

    def _get_entry_map(self):
        """
        get entry map.
        """
        self._entry_map = {}
        self._entry_map[UP] = []
        self._entry_map[RIGHT] = []
        self._entry_map[LEFT] = []
        self._entry_map[DOWN] = []
        for idx in range(self._grid_width):
            self._entry_map[UP].append((0, idx))
            self._entry_map[DOWN].append((self._grid_height - 1, idx))
        for idx in range(self._grid_height):
            self._entry_map[LEFT].append((idx, 0))
            self._entry_map[RIGHT].append((idx, self._grid_width -1))

    def reset(self):
        """
        Reset the game so the grid is empty except for two
        initial tiles.
        """
        self._cells = []
        for dummy_row in range(self._grid_height):
            row = []
            for dummy_col in range(self._grid_width):
                row.append(0)
            self._cells.append(row)
        self.new_tile()
        self.new_tile()

    def __str__(self):
        """
        Return a string representation of the grid for debugging.
        """
        # replace with your code
        grid_str = "["
        for idx in range(self._grid_height):
            grid_str += str(self._cells[idx])
            if idx < self._grid_height -1 :
                grid_str += "\n "

        return grid_str + "]"

    def get_grid_height(self):
        """
        Get the height of the board.
        """
        # replace with your code
        return self._grid_height

    def get_grid_width(self):
        """
        Get the width of the board.
        """
        # replace with your code
        return self._grid_width

    def move(self, direction):
        """
        Move all tiles in the given direction and add
        a new tile if any tiles moved.
        """
        # replace with your code
        entry = self._entry_map[direction]
        offset = OFFSETS[direction]
        has_changed = False
        if direction == UP or direction == DOWN:
            max_offset = self._grid_height
        else:
            max_offset = self._grid_width
        for enter_grid in entry:
            coordinate_list = [(enter_grid[0],enter_grid[1])]
            step = 1
            while step < max_offset:
                last_grid = coordinate_list[len(coordinate_list) -1]
                new_row = last_grid[0] + offset[0]
                new_col = last_grid[1] + offset[1]
                coordinate_list.append((new_row,new_col))
                step += 1
            before_grid_list = []
            for coordinate in coordinate_list:
                before_grid_list.append(self.get_tile(coordinate[0], coordinate[1]))
            new_grid_list = merge(before_grid_list)
            for coordinate in coordinate_list:
                if (not has_changed) and self.get_tile(coordinate[0], coordinate[1]) != new_grid_list[0]:
                    has_changed = True
                self.set_tile(coordinate[0], coordinate[1], new_grid_list[0])
                new_grid_list.pop(0)
        if has_changed:
            self.new_tile()




    def new_tile(self):
        """
        Create a new tile in a randomly selected empty
        square.  The tile should be 2 90% of the time and
        4 10% of the time.
        """
        zero_list = []
        for row_idx in range(self._grid_height):
            for col_idx in range(self._grid_width):
                if self._cells[row_idx][col_idx] == 0:
                    zero_list.append((row_idx, col_idx))

        if len(zero_list) >= 1:
            new_tile = 4 if random.random() <= 0.1 else 2
            random_pos = zero_list[random.randrange(0, len(zero_list))]
            self.set_tile(random_pos[0], random_pos[1], new_tile)


    def set_tile(self, row, col, value):
        """
        Set the tile at position row, col to have the given value.
        """
        # replace with your code
        self._cells[row][col] = value

    def get_tile(self, row, col):
        """
        Return the value of the tile at position row, col.
        """
        # replace with your code
        return self._cells[row][col]

poc_2048_gui.run_gui(TwentyFortyEight(4, 5))
