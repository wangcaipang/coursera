"""
Student portion of Zombie Apocalypse mini-project
"""

import random
import poc_grid
import poc_queue
import poc_zombie_gui

# global constants
EMPTY = 0
FULL = 1
FOUR_WAY = 0
EIGHT_WAY = 1
OBSTACLE = 5
HUMAN = 6
ZOMBIE = 7


class Apocalypse(poc_grid.Grid):
    """
    Class for simulating zombie pursuit of human on grid with
    obstacles
    """

    def __init__(self, grid_height, grid_width, obstacle_list = None,
                 zombie_list = None, human_list = None):
        """
        Create a simulation of given size with given obstacles,
        humans, and zombies
        """
        poc_grid.Grid.__init__(self, grid_height, grid_width)
        if obstacle_list != None:
            for cell in obstacle_list:
                self.set_full(cell[0], cell[1])
        if zombie_list != None:
            self._zombie_list = list(zombie_list)
        else:
            self._zombie_list = []
        if human_list != None:
            self._human_list = list(human_list)
        else:
            self._human_list = []

    def clear(self):
        """
        Set cells in obstacle grid to be empty
        Reset zombie and human lists to be empty
        """
        poc_grid.Grid.clear(self)
        self._zombie_list = []
        self._human_list = []

    def add_zombie(self, row, col):
        """
        Add zombie to the zombie list
        """
        self._zombie_list.append((row, col))

    def num_zombies(self):
        """
        Return number of zombies
        """
        return len(self._zombie_list)

    def zombies(self):
        """
        Generator that yields the zombies in the order they were
        added.
        """
        # replace with an actual generator
        return (zombie for zombie in self._zombie_list)

    def add_human(self, row, col):
        """
        Add human to the human list
        """
        self._human_list.append((row, col))

    def num_humans(self):
        """
        Return number of humans
        """
        return len(self._human_list)

    def humans(self):
        """
        Generator that yields the humans in the order they were added.
        """
        # replace with an actual generator
        return (human for human in self._human_list)

    def compute_distance_field(self, entity_type):
        """
        Function computes and returns a 2D distance field
        Distance at member of entity_list is zero
        Shortest paths avoid obstacles and use four-way distances
        """
        entry_list = self._human_list if entity_type == HUMAN else self._zombie_list
        distance_grid = [[self._grid_height * self._grid_width for dummy_x in range(self._grid_width)]for dummy_y in range(self._grid_height)]
        visited = poc_grid.Grid(self._grid_height, self._grid_width)
        boundary = poc_queue.Queue()
        for item in entry_list:
            boundary.enqueue(item)
            distance_grid[item[0]][item[1]] = 0
        while len(boundary) != 0:
            item = boundary.dequeue()
            visited.set_full(item[0], item[1])
            neighbors = poc_grid.Grid.four_neighbors(self, item[0], item[1])
            for each_neighbor in neighbors:
                if visited.is_empty(each_neighbor[0], each_neighbor[1]) and poc_grid.Grid.is_empty(self, each_neighbor[0], each_neighbor[1]):
                    visited.set_full(each_neighbor[0], each_neighbor[1])
                    distance_grid[each_neighbor[0]][each_neighbor[1]] = distance_grid[item[0]][item[1]] + 1
                    boundary.enqueue(each_neighbor)
        return distance_grid

    def move_humans(self, zombie_distance_field):
        """
        Function that moves humans away from zombies, diagonal moves
        are allowed
        """
        for idx in range(len(self._human_list)):
            pos = self._human_list[idx]
            neighbors = poc_grid.Grid.eight_neighbors(self, pos[0], pos[1])
            next_pos = pos
            for neighbor in neighbors:
                if poc_grid.Grid.is_empty(self, neighbor[0], neighbor[1]) and zombie_distance_field[neighbor[0]][neighbor[1]] >= zombie_distance_field[next_pos[0]][next_pos[1]]:
                    next_pos = neighbor
            self._human_list[idx] = next_pos

    def move_zombies(self, human_distance_field):
        """
        Function that moves zombies towards humans, no diagonal moves
        are allowed
        """
        for idx in range(len(self._zombie_list)):
            pos = self._zombie_list[idx]
            neighbors = poc_grid.Grid.four_neighbors(self, pos[0], pos[1])
            next_pos = pos
            for neighbor in neighbors:
                if poc_grid.Grid.is_empty(self, neighbor[0], neighbor[1]) and human_distance_field[neighbor[0]][neighbor[1]] <= human_distance_field[next_pos[0]][next_pos[1]]:
                    next_pos = neighbor
            self._zombie_list[idx] = next_pos

# Start up gui for simulation - You will need to write some code above
# before this will work without errors
poc_zombie_gui.run_gui(Apocalypse(30, 40))
