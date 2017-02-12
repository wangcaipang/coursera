"""
Merge function for 2048 game.
"""

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
