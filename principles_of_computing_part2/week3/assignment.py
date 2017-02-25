"""
Mini-max Tic-Tac-Toe Player
"""

import poc_ttt_gui
import poc_ttt_provided as provided

# Set timeout, as mini-max can take a long time
import codeskulptor
codeskulptor.set_timeout(60)

# SCORING VALUES - DO NOT MODIFY
SCORES = {provided.PLAYERX: 1,
          provided.DRAW: 0,
          provided.PLAYERO: -1}

def mm_move(board, player):
    """
    Make a move on the board.

    Returns a tuple with two elements.  The first element is the score
    of the given board and the second element is the desired move as a
    tuple, (row, col).
    """
    winner = board.check_win()
    score = SCORES[provided.DRAW]
    if winner != None:
        if winner == provided.PLAYERX:
            score = 1
        elif winner == provided.PLAYERO:
            score = -1
        return score, (-1, -1)
    else:
        is_self = player == provided.PLAYERX
        score = -2 if is_self else 2
        move_list = board.get_empty_squares()
        move = move_list[0]
        for idx in range(len(move_list)):
            next_board = board.clone()
            each_move = move_list[idx]
            next_board.move(each_move[0], each_move[1], player)
            child_score = mm_move(next_board, provided.switch_player(player))
            if (is_self and child_score[0] > score) or (not is_self and child_score[0] < score):
                score = child_score[0]
                move = each_move
        return score,move

def move_wrapper(board, player, trials):
    """
    Wrapper to allow the use of the same infrastructure that was used
    for Monte Carlo Tic-Tac-Toe.
    """
    move = mm_move(board, player)
    assert move[1] != (-1, -1), "returned illegal move (-1, -1)"
    return move[1]

# Test game with the console or the GUI.
# Uncomment whichever you prefer.
# Both should be commented out when you submit for
# testing to save time.

# provided.play_game(move_wrapper, 1, False)
poc_ttt_gui.run_gui(3, provided.PLAYERO, move_wrapper, 1, False)
