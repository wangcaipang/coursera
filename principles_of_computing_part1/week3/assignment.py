"""
Monte Carlo Tic-Tac-Toe Player
"""

import random
import poc_ttt_gui
import poc_ttt_provided as provided
print provided.PLAYERO
# Constants for Monte Carlo simulator
# You may change the values of these constants as desired, but
#  do not change their names.
NTRIALS = 100         # Number of trials to run
SCORE_CURRENT = 1.0 # Score for squares played by the current player
SCORE_OTHER = 1.0   # Score for squares played by the other player

# Add your functions here.

def mc_trial(board, player):
    """
    mc_trial
    """

    while board.check_win() == None:
        random_move = random.choice(board.get_empty_squares())
        board.move(random_move[0], random_move[1], player)
        player = provided.switch_player(player)

def mc_update_scores(scores, board, player):
    """
    mc_trial
    """

    winner = board.check_win()
    dim = board.get_dim()
    for row in range(dim):
        for col in range(dim):
            each_square = board.square(row, col)
            if winner == provided.DRAW or each_square == provided.EMPTY:
                scores[row][col] += 0
            elif winner == player:
                if each_square == player:
                    scores[row][col] += SCORE_CURRENT
                else:
                    scores[row][col] += 0 - SCORE_OTHER
            else:
                if each_square == player:
                    scores[row][col] += 0 - SCORE_CURRENT
                else:
                    scores[row][col] += SCORE_OTHER

def get_best_move(board, scores):
    """
    mc_trial
    """

    empty_list = board.get_empty_squares()
    best_move = empty_list[0]
    for idx in range(1, len(empty_list)):
        square = empty_list[idx]
        if scores[square[0]][square[1]] > scores[best_move[0]][best_move[1]]:
            best_move = square
    return best_move

def mc_move(board, player, trials):
    """
    mc_trial
    """

    dim = board.get_dim()
    scores = [[0 for dummy_col in range(dim)] for dummy_row in range(dim)]
    tic = board.clone()
    while trials != 0:
        simulate_board = tic.clone()
        mc_trial(simulate_board, player)
        mc_update_scores(scores, simulate_board, player)
        trials -= 1
    return get_best_move(board, scores)


# Test game with the console or the GUI.  Uncomment whichever
# you prefer.  Both should be commented out when you submit
# for testing to save time.

# provided.play_game(mc_move, NTRIALS, False)
# poc_ttt_gui.run_gui(3, provided.PLAYERX, mc_move, NTRIALS, False)
