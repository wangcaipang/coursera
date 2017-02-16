"""
Student code for Word Wrangler game
"""

import urllib2
import codeskulptor
import poc_wrangler_provided as provided

WORDFILE = "assets_scrabble_words3.txt"


# Functions to manipulate ordered word lists

def remove_duplicates(list1):
    """
    Eliminate duplicates in a sorted list.

    Returns a new sorted list with the same elements in list1, but
    with no duplicates.

    This function can be iterative.
    """
    results = []
    if len(list1) == 0:
        return results
    results.append(list1[0])
    for idx in range(1, len(list1)):
        last_ele = results[len(results) -1]
        cur_ele = list1[idx]
        if last_ele != cur_ele:
            results.append(cur_ele)
    return results

def intersect(list1, list2):
    """
    Compute the intersection of two sorted lists.

    Returns a new sorted list containing only elements that are in
    both list1 and list2.

    This function can be iterative.
    """
    results = []
    start_x = 0
    start_y = 0
    while start_x < len(list1) and start_y < len(list2):
        list1_ele = list1[start_x]
        list2_ele = list2[start_y]
        if list1_ele == list2_ele:
            results.append(list1_ele)
            start_x += 1
            start_y += 1
        elif list1_ele > list2_ele:
            start_y += 1
        else:
            start_x += 1
    return results

# Functions to perform merge sort

def merge(list1, list2):
    """
    Merge two sorted lists.

    Returns a new sorted list containing those elements that are in
    either list1 or list2.

    This function can be iterative.
    """
    results = []
    start_x = 0
    start_y = 0
    while start_x < len(list1) or start_y < len(list2):
        if start_x >= len(list1):
            results.append(list2[start_y])
            start_y += 1
            continue
        if start_y >= len(list2):
            results.append(list1[start_x])
            start_x += 1
            continue
        list1_ele = list1[start_x]
        list2_ele = list2[start_y]
        if list1_ele < list2_ele:
            results.append(list1_ele)
            start_x += 1
        else:
            results.append(list2_ele)
            start_y += 1
    return results

def merge_sort(list1):
    """
    Sort the elements of list1.

    Return a new sorted list with the same elements as list1.

    This function should be recursive.
    """
    if len(list1) == 0:
        return []
    if len(list1) == 1:
        return [list1[0]]
    center = len(list1) // 2
    return merge(merge_sort(list1[0:center]), merge_sort(list1[center:]))

# Function to generate all strings for the word wrangler game

def gen_all_strings(word):
    """
    Generate all strings that can be composed from the letters in word
    in any order.

    Returns a list of all strings that can be formed from the letters
    in word.

    This function should be recursive.
    """
    if word == '':
        return ['']
    else:
        rest_strings = gen_all_strings(word[1:])[0:]
        for idx in range(len(rest_strings)):
            cur_string = rest_strings[idx]
            for str_idx in range(len(cur_string) + 1):
                rest_strings.append(cur_string[0:str_idx] + word[0] + cur_string[str_idx:])

    return (merge_sort(rest_strings))

# Function to load words from a file

def load_words(filename):
    """
    Load word list from the file named filename.

    Returns a list of strings.
    """
    return []

def run():
    """
    Run game.
    """
    words = load_words(WORDFILE)
    wrangler = provided.WordWrangler(words, remove_duplicates,
                                     intersect, merge_sort,
                                     gen_all_strings)
    provided.run_game(wrangler)
