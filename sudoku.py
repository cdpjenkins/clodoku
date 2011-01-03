# My third sudoku solver, inspired by Peter Norvig's one
#
# TODO add some tests
# TODO tidy up
# Allow more than one result to be returned by the search (ie for board with
# non-unique solution
# Consider iterative deepening search for the above if it is slow
# Port to Clojure

import copy

DEBUG = False

dimension=3
width = dimension*dimension
height = dimension*dimension

# A cell is a set of possible values. If the set has only one element then
# the value is definite

all_cells = [(x, y) for x in xrange(9)
                    for y in xrange(9)]

def parse_cell_value(cell_str):
    if cell_str in "123456789":
        return int(cell_str)
    else:
        return -1

def cell_value(cell):
    """Returns the value of a cell. This kind of assumes that the cell
    actully has a known value.
    """
    return list(cell)[0]

def cell_value_at(board, pos):
    return cell_value(board[pos])

def cell_to_str(cell):
    if len(cell) == 1:
        return str(list(cell)[0])
    else:
        return "_"

def set_cell_value(board, pos, val):
    if DEBUG: print "set_cell_value(pos==%s, val==%d)" % (pos, val)
    if len(board[pos]) == 1:
        if cell_value_at(board, pos) != val:
            return None
    if val not in board[pos]:
        return None
    
    other_possibilities = board[pos] - set([val])
    for possibility in other_possibilities:
        if not eliminate(board, pos, possibility):
            return None
    for peer in peers[pos]:
        if not eliminate(board, peer, val):
            return None
    return board

# A Board is a mapping from (x, y) coord pair to the cell

def print_board(board):
    for y in xrange(height):
        print "".join([cell_to_str(board[(x, y)])
                       for x in xrange(width)])

def make_board(values):
    board = dict([((x, y), set(xrange(1, 10)))
                  for x in xrange(9)
                  for y in xrange(9)])

    for y, row in enumerate(values):
        for x, val in enumerate(row):
            val = parse_cell_value(val)
            if val != -1:
                set_cell_value(board, (x, y), val)
    return board

# A region is either a row or a column or a 3x3 box
rows = [[(x, row) for x in xrange(9)]
          for row in xrange(9)]
cols = [[(col, y) for y in xrange(9)]
          for col in xrange(9)]
def get_box_cells(box):
    top_x = 3 * (box % 3)
    top_y = 3 * (box / 3)
    return [(x, y) for y in xrange(top_y, top_y + 3)
                   for x in xrange(top_x, top_x + 3)]
boxes = [get_box_cells(box_num) for box_num in xrange(9)]
# regions is a list of regions, each of which is a list of coord pairs for
# the cells in that region
regions = rows + cols + boxes

# We need a mapping from cells to regions
def regions_for_cell(pos):
    return [region for region in regions if pos in region]
cell_to_regions = dict([((x, y), regions_for_cell((x, y)))
                       for x in xrange(9)
                       for y in xrange(9)])
def peers_for_cell(pos):
    regions = cell_to_regions[pos]
    return set(reduce(list.__add__, regions)) - set([pos])
peers = dict([ ((x, y), peers_for_cell((x, y)))
               for x in xrange(9)
               for y in xrange(9)])

def eliminate(board, pos, val):
    if DEBUG: print "eliminate(pos==%s, val==%d)" % (pos, val)
    if not val in board[pos]:
        return board
    elif val in board[pos]:
        board[pos] -= set([val])
        if len(board[pos]) == 0:
            # contradiction
            return None
        elif len(board[pos]) == 1:
            if not set_cell_value(board, pos, list(board[pos])[0]):
                return None
            # this will have the effect of recursively calling eliminate on
            # all our peers
        # find any of the regions that this cell is in. If any of the regions
        # has only one place for this val then set it
        # TODO if any region has zero places for this val then contradiction
        for reg in cell_to_regions[pos]:
            poses = [p for p in reg if val in board[p]]
            if len(poses) == 0:
                return None
            elif len(poses) == 1:
                if not set_cell_value(board, poses[0], val):
                    return None
    return board

def check_validity(board):
    # TODO this only really checks the validity of a complated board.
    for region in regions:
        vals = set([cell_value_at(board, pos) for pos in region])
        if vals != set(range(1, 10)):
            return False
    return True
                      
def is_completed(board):
    return all(len(board[pos]) == 1 for pos in all_cells)

def depth_first_search(board):
    if board == None: # or not check_validity(board):
        return []
    if is_completed(board):
        return [board]
    global bo
    bo = board
    _, pos = min((len(board[pos]), pos)
                 for pos in all_cells if len(board[pos]) > 1)
    results = []
    for possibility in board[pos]:
        thar_copy = copy.deepcopy(board)
        thar_copy = set_cell_value(thar_copy, pos, possibility)
        if not thar_copy:
            # Reached a dead end; try next possibility
            pass
        else:
            result = depth_first_search(thar_copy)
#            print "result:", result
            results += depth_first_search(thar_copy)
#    print results
    return results

def time_me_do():
    import time
    start = time.clock()
    b = make_board(open("board6.txt"))
    b = depth_first_search(b)
    end = time.clock()
    print end - start


