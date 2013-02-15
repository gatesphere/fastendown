<!--@+leo-ver=5-thin-->
<!--@+node:peckj.20130212140318.1379: * @file README.md-->
<!--@@color-->
<!--@@language md-->
<!--@@tabwidth -2-->
<!--@+others-->
<!--@+node:peckj.20130215091009.1410: ** @md readme-->
fastendown
==========

A newLISP AI for Button Up!

Overview
--------
Button Up! is an abstract strategy game with a limited search space, but interesting mechanics.
Rules for Button Up! can be read [here][1].
  
FastenDown is a minimaxing player for Button Up!, written in [newLisp][2], that searches the 
entire statespace before determining a move.  This is possible in a reasonable amount of time 
because a single round of Button Up! is guaranteed to last only nine moves, and each possible game state is guaranteed to have at most three possible moves to consider, with the last move having at most two legal moves.  Draws are possible, but extremely rare.  Initial board layout is random at 
the beginning of each round.

  [1]: http://boiteajeux.net/jeux/btu/aide.php
  [2]: http://www.newlisp.org/

License
-------
FastenDown is BSD Licensed.  Please see [license/license.txt][3] for more info.

  [3]: https://raw.github.com/gatesphere/fastendown/master/license/license.txt
  
Requirements
------------
To use FastenDown, you need a copy of newLISP.  FastenDown was developed and tested on 
newLISP 10.4.5.

Features
--------
FastenDown has the following features:

  - Any combination of:
    - Human players
    - Random players
    - Minimax players
  - Random board layouts
  - Custom board layouts
  - Variable turn order
  
Usage
-----
### Interactive
Basic interactive usage is simple:

    ./fastendown.lsp

In this mode, FastenDown will prompt you for various information:

    $ ./fastendown.lsp
    Board: (r)andom, or (c)ustom? r
    Red: (h)uman, (r)andom, or (m)inimax? h
    Black: (h)uman, (r)andom, or (m)inimax? m
    Who goes first, (r)ed, or (b)lack? r
    0: W
    1: R
    2: R
    3: B
    4: B
    5: B
    6: W
    7: W
    8: R
    HumanPlayer, color R
    Valid moves are: (7 6 0)
    Please enter your move:
    
Here we see the user has requested a random board setup, red player is human, black player is minimax, and red goes first.  The board layout is printed out, and the human player is prompted 
for their move, which is an integer from the list of valid moves.  In Button Up!, buttons are 
deposited on stacks in a clockwise fashion.  In FastenDown, this is represented by going straight down, and looping back to zero when moving past the last stack.  Empty stacks are not represented, as they do not have any effect on gameplay.  The readout of a stack follows the convention that the leftmost piece is on top of the stack, going down the stack as you proceed right.  So that:

    0: R W B B R
    1: R W W B
    
Is representative of the following two stacks:

    R
    W R
    B W
    B W
    R B
    - -
    0 1

#### Custom board layout

If using a custom board layout, enter it as a quoted list.  The following input will produce the board layout example above:

    $ ./fastendown.lsp
    Board: (r)andom, or (c)ustom? c
    Please enter board layout: '((R W B B R) (R W W B))

#### Quitting
FastenDown will end once the round is over.  If you wish to exit earlier, and at least one of the players is human, instead of entering a move, enter `q`.  That will exit the game immediately.  Otherwise, you will likely have to kill the process.  Though, the game should run pretty quickly on hardware produced within the last decade.

#### Scoring
Score is calculated at the end of the game.  A score of 0 is a draw, and is extremely rare.  A positive score is points for the red player, and a negative score is points for the black player.  Please see the [rules][1] for more information.

### Command-line arguments
FastenDown may be invoked with exactly 4 arguments to set up the game without going through the prompts.  An example of setting up the previous game is like so:

    ./fastendown.lsp r h m r
    
The arguments are in the same order as they are in the prompts.

To start with a custom board, do the following:

    ./fastendown.lsp "'((R W B B R) (R W W B))" h m r
    
That will set up the game with the following board layout:

    0: R W B B R
    1: R W W B

What's with all the strange comments in the source code?
--------------------------------------------------------
The funky comments in the source code (the ones that look like 
`;@+node:peckj.20130212140318.1380: * @file fastendown.lsp`) are a side-effect of my chosen 
editor, [Leo][4].  If you're going to be working on this program, using Leo to edit the fastendown.leo file is recommended.  Leo is amazing.

  [4]: http://webpages.charter.net/edreamleo/front.html

Disclaimer
----------
This program is designed for educational use and training only.  Please don't cheat, 
it's not sportsman-like.

<!--@-others-->
<!--@-leo-->
