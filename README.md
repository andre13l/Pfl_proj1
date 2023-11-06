# STACKS_3
up201806860 - André Moreira Leal Leonor - 50% <br>
up202108807 - Rui Pedro Braga Carvalho - 50%

## Installation and Execution

The game is executed by loading the play.pl file in sicstus and running the predicat play.

## Description of the game

This is a 1 vs 1 board game. <br>
Each player has 10 pieces (either in red or black), who can be combined in stacks, with up to 4 pieces per stack. At the beginning of the game, all pieces are combined in stacks of 2.<br>
A player move can consist in either moving a piece, attacking, unstacking or stacking pieces.<br>
The winner is the one that can place 4 or more of his own pieces into one or more of the opponent's homespaces, or eliminate 6 or more opponent's pieces.

## Game Logic
### Internal Game State Representation

The board consists in a list of 7 lists (5 for 7 board), one of the players pieces' are represented by the letters in caps while the other player pieces' are represented by the opposite. A score is always being shown bellow the board. The letters used to represent the stacks are used in alfabetic order, a is 1, b is 2, c is 3 and d is 4.

### Game State Visualization

The display_game predicate, in board.pl file, prints the current state of the board using a series of predicates making use of different characters that help making the board more appealing. 

### Move Validation and Execution

There are 4 types of moves (moving, attacking, unstacking and stacking), for each of them there are predicates that make a list of the possible moves.  A read_input predicate is used so that a user can select a specific place of the board. In case of selecting something from the list, a read_number predicate present in the input.pl file is used, to user that the input is between upper and lower bounds.

### List of Valid Moves

A show_moves predicate is used so that the player can choose a possible move from the list previously created. For that a findall is used.

### End of Game

We were not able to fully implement end game related predicates.

### Game State Evaluation

We did not implement this.

### Computer Plays 

This is barelly implemented.

## Conclusions

Although our game is apealing and extremely easy to be played in a user's prespective, it is not finished as it lacks a way to determine it's end. It also does not include different levels for the computer play so futurely it would be required to also add it.

## Bibliography

https://www.swi-prolog.org/
