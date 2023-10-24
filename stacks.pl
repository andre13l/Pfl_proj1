% Define the board as a 5x5 grid
board([
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty]
]).

% Define the stacks as a list of 5 stacks, each containing 1, 2, 3, or 4 pieces
stacks([
    [piece],
    [piece, piece],
    [piece, piece, piece],
    [piece, piece, piece, piece]
]).

% Define the players as player1 and player2
player(player1).
player(player2).

% Define the initial state of the game
initial_state(state(Board, Stacks, Player)) :-
    board(Board),
    stacks(Stacks),
    player(Player).

% Print the board
print_board :-
    write(' - - - - -            player 1:          player 2:    '), nl,
    write('|b|b|b|b|b|           a: stack of 1      z: stack of 1'), nl,
    write('|+|+|+|+|+|           b: stack of 2      y: stack of 2'), nl,
    write('| | | | | |           c: stack of 3      x: stack of 3'), nl,
    write('|-|-|-|-|-|           d: stack of 4      w: stack of 4'), nl,
    write('| | | | | |'), nl,
    write('|-|-|-|-|-|'), nl,
    write('| | | | | |'), nl,
    write('|-|-|-|-|-|'), nl,
    write('| | | | | |'), nl,
    write('|-|-|-|-|-|'), nl,
    write('| | | | | |'), nl,
    write('|-|-|-|-|-|'), nl,
    write('| | | | | |'), nl,
    write('|+|+|+|+|+|'), nl,
    write('|y|y|y|y|y|'), nl,
    write(' - - - - - '), nl.

% Print the state of the game
print_state(state(Board, Stacks, Player)) :-
    write(' - - - - -            player 1:          player 2:    '), nl,
    write('|'), print_board(Board, Stacks), nl,
    write(' - - - - - '), nl,
    write('Player '), write(Player), write(' turn.'), nl.

    % Make a move by placing a stack on the board
    make_move(state(Board, Stacks, Player), NewState, Stack, Position) :-
        % Check if the stack is valid
        valid_stack(Stack, Stacks),
        % Check if the position is valid
        valid_position(Position),
        % Check if the position is empty
        empty_position(Position, Board),
        % Place the stack on the board
        place_stack(Stack, Position, Board, NewBoard),
        % Remove the stack from the list of stacks
        remove_stack(Stack, Stacks, NewStacks),
        % Switch the player
        switch_player(Player, NewPlayer),
        % Create the new state
        NewState = state(NewBoard, NewStacks, NewPlayer).
