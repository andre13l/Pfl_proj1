% Reads a Column and Row
read_input(X, Y) :-
    write('Column: '),
    read(X),
    write('Row: '),
    read(Y).

% Return the value at the given position
value_in_board(Board, X, Y, Value) :-
    nth0(Y, Board, Line),
    nth0(X, Line, Value).

% Check if selected spot is playable
validate_choice(Board, X, Y) :-
    value_in_board(Board, X, Y, Value),
    Value \= 0.

% Predicate to read input, checks if is available and return 
choose_piece(Board, X, Y) :-
    read_input(X, Y),
    validate_choice(Board, X, Y).

choose_piece(Board, _, _) :-
    write('Invalid choice, try again.\n'),
    read_input(X, Y),
    choose_piece(Board, X, Y).

% Replace Element E in List L at index I, REsulting in List K
replace_index(I, L, E, K) :-
    nth0(I, L, _, R),
    nth0(I, K, E, R).

% Replace a value in the board
replace(Board, X, Y, Value, NewBoard) :-
    nth0(Y, Board, Line),
    replace_index(X, Line, Value, NewLine),
    replace_index(Y, Board, NewLine, NewBoard).

% performs the change in the board, replaces current piece with 0 and empty space with player code
move(Board, X, Y, A, NewBoard) :-
    replace(Board, X, Y, A, NewBoard).

% Make a move
make_move('Player', GameState, PlayerS, NewGameState) :-
    move(Board, X, Y, A, NewBoard).

