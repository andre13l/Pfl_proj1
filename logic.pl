% initial(+Identifier, -Board)
% Initial board 
initial_state([
  [b,b,b,b,b],
  [0,0,0,0,0],
  [0,0,0,0,0],
  [0,0,0,0,0],
  [0,0,0,0,0],
  [0,0,0,0,0],
  [2,2,2,2,2]
]).

% valid_move(+Board, +X, +Y, +NX, +NY)
% checks if the move is valid
valid_move(Board, X, Y, NX, NY) :-
  value_in_board(Board, X, Y, Value),
  player_piece(_, Value, Z),
  Xma is X + Z, Xmi is X - Z, Yma is Y + Z, Ymi is Y - Z,
  between(Xmi, Xma, NX), between(Ymi, Yma, NY),
  value_in_board(Board, NX, NY, Value2),
  Value2 == 0.
  
% get the list of possible moves
moves_list(Board, X, Y, ListOfMoves) :-
  findall([NX, NY], valid_move(Board, X, Y, NX, NY), ListOfMoves).

% Pieces codes for board representation
code(0, 32). % ascii code for space
code(1, 65). % A - Player 2 stack 1
code(2, 66). % B - Player 2 stack 2
code(3, 67). % C - Player 2 stack 3
code(4, 68). % D - Player 2 stack 4
code(a, 97). % a - Player 1 stack 1
code(b, 98). % b - Player 1 stack 2
code(c, 99). % c - Player 1 stack 3
code(d, 100). % d - Player 1 stack 4

% Pieces codes for each player
player_piece('Player 2', 1, 3).
player_piece('Player 2', 2, 2).
player_piece('Player 2', 3, 1).
player_piece('Player 2', 4, 0).
player_piece('Player 1', a, 3).
player_piece('Player 1', b, 2).
player_piece('Player 1', c, 1).
player_piece('Player 1', d, 0).

% Switch player
player_swap('Player 1', 'Player 2').
player_swap('Player 2', 'Player 1').

print_board_middle_separator(1):- write('|\n').
print_board_middle_separator(X):- write('+ - '), 
                                  X1 is X-1, 
                                  print_board_middle_separator(X1).

% When the counter reaches 0, it ends
print_matrix([], 8, _).
print_matrix([L|T], N, X) :-
  code(1,P), write(' '), write(N), write(' | '), write(' | '),
  N1 is N + 1,
  print_line(L), nl,
  N < X - 1, write('---+  | - '), print_board_middle_separator(5),
  print_matrix(T, N1, X).
print_matrix(_, _, X):-
  write('---+  *---'),
  print_board_separator(5).

% Prints a line of the board
print_line([]):-
  write(' ').
print_line([C|L]) :-
  code(C, P),put_code(P), write(' | '),
  print_line(L).

print_header_numbers(Inicial, Inicial):-
  write('\n').
print_header_numbers(Inicial, Final):-
  write(' '), write(Inicial), write(' |'), N1 is Inicial + 1, print_header_numbers(N1, Final).

print_separator(0):-
  write('|\n').
print_separator(X):-
  write('+---'), X1 is X-1, print_separator(X1).

print_board_separator(1):-
  write('*\n').
print_board_separator(X):-
  write('+---'), X1 is X-1, print_board_separator(X1).

print_header(P, X):-
  write('      |'),
  print_header_numbers(0, X),
  write('      '),
  print_separator(X),
  write('---+  *---'),
  print_board_separator(X).

% Prints the board according to its state
display_game(Board):- 
  nl, code(0, P),
  print_header(P, 5),
  print_matrix(Board, 0, 7),
  write('         ').

% read_column(-Column)
% predicate to read column from user
read_column(Column) :-
  write('| Column (0-4) - '),
  read_number(0, 4, Column).

% read_row(-Row)
% predicate to read row from user
read_row(Row) :-
  write('| Row (0-6) - '),
  read_number(0, 6, Row).

% Reads a Column and Row 
read_input(X, Y) :-
    read_row(Y),
    read_column(X).

% Return the value at the given position
value_in_board(Board, X, Y, Value) :-
    nth0(Y, Board, Line),
    nth0(X, Line, Value).

% Check if selected spot is playable
validate_choice(Board, X, Y) :-
    value_in_board(Board, X, Y, Value),
    Value == -1.

% player_in_board(+Board, +X, +Y, -PlayerS)
% returns in Player a string representing the player or fails if space is empty.
player_in_board(Board, X, Y, PlayerS):-
    value_in_board(Board, X, Y, Value),
    player_piece(PlayerS, Value, _).

% Predicate to read input, checks if is available and return 
choose_piece(Board, X, Y) :-
    read_input(X, Y).    

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

% show a numbered list of possible moves
show_moves([], _).
show_moves([S|Rest], N):-
  write(N), write(S), nl,
  N1 is N + 1,
  show_moves(Rest, N1),!.

% Make a move
make_move('Player', GameState, PlayerS, NewGameState) :-
  Player = 'Player', format('~n~`*t ~a turn ~`*t~57|~n', [PlayerS]), 
  write('Choose a piece to move: '), nl,
  choose_piece(GameState, X, Y),
  value_in_board(GameState, X, Y, Value),
  write('- Selected piece: '), write(Value), nl,
  moves_list(GameState, X, Y, ListOfMoves),
  show_moves(ListOfMoves, 1),
  write('Choose a spot to move: '), nl,
  read_input(X1, Y1),
  value_in_board(GameState, X1, Y1, Value1),
  format('- Selected spot: X: ~d, Y: ~w \n', [X1,Y1]),
  sleep(2),
  move(GameState, X1, Y1, Value, NewGame),
  move(NewGame, X, Y, Value1, NewGameState).

% Turn for moving a piece
turn(GameState, Player, PlayerS, NextPlayer):-
  make_move(Player, GameState, PlayerS, NewGameState),
  player_swap(PlayerS,EnemyS),
  clear, 
  display_game(NewGameState),
  turn(NewGameState, NextPlayer, EnemyS, Player).

% Start the game 
start_game :-
  initial_state(Board),
  display_game(Board),
  turn(Board, P1, 'Player 1', P2).







