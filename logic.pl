:- consult('board.pl').

% Pieces codes for board representation
code(0, 32). % ascii code for space
code(1, 65). % A - Player 1 stack 1
code(2, 66). % B - Player 1 stack 2
code(3, 67). % C - Player 1 stack 3
code(4, 68). % D - Player 1 stack 4
code(a, 97). % a - Player 2 stack 1
code(b, 98). % b - Player 2 stack 2
code(c, 99). % c - Player 2 stack 3
code(d, 100). % d - Player 2 stack 4
/*
code(0, 32). % ascii code for space
code(a, 97). % a - Player 1 stack 1
code(b, 98). % b - Player 1 stack 2
code(c, 99). % c - Player 1 stack 3
code(d, 100). % d - Player 1 stack 4
code(1, 65). % A - Player 2 stack 1
code(2, 66). % B - Player 2 stack 2
code(3, 67). % C - Player 2 stack 3
code(4, 68). % D - Player 2 stack 4
*/
/*
player_piece('Player 1', 1).
player_piece('Player 2', 0).
*/
% Pieces codes for each player
player_piece('Player 2', 1, 3).
player_piece('Player 2', 2, 2).
player_piece('Player 2', 3, 1).
player_piece('Player 2', 4, 0).
player_piece('Player 1', a, 3).
player_piece('Player 1', b, 2).
player_piece('Player 1', c, 1).
player_piece('Player 1', d, 0).

end_spot('Player 1', 6).
end_spot('Player 2', 0).

att(2,1).
att(3,1).
att(4,1).
att(3,2).
att(4,2).
att(4,3).

% Switch player
player_swap('Player 1', 'Player 2').
player_swap('Player 2', 'Player 1').

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
    read_column(X),
    read_row(Y).

% Return the value at the given position
value_in_board(Board, X, Y, Value) :-
    nth0(Y, Board, Line),
    nth0(X, Line, Value).

% Check if selected spot is playable
validate_choice(Board, X, Y) :-
    value_in_board(Board, X, Y, Value),
    Value == -1.

% player_in_board(+Board, +X, +Y, -PlayerS)
% returns in PlayerS a string representing the player or fails if space is empty.
player_in_board(Board, X, Y, PlayerS):-
    value_in_board(Board, X, Y, Value),
    player_piece(PlayerS, Value, _).

% Predicate to read input, checks if is available and return 
choose_piece(Board, X, Y, Player) :-
    read_input(X, Y),
    value_in_board(Board, X, Y, Value),
    player_piece(Player, Value, _).    

choose_piece(Board, X, Y, Player) :-
    write('Invalid choice!'), nl,
    choose_piece(Board, X, Y, Player).

% Replace Element E in List L at index I, Resulting in List K
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
show_moves([], _, _).
show_moves([S|Rest], N, R):-
  write(N), write(S), nl,
  N1 is N + 1,
  show_moves(Rest, N1, R).

% valid_move(+Board, +X, +Y, +NX, +NY)
% checks if the move is valid
valid_move(Board, X, Y, NX, NY) :-
  value_in_board(Board, NX, NY, Value2),
  value_in_board(Board, X, Y, Value),
  player_piece(_, Value, Z),
  Xma is X + Z, Xmi is X - Z, Yma is Y + Z, Ymi is Y - Z,
  between(Xmi, Xma, NX), between(Ymi, Yma, NY),
  Value2 == 0.

  
% get the list of possible moves
moves_list(Board, X, Y, ListOfMoves) :-
  findall([NX, NY], valid_move(Board, X, Y, NX, NY), ListOfMoves).

check_end_row(Board, X, Y, 'Player 1', NewBoard, S) :-
  Y == 6,
  Q is S + 1,
  S = Q,
  replace(Board, X, Y, 0, NewBoard).

check_end_row(Board, X, Y, 'Player 1', NewBoard, S) :-
  Y < 6,
  NewBoard = Board.

check_end_row(Board, X, Y, 'Player 2', NewBoard, S) :-
  Y == 0,
  Q is S + 1,
  S = Q,
  replace(Board, X, Y, 0, NewBoard).

check_end_row(Board, X, Y, 'Player 2', NewBoard, S) :-
  Y > 0,
  NewBoard = Board.

% Make a move
make_move('Player', GameState, PlayerS, NewGameState, 2) :-
  write('Choose a piece to move: '), nl,
  choose_piece(GameState, X, Y, PlayerS),
  value_in_board(GameState, X, Y, Value),
  write('- Selected piece: '), write(Value), nl,
  % read_input(NX, NY),
  % attack(GameState, X, Y, NX, NY, NewGameState, 1),
  moves_list(GameState, X, Y, ListOfMoves),
  write('Possible moves:'), nl, 
  show_moves(ListOfMoves, 1, R),
  length(ListOfMoves, R),
  write('Choose a possible move: '), nl,
  read_number(1, R, N),
  nth1(N, ListOfMoves, [X1, Y1]),
  value_in_board(GameState, X1, Y1, Value1),
  format('- Selected spot: X: ~d, Y: ~w \n', [X1,Y1]),
  sleep(1),
  move(GameState, X1, Y1, Value, NewGame),
  move(NewGame, X, Y, Value1, NewGameState1),
  check_end_row(NewGameState1, X1, Y1, PlayerS, NewGameState, S).

% Attack
make_move('Player', GameState, PlayerS, NewGameState, 1) :-
  write('Choose a piece to attack with: '), nl,
  choose_piece(GameState, X, Y, PlayerS),
  value_in_board(GameState, X, Y, Value),
  write('- Selected piece: '), write(Value), nl,
  attack_list(GameState, X, Y, ListOfMoves),
  write('Possible attacks:'), nl,
  show_moves(ListOfMoves, 1, R),
  length(ListOfMoves, R),
  write('Choose a possible attack: '), nl,
  read_number(1, R, N),
  nth1(N, ListOfMoves, [X1, Y1]),
  format('- Selected spot: X: ~d, Y: ~w \n', [X1,Y1]),

  reduce_stack(GameState, X1, Y1, NewGameState1, D),
  value_in_board(NewGameState1, X1, Y1, Value1),
  write('Spot for attacked to go: '), nl,
  read_input(X2, Y2),
  move(NewGameState1, X2, Y2, Value1, NewGameState2),               % victim retreats one place (ex: goes to [X1, Y2]), but we still need to choose the direction
  move(NewGameState2, X, Y, 0, NewGameState3),                      % empty the place where the attacker was [X, Y]
  move(NewGameState3, X1, Y1, Value, NewGameState4),                 % the place where the victim was [X1, Y1] will have now the attacker piece (Value)
  check_end_row(NewGameState4, X1, Y1, PlayerS, NewGameState, S).

possible_separate_moves(Board, X, Y, NX, NY) :-
  value_in_board(Board, X, Y, Value),
  Xma is X + 1, Xmi is X - 1, Yma is Y + 1, Ymi is Y - 1,
  between(Xmi, Xma, NX), between(Ymi, Yma, NY),
  value_in_board(Board, NX, NY, 0).

list_separate_moves(Board, X, Y, ListOfMoves) :-
  findall([NX, NY], possible_separate_moves(Board, X, Y, NX, NY), ListOfMoves).

% Separate Stack
make_move('Player', GameState, PlayerS, NewGameState, 3) :-
  write('Choose a piece to separate: '), nl,
  choose_piece(GameState, X, Y, PlayerS),
  value_in_board(GameState, X, Y, Value),
  write('- Selected piece: '), write(Value), nl,
  player_piece(PlayerS, Value, Z),
  Z < 3,
  NewZ is Z + 1,
  player_piece(PlayerS, NewValue, NewZ),
  player_piece(PlayerS, Value1, 3),
  write('Spot for new piece to go: '), nl,
  list_separate_moves(GameState, X, Y, ListOfMoves),
  write('Possible moves:'), nl,
  length(ListOfMoves, R),
  show_moves(ListOfMoves, 1, R),
  write('Choose a possible move: '), nl,
  read_number(1, R, N),
  nth1(N, ListOfMoves, [X1, Y1]),
  format('- Selected spot: X: ~d, Y: ~w \n', [X1,Y1]),
  move(GameState, X1, Y1, Value1, NewGameState1),
  replace(NewGameState1, X, Y, NewValue, NewGameState).

% Join Stacks
make_move('Player', GameState, PlayerS, NewGameState, 4) :-
  write('Choose a piece to join: '), nl,
  choose_piece(GameState, X, Y, PlayerS),
  value_in_board(GameState, X, Y, Value),
  write('- Selected piece: '), write(Value), nl,
  player_piece(PlayerS, Value, Z),
  Z > 0,
  list_separate_moves(GameState, X, Y, ListOfMoves),
  write('Possible moves:'), nl,
  length(ListOfMoves, R),
  show_moves(ListOfMoves, 1, R),
  write('Choose a possible move: '), nl,
  read_number(1, R, N),
  nth1(N, ListOfMoves, [X1, Y1]),
  format('- Selected spot: X: ~d, Y: ~w \n', [X1,Y1]),
  value_in_board(GameState, X1, Y1, Value1),
  player_piece(PlayerS, Value1, NewZ),
  plus(Value, Value1, NewValue),
  NewValue < 5,
  player_piece(PlayerS, NewValue, NewZ),
  player_piece(PlayerS, Value1, 0),
  write('Spot for new piece to go: '), nl,
  read_input(X1, Y1),
  move(GameState, X1, Y1, Value1, NewGameState1),
  replace(NewGameState1, X, Y, NewValue, NewGameState).

can_attack(Board, X, Y, NX, NY) :-
  value_in_board(Board, X, Y, Value),
  % player_piece(_, Value, Z),
  Xma is X + 1, Xmi is X - 1, Yma is Y + 1, Ymi is Y - 1,
  between(Xmi, Xma, NX), between(Ymi, Yma, NY),
  value_in_board(Board, NX, NY, Value2),
  player_piece(Player, Value, _),
  player_piece(Player1, Value2, _),
  att(Value, Value2),
  Player \= Player1,
  Value2 \= 0.

attack_list(Board, X, Y, ListOfMoves) :-
  findall([NX, NY], can_attack(Board, X, Y, NX, NY), ListOfMoves).

reduce_stack(GameState, X, Y, NewGameState) :-
  value_in_board(GameState, X, Y, Value),
  % value_in_board(GameState, X1, Y1, Value1),
  player_piece(Player, Value, Z),
  % player_piece(Player1, Value1, Z1),
  NewZ is Z + 1,
  NewZ < 4, 
  player_piece(Player, NewValue, NewZ),
  replace(GameState, X, Y, NewValue, NewGameState).

reduce_stack(GameState, X, Y, NewGameState, D) :-
  value_in_board(GameState, X, Y, Value),
  player_piece(Player, Value, Z),
  Z == 3,
  D = D - 1,
  replace(GameState, X, Y, 0, NewGameState).

attack(GameState, X, Y, X1, Y1, NewGameState):-
  reduce_stack(GameState, X1, Y1, NewGameState1),
  value_in_board(NewGameState1, X1, Y1, Value),
  Y2 is Y1 + 1,
  move(NewGameState1, X1, Y2, Value, NewGameState2).


% Turn for moving a piece
turn(GameState, Player, P1pieces, P1stacks, PlayerS, NextPlayer, P2pieces, P2stacks):-
  format('~n~`*t ~a turn ~`*t~57|~n', [PlayerS]), 
  write('What do you want to do? (1 - Attack, 2 - Move, 3 - Separate Stack, 4 - Join Stacks): '), nl,
  read_number(1, 4, Choice),
  make_move(Player, GameState, PlayerS, NewGameState, Choice),
  player_swap(PlayerS,EnemyS),
  clear, 
  display_game(NewGameState),
  print_score('Player 1', P1pieces, P1stacks, 'Player 2', P2pieces, P2stacks),
  turn(NewGameState, NextPlayer, EnemyS, Player).

% Start the game 
start_game :-
  initial_state(Board),
  display_game(Board),
  print_score('Player 1', 10, 0, 'Player 2', 10, 0),
  turn(Board, P1, 10, 0, 'Player 1', P2, 10, 0).
