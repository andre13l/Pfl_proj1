:- consult('board.pl').
:- use_module(library(random)).

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


% Switch player
player_swap('Player 1', 'Player 2').
player_swap('Player 2', 'Player 1').

% Return the value at the given position
% Value will store the value in [X, Y]
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
    random(0, 4, X),
    random(0, 6, Y),
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
  value_in_board(Board, X, Y, Value),
  player_piece(_, Value, Z),
  Xma is X + Z, Xmi is X - Z, Yma is Y + Z, Ymi is Y - Z,
  between(Xmi, Xma, NX), between(Ymi, Yma, NY),
  value_in_board(Board, NX, NY, Value2),
  Value2 == 0.
  
% get the list of possible moves
moves_list(Board, X, Y, ListOfMoves) :-
  findall([NX, NY], valid_move(Board, X, Y, NX, NY), ListOfMoves).

% Make a move
make_move('Player', GameState, PlayerS, NewGameState) :-
  format('~n~`*t ~a turn ~`*t~57|~n', [PlayerS]), 
  write('Attack or Move? (1 - Attack, 2 - Move): '), nl,
  random(1, 2, Choice),
  Choice == 2,
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
  random(1, R, N),
  nth1(N, ListOfMoves, [X1, Y1]),
  value_in_board(GameState, X1, Y1, Value1),
  format('- Selected spot: X: ~d, Y: ~w \n', [X1,Y1]),
  sleep(1),
  move(GameState, X1, Y1, Value, NewGame),
  move(NewGame, X, Y, Value1, NewGameState),
  % check_winner(...).


can_attack(Board, X, Y, NX, NY) :-
  value_in_board(Board, X, Y, Value),
  % player_piece(_, Value, Z),
  Xma is X + 1, Xmi is X - 1, Yma is Y + 1, Ymi is Y - 1,
  between(Xmi, Xma, NX), between(Ymi, Yma, NY),
  value_in_board(Board, NX, NY, Value2),
  player_piece(Player, Value, _),
  player_piece(Player1, Value2, _),
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

reduce_stack(GameState, X, Y, NewGameState) :-
  value_in_board(GameState, X, Y, Value),
  player_piece(Player, Value, Z),
  Z == 3,
  replace(GameState, X, Y, 0, NewGameState).

attack(GameState, X, Y, X1, Y1, NewGameState):-
  reduce_stack(GameState, X1, Y1, NewGameState1),
  value_in_board(NewGameState1, X1, Y1, Value),
  Y2 is Y1 + 1,
  move(NewGameState1, X1, Y2, Value, NewGameState2).

make_move('Player', GameState, PlayerS, NewGameState) :-
  write('Choose a piece to attack with: '), nl,
  choose_piece(GameState, X, Y, PlayerS),
  value_in_board(GameState, X, Y, Value),
  write('- Selected piece: '), write(Value), nl,
  attack_list(GameState, X, Y, ListOfMoves),
  write('Possible attacks:'), nl,
  show_moves(ListOfMoves, 1, R),
  length(ListOfMoves, R),
  write('Choose a possible attack: '), nl,
  random(1, R, N),
  nth1(N, ListOfMoves, [X1, Y1]),
  format('- Selected spot: X: ~d, Y: ~w \n', [X1,Y1]),

  reduce_stack(GameState, X1, Y1, NewGameState1),
  value_in_board(NewGameState1, X1, Y1, Value1),
  write('Spot for attacked to go: '), nl,
  random(0, 4, X2),
  random(0, 6, Y2),
  sleep(1),
  move(NewGameState1, X2, Y2, Value1, NewGameState2),               % victim retreats one place (ex: goes to [X1, Y2]), but we still need to choose the direction
  move(NewGameState2, X, Y, 0, NewGameState3),                      % empty the place where the attacker was [X, Y]
  move(NewGameState3, X1, Y1, Value, NewGameState),                 % the place where the victim was [X1, Y1] will have now the attacker piece (Value)
  % check_winner(...).

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
  print_score('Player 1', 10, 0, 'Player 2', 10, 0),
  turn(Board, P1, 'Player 1', P2).



% used to check only in the homespaces
% check_homespaces([], _, _).
% check_homespaces([V|Rest], Player, N) :-
%   NrPieces is N + V,
%   check_homespaces(Rest, Player, NrPieces).

% used all over the grid
% count_pieces([], _, _).
% count_pieces([V|Rest], Player, N) :-
%   NrPieces is N + V,
%   count_pieces(Rest, Player, NrPieces).

% ------------------------------------------
% check_winner_hs(Board, CurrentPlayer, Player1, Player2, N1, N2, Winner) :-       % N as start as 0
%   try to put this on move function
%   nth0(0, Board, List0),                          % check the first sublist (homespaces)
%   nth0(6, Board, List6),                          % check the last sublist (homespaces)
%   check_homespaces(List0, Player2, N2),           % count how many Player2 pieces are in the Player1 homespaces
%   check_homespaces(List6, Player1, N1),           % count how many Player1 pieces are in the Player2 homespaces
% 
%   (
%     Check for condition 1: Four or more pieces in Opponent homespaces
%     (CurrentPlayer == Player1, N1 >= 4);
%     (CurrentPlayer == Player2, N2 >= 4)
%   ) -> Winner = CurrentPlayer.


% need to test these 2 functions
check_winner_hs(Board, X, Y, Player, N, Winner) :- 
  value_in_board(Board, X, Y, Value),
  player_piece(Player, Value, _),
  NextX is X + 1,                     % traverse the row corresponding to the homespaces
  NrPieces is N + Value,
  (
    (NrPieces >= 4)
  ) -> Winner = Player, !,            % we can stop now we found the winner
  check_winner_hs(Board, NextX, Y, Player, NrPieces, Winner).

check_winner_elim_pieces(Board, X, Y, Player, N, Winner) :-
  value_in_board(Board, X, Y, Value),
  player_piece(Player, Value, _),
  (
    % Check for condition 2: Six or more opponent pieces eliminated
    % same as the number of pieces being <= 4
    % (CurrentPlayer == Player1, length(CurrentPlayerPieces, N), N >= 6);
    % (CurrentPlayer == Player2, length(OpponentPieces, N), N >= 6)
  ) -> Winner = CurrentPlayer.


check_winner(Board, X, Y, Player, N, Winner) :-
  ( 
    check_winner_hs(Board, X, Y, Player, N, Winner);
    check_winner_elim_pieces(Board, X, Y, Player, N, Winner)
  ),
  menu_winner(10, '*', Winner).
