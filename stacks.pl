:- consult('logic.pl').

select_piece(Value):-
    initial_state(Board),
    value_in_board(Board, X, Y, Value),
    write(X), write(Y), write(Value), nl.