% Select piece to move
sel_move (X,Y):-
    write('Select piece to move: '),
    read(X),
    write('Select position to move to: '),
    read(Y).
% Select position to move to
sel_pos (X,Y):-
    write('Select position to move to: '),
    read(X),
    write('Select position to move to: '),
    read(Y).
% Check if the move is valid

