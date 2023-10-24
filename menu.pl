% clear/0
% Clears the screen, for better user experience (UX)
clear :- write('\33\[2J').
% stacks_logo/0
% Prints stacks
stacks_logo :- 
    write('  _____ _____ _____  ____ _   _   _____'), nl,
    write(' / ____|_   _|  _  |/ ___| | / / / ____|'), nl,
    write('| (___   | | | |_| | |   | |/ / | (___  '), nl,
    write(' \\___ \\  | | |  _  | |   |   |   \\___ \\'), nl,
    write(' ____) | | | | | | | |___| |\\ \\  ____) |'), nl,
    write('|_____/  |_| |_| |_|\\____|_| \\_\\|_____/'), nl.
% menu/0
% Prints the menu
menu :-
    clear,
    stacks_logo,
    nl,
    write('1. Player vs Player'), nl,
    write('2. Player vs Computer'), nl,
    write('3. Computer vs Computer'), nl,
    write('4. Exit'), nl,
    write('Choose an option: ').

