:- consult('input.pl').
:- consult('logic.pl').
:-use_module(library(lists)).
:-use_module(library(system)).
:-use_module(library(random)).
:-use_module(library(between)).

% clear/0
% Clears the screen, for better user experience (UX)
clear :- write('\33\[2J').

% stacks_logo/0
% Prints stacks
stacks_logo :- 
    write('          _____ _____ _____  ____ _   _   _____'), nl,
    write('         / ____|_   _|  _  |/ ___| | / / / ____|'), nl,
    write('        | (___   | | | |_| | |   | |/ / | (___  '), nl,
    write('         \\___ \\  | | |  _  | |   |   |   \\___ \\'), nl,
    write('         ____) | | | | | | | |___| |\\ \\  ____) |'), nl,
    write('        |_____/  |_| |_| |_|\\____|_| \\_\\|_____/'), nl.

% menu_header_format(+Header)
% prints the header of a menu (UX)
menu_header_format(Header):-
  format('~n~`*t ~p ~`*t~57|~n', [Header]).

% menu_empty_format/0
% Prints an empty line inside a menu (UX)
menu_empty_format :-
  format('*~t*~57|~n', []).

% menu_option_format(+Option, +Details)
% prints the option number and associated details in a menu-like format (UX)
menu_option_format(Option, Details):-
  format('*~t~d~t~15|~t~a~t~40+~t*~57|~n',
        [Option, Details]).

% menu_bottom_format/0
% Prints a row of '*' to end the menu (UX)
menu_bottom_format :-
  format('~`*t~57|~n', []).

% banner(+String)
% Prints a banner with a String inside (UX)
banner(String):-
  format('~n~`*t~57|~n', []),
  format('*~t~a~t*~57|~n', [String]),
  format('~`*t~57|~n', []).

% menu_text_format(+Text)
% Prints a center-aligned text inside a menu (UX)
menu_text_format(Text):-
  format('*~t~a~t*~57|~n', [Text]).

% menu_option(+Option)
% Sub-Menus related to option selected on the main menu

% Exit Main Menu
menu_option(0):-
  banner('Thank You For Playing'),
  stacks_logo.
% Player vs PLayer
menu_option(1):-
  clear,
  start_game,
  menu.
% Player vs Computer, need to choose Board Size
menu_option(2):-
  banner('Player vs Computer'),
  menu_board_size(Size),
  pc_menu_1(Size),
  clear, menu.
% Game Instructions
menu_option(3):-
  clear,
  menu_header_format('INSTRUCTIONS'),
  menu_empty_format,
  format('*~t~s~t~30|~t~c~t~23+~t*~57|~n', ["Player 1 stack 1", 65]),
  format('*~t~s~t~30|~t~c~t~23+~t*~57|~n', ["Player 1 stack 2", 66]),
  format('*~t~s~t~30|~t~c~t~23+~t*~57|~n', ["Player 1 stack 3", 67]),
  format('*~t~s~t~30|~t~c~t~23+~t*~57|~n', ["Player 1 stack 4", 68]),
  nl,
  format('*~t~s~t~30|~t~c~t~23+~t*~57|~n', ["Player 2 stack 1", 97]),
  format('*~t~s~t~30|~t~c~t~23+~t*~57|~n', ["Player 2 stack 2", 98]),
  format('*~t~s~t~30|~t~c~t~23+~t*~57|~n', ["Player 2 stack 3", 99]),
  format('*~t~s~t~30|~t~c~t~23+~t*~57|~n', ["Player 2 stack 4", 100]),

  menu_empty_format,

  menu_text_format('The game is played on a 5 x 5 grid (25 spaces in'),
  menu_text_format('total). Each player gets 10 playing pieces either'),
  menu_text_format('in red or black. Your pieces start on stacks of 2'),
  menu_text_format('on your own home spaces, adjacent to the main playing'),
  menu_text_format('grid.The aim of the game is to get four or more of '),
  menu_text_format('your pieces into theopponents home spaces, or to'),
  menu_text_format('eliminate six or more of the opponents pieces.'),
  menu_empty_format,
  menu_empty_format,
  menu_text_format('-- GENERAL RULES --'),
  menu_empty_format,
  menu_text_format('The game starts with a square chess-like empty board'),
  menu_text_format('Each player has an allocated symbol: "X" and "O"'),
  menu_text_format('"X" plays first, putting one white stone anywhere'),
  menu_text_format('on the board. After this move players take turns'),
  menu_text_format('placing their stones on empty cells adjacent to the'),
  menu_text_format('last opponents stone. If all cells adjacent to the'),
  menu_text_format('last opponents stone are occupied then the player gets'),
  menu_text_format('the right ("freedom") to place his stone on any empty'),
  menu_text_format('cell of the board. The game ends when the board is'),
  menu_text_format('filled with stones. The last player has the right to'),
  menu_text_format('pass on his last turn (and leave the last cell empty)'),
  menu_text_format('if placing his stone reduces his score.'),
  menu_empty_format,
  menu_bottom_format,
  menu.

% Information about the Project
menu_option(4):-
  menu_bottom_format,
  menu_empty_format,
  menu_text_format('Made By Andre Leonor and Rui Carvalho'),
  menu_empty_format,
  menu_bottom_format,
  menu.

% menu/0
% Prints the menu
menu :-
    stacks_logo,
    menu_header_format('Main Menu'),
    menu_empty_format,
    menu_option_format(1, 'Player vs Player'),
    menu_option_format(2, 'Player vs Computer'),
    menu_option_format(3, 'Game Instructions'),
    menu_option_format(4, 'Information about the project'),
    menu_empty_format,
    menu_option_format(0, 'Exit'),
    menu_empty_format,
    menu_bottom_format,

    write('| Choose an Option (0-4) - '),
    read_number(0, 4, Option),
    menu_option(Option).