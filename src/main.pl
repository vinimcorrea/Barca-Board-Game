:- consult('gameplay.pl').


/**
 * play
 * 
 * Entry point of the game
 */
play :-
    menu.


/*
 * menu
 *
 * Starts initial menu and wait for input.
*/
menu :-
    repeat,
    print_game_name, nl,
    display_menu,
    format("Option: ", []), read_input(Option),
    exec_initial_menu(Option), !.
 

display_menu :-
    write('    Made available under GNU General Public License v3, copyrighted material used under fair use for education'), nl,
    write('    We lay claim only over the software; this software cannot be used for commercial purposes'), nl, nl,
    write('1. Instructions'), nl,
    write('2. Play'), nl,
    write('0. Quit'), nl, nl.
/*
 * read_input(+Input)
 * 
 * Read the next Prolog term from the current input stream.
 * If a not valid term is provided, it tries again until it receives one that is.
*/
read_input(Input):-
    repeat,
    catch(read(Input), _Error, false),
    !.

/*
 * exec_initial_menu(+Option)
 *
 * Executes initial menu Option.
*/
exec_initial_menu(0):-
    abort.
exec_initial_menu(2):-
    repeat,
    display_play_options,
    format("Option: ", []), read_input(Option),
    exec_play_options(Option), !.
exec_initial_menu(1):-
    print_game_rules,
    read_input(_),
    menu.


% print_game_name/0
print_game_name :- 
    nl,
    write('   01010      1010101010  10101          01010101   0101010101'), nl,
    write('   1     1    0        1  0     0      1            1        0'), nl,
    write('   0      0   1        0  1      1    0             0        1'), nl,
    write('   1     1    0        1  0     0    1              1        0'), nl,
    write('   0   0      1        0  1   1      0              0        1'), nl,
    write('   1 1        0101010101  0 0        1              1        0'), nl,
    write('   0    0     1        0  1   1      0              0101010101'), nl,
    write('   1     1    0        1  0    0     1              1        0'), nl,
    write('   0      0   1        0  1     1     0             0        1'), nl,
    write('   1     1    0        1  0      0     1            1        0'), nl,
    write('   01010      1        0  1       1      01010101   0        1'), nl. 



/*
 * print_game_rules
 *
 * Displays game instructions.
*/
print_game_rules :-
    nl, write('BOARD'), nl,
    write('   This game is played in a 10x10 squared board.'), nl, nl,
    write('PIECES'), nl,
    write('   l -> white lion'), nl,
    write('   e -> white elephant'), nl,
    write('   m -> white mouse'), nl,
    write('   L -> black lion'), nl,
    write('   E -> black elephant'), nl,
    write('   M -> black mouse'), nl,
    write('   O -> watering hole'), nl, nl,
    write('SETUP'), nl,
    write('   Each player has 2 lions, 2 elephants and 2 mouses in the game.'), nl, nl,
    write('RULES'), nl,
    write('   Mice move horizontally or vertically. Lions only move diagonally. Elephants move horizontally, vertically or diagonally. Animals'), nl,
    write('   can move any number of vacant squares in a single direction. Animals cannot jump over other animals.'), nl, nl,
    write('   A players animals can be adjacent to each other. However, for opposing sides, mice fear lions. Lions fear elephants. And elephants fear mice.'), nl, nl,
    write('   An animal cannot be moved next to the animal it fears, except for trapped animals.'), nl, nl,
    write('   An animal is scared when it is next to the animal it fears.'), nl, nl,
    write('   A scared animal must escape to a safe location before other animals can be moved. When many are scared, choose one to move.'), nl, nl,
    write('   Watering holes are near the center of the game board. The player who has animals on 3 watering holes at the same time wins!'), nl, nl,
    write('   A player can still win if animals are scared or trapped. Animals are never removed.'), nl, nl,
    write('PRESS ANY BUTTON TO GO BACK TO MAIN MENU'), nl, nl.

% error on maplist
% start_game/0
%start_game :-
%    write('Print board here'), nl,
%    create_board(Board),
%    display_board(Board).


/**
 * display_play_options
 *
 * Displays play modes.
 */
display_play_options :-
    nl,
    write('1. Human vs Human'), nl,
    write('2. Human vs Computer'), nl,
    write('3. Computer vs Computer'), nl,
    write('0. Back'), nl.


/*
 * exec_play_options(+Option)
 *
 * Executes play modes menu Option.
*/
exec_play_options(0) :-
    menu.
% human vs human
exec_play_options(1) :-
    initial_board(FirstGameState),
    display_game(FirstGameState),
    game_loop(1, FirstGameState).
% human vs computer
exec_play_options(2) :-
    repeat,
    display_choose_level,
    format("Option: ", []), read_input(Option),
    exec_choose_level(h-c, Option), !.
% computer vs computer
exec_play_options(3) :-
    display_choose_level,
    format("Option: ", []), read_input(Option),
    exec_choose_level(c-c, Option), !.

/*
 * display_choose_level
 *
 * Displays computer A.I. levels.
*/
display_choose_level :-
    nl,
    write('1. Level 1 (Random Moves)'), nl,
    write('2. Level 2 (Greedy Algorithm)'), nl,
    write('0. Back'), nl.


/*
 * exec_choose_level(+Mode, +Option)
 *
 * Executes choose levels menu Option.
*/
exec_choose_level(_, 0) :-
    menu.
% level 1
%exec_choose_level(Mode, 1) :-
%%    play_game(Mode, 1, 5),
%    !.
%
%% level 2
%exec_choose_level(Mode, 2) :-
%    play_game(Mode, 3, 7).



