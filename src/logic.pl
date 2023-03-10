:- use_module(library(lists)).
:- consult('board.pl').
:- consult('utils.pl').
:- consult('moves.pl').

% movement_rule(?mice, ?orthogonal)
movement_rule(mice, orthogonal).
movement_rule(lion, diagonal).
movement_rule(elephant, both).


% select_piece(+GameState, +Player, +X-+Y)
% print if piece is from player or not
select_piece(GameState, Player, X-Y) :-
    char_at_position(GameState, X-Y, Piece),
    is_piece_from_player(Player, Piece),
    player(Player, Color),
    piece(Piece, Animal, Color),
    write('You chose: '), write(Color), write(' '), write(Animal), nl.



% Define a predicate to update the board based on a move
% update_board(+X-+Y, +NewX-+NewY, +Board, -NewBoard)
update_board(X-Y, NewX-NewY, Board, NewBoard) :-
    char_at_position(Board, X-Y, Piece), 
    coordinates_to_position(X, Y, Position),
    (\+ member(Position, [d4, d7, g4, g7]) ->
    set_char_at_position(Board, X, Y, -, TempBoard) ;
    set_char_at_position(Board, X, Y, 'O', TempBoard)), % Clear the starting position
    set_char_at_position(TempBoard, NewX, NewY, Piece, NewBoard). 
    


% Set a character at a position on the board
% set_char_at_position(+Board, +X, +Y, +Char, -NewBoard)
set_char_at_position(Board, X, Y, Char, NewBoard) :-
    nth1(X, Board, Row),
    N1 is X-1,
    Y1 is Y-1,
    replace(Row, Y1, Char, NewRow),
    replace(Board, N1, NewRow, NewBoard).

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 0,
    I1 is I-1,
    replace(T, I1, X, R).

% return if piece is from player
% is_piece_from_player(+Player, +Piece)
is_piece_from_player(Player, Piece) :-
    player(Player, Color),
    piece(Piece, _, Color).

% return a list with the positions of the player
% get_player_pieces(+Board, +Player, -Pieces)
get_player_pieces(Board, Player, Pieces) :-
    findall(Position, (player(Player, Color), piece(Piece, _, Color), char_at_position(Board, X-Y, Piece), coordinates_to_position(X, Y, Position)), Pieces).

% check if a piece is scared
% is_any_piece_scared(+Board, +Pieces)
is_any_piece_scared(Board, [H|_]) :-
        position_to_coordinates(H, X, Y),
        char_at_position(Board, X-Y, Piece),
        is_scared(X, Y, Piece, Board), !.

is_any_piece_scared(Board, [_|T]) :-
        is_any_piece_scared(Board, T).

% get_piece_scared(+Board, +Pieces, -ScaredPiece)
get_piece_scared(Board, [H|_], ScaredPiece) :-
    position_to_coordinates(H, X, Y),
    char_at_position(Board, X-Y, Piece),
    is_scared(X, Y, Piece, Board), !,
    coordinates_to_position(X, Y, ScaredPiece).

get_piece_scared(Board, [_|T], ScaredPiece) :-
    get_piece_scared(Board, T, ScaredPiece).

% mark an X in the board with the possible moves
% show_possible_moves(+GameState, +Moves, -NewGameState)
show_possible_moves(A, [], B) :-
    A = B.
show_possible_moves(GameState, [Position|T], D) :-
    position_to_coordinates(Position, X, Y),
    set_char_at_position(GameState, X, Y, 'X', NewGameState),
    show_possible_moves(NewGameState, T, D).

% print_board_with_moves(+GameState, +Animal, +Position)
print_board_with_moves(GameState, mice, Position) :-
    possible_moves_orthogonally(GameState, Position, Moves),
    show_possible_moves(GameState, Moves, NewGameState),
    display_game(NewGameState).

print_board_with_moves(GameState, lion, Position) :-
    possible_moves_diagonally(GameState, Position, Moves),
    show_possible_moves(GameState, Moves, NewGameState),
    display_game(NewGameState).

print_board_with_moves(GameState, elephant, Position) :-
    possible_moves_elephant(GameState, Position, Moves),
    show_possible_moves(GameState, Moves, NewGameState),
    display_game(NewGameState).