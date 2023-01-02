:- use_module(library(lists)).
:- consult('utils.pl').
:- consult('board.pl').

char_at_position(GameState, X-Y, Char) :-
    nth1(X, GameState, Row),
    nth1(Y, Row, Char).


% Define the occupied/3 predicate
% occupied(+Row, +Col, +Board)
occupied(X, Y, GameState) :-
    nth1(X, GameState, Row),
    nth1(Y, Row, Char),
    is_occupied(Char).

is_occupied(Char) :-
    Char \= '-' , Char \= 'O'.


% Define the afraid/4 predicate.
% Animals from the same player dont fear each other. Only if the animal is from a different player.
% return if a animal is scared from an adjacent animal.
% is_scared(+Piece, +Row, +Y, +Board)
is_trapped(GameState, mice, Position) :-
    possible_moves_orthogonally(GameState, Position, []).

is_trapped(GameState, lion, Position) :-
    possible_moves_diagonally(GameState, Position, []).

is_trapped(GameState, elephant, Position) :-
    possible_moves_elephant(GameState, Position, []).
    
is_scared(Row, Col, P, Board) :-
    piece(P, Type, Color),
    coordinates_to_position(Row, Col, Position),
    \+ is_trapped(Board, Type, Position),
    afraid_of(Type, AfraidType),
    (
        (Row1 is Row + 1, Col1 is Col + 1,
         occupied(Row1, Col1, Board),
         char_at_position(Board, Row1-Col1, Piece),
         piece(Piece, AfraidType, C),
         Color \= C, !)
    ;
        (Row1 is Row + 1, Col1 is Col,
         occupied(Row1, Col1, Board),
         char_at_position(Board, Row1-Col1, Piece),
         piece(Piece, AfraidType, C),
         Color \= C, !)
    ;
        (Row1 is Row + 1, Col1 is Col - 1,
         occupied(Row1, Col1, Board),
         char_at_position(Board, Row1-Col1, Piece),
         piece(Piece, AfraidType, C),
         Color \= C, !)
    ;
        (Row1 is Row, Col1 is Col + 1,
         occupied(Row1, Col1, Board),
         char_at_position(Board, Row1-Col1, Piece),
         piece(Piece, AfraidType, C),
         Color \= C, !)
    ;
        (Row1 is Row, Col1 is Col - 1,
         occupied(Row1, Col1, Board),
         char_at_position(Board, Row1-Col1, Piece),
         piece(Piece, AfraidType, C),
         Color \= C, !)
    ;
        (Row1 is Row - 1, Col1 is Col + 1,
         occupied(Row1, Col1, Board),
         char_at_position(Board, Row1-Col1, Piece),
         piece(Piece, AfraidType, C),
         Color \= C, !)
    ;
        (Row1 is Row - 1, Col1 is Col,
         occupied(Row1, Col1, Board),
         char_at_position(Board, Row1-Col1, Piece),
         piece(Piece, AfraidType, C),
         Color \= C, !)
    ;
        (Row1 is Row - 1, Col1 is Col - 1,
         occupied(Row1, Col1, Board),
         char_at_position(Board, Row1-Col1, Piece),
         piece(Piece, AfraidType, C),
         Color \= C, !)
    ).

possible_moves_elephant(GameState, Position, Moves) :-
    possible_moves_diagonally(GameState, Position, MovesD),
    possible_moves_orthogonally(GameState, Position, MovesO),

    append([], MovesD, Moves1),
    append(Moves1, MovesO, Moves).
    
    
possible_moves_diagonally(GameState, Position, Moves):-
        position_to_coordinates(Position, Row, Col),
        possible_moves_up_right_first(GameState,Row,Col,MovesUpRight),
        possible_moves_up_left_first(GameState,Row,Col,MovesUpLeft),
        possible_moves_down_right_first(GameState,Row,Col,MovesDownRight),
        possible_moves_down_left_first(GameState,Row,Col,MovesDownLeft),

        append([], MovesUpRight, Moves1),
        append(Moves1, MovesUpLeft, Moves2),
        append(Moves2, MovesDownRight, Moves3),
        append(Moves3, MovesDownLeft, Moves).


possible_moves_up_right_first(_, Row, Col, []) :-
    (Row > 9 ; Col > 9), !.

possible_moves_up_right_first(GameState, Row, Col, []) :-
    NewRow is Row+1,
    NewCol is Col+1,
    occupied(NewRow, NewCol, GameState), !.

possible_moves_up_right_first(GameState, Row, Col, []) :-
    NewRow is Row+1,
    NewCol is Col+1,
    char_at_position(GameState, Row-Col, Piece),
    is_scared(NewRow, NewCol, Piece, GameState), !.
    
possible_moves_up_right_first(GameState, Row, Col, Moves) :-
    NewRow is Row+1,
    NewCol is Col+1,
    char_at_position(GameState, Row-Col, Piece),
    coordinates_to_position(NewRow, NewCol,  Position),
    NR is NewRow +1,
    NC is NewCol +1,
    possible_moves_up_right(GameState, NR, NC, Piece, Moves1),
    append([Position], Moves1, Moves).


possible_moves_up_right(_, Row, Col, _, []) :-
   (Row > 10; Col > 10), !.

possible_moves_up_right(GameState, Row, Col, _, []) :-
    Row > 1,
    Col > 1,
    occupied(Row, Col, GameState), !.

possible_moves_up_right(GameState, Row, Col, Piece, []) :-
    Row > 1,
    Col > 1,
    is_scared(Row, Col, Piece, GameState), !.
                                   

possible_moves_up_right(GameState, Row, Col, Piece, Moves) :-
   NewRow is Row + 1,
   NewCol is Col +1,
   possible_moves_up_right(GameState, NewRow, NewCol, Piece, MovesUp),
   coordinates_to_position(Row, Col,  Position),
   append([Position], MovesUp, Moves).


possible_moves_up_left_first(_, Row, Col, []) :-
    (Row > 9 ; Col < 2), !.

possible_moves_up_left_first(GameState, Row, Col, []) :-
    NewRow is Row+1,
    NewCol is Col-1,
    occupied(NewRow, NewCol, GameState), !.

possible_moves_up_left_first(GameState, Row, Col, []) :-
    NewRow is Row+1,
    NewCol is Col-1,
    char_at_position(GameState, Row-Col, Piece),
    is_scared(NewRow, NewCol, Piece, GameState), !.

possible_moves_up_left_first(GameState, Row, Col, Moves) :-
    NewRow is Row+1,
    NewCol is Col-1,
    char_at_position(GameState, Row-Col, Piece),
    coordinates_to_position(NewRow, NewCol,  Position),
    NR is NewRow +1,
    NC is NewCol -1,
    possible_moves_up_left(GameState, NR, NC, Piece, Moves1),
    append([Position], Moves1, Moves).


possible_moves_up_left(_, Row, Col, _, []) :-
   (Row > 10; Col < 1), !.

possible_moves_up_left(GameState, Row, Col, _, []) :-
    Row > 1,
    Col > 1,
    occupied(Row, Col, GameState), !.

possible_moves_up_left(GameState, Row, Col, Piece, []) :-
    Row > 1,
    Col > 1,
    is_scared(Row, Col, Piece, GameState), !.

possible_moves_up_left(GameState, Row, Col, Piece, Moves) :-
   NewRow is Row + 1,
   NewCol is Col -1,
   possible_moves_up_left(GameState, NewRow, NewCol, Piece, MovesUp),
   coordinates_to_position(Row, Col,  Position),
   append([Position], MovesUp, Moves).


possible_moves_down_right_first(_, Row, Col, []) :-
    (Row < 2 ; Col > 9), !.
possible_moves_down_right_first(GameState, Row, Col, []) :-
    NewRow is Row-1,
    NewCol is Col+1,
    occupied(NewRow, NewCol, GameState), !.
possible_moves_down_right_first(GameState, Row, Col, []) :-
    NewRow is Row-1,
    NewCol is Col+1,
    char_at_position(GameState, Row-Col, Piece),
    is_scared(NewRow, NewCol, Piece, GameState), !.

possible_moves_down_right_first(GameState, Row, Col, Moves) :-
    NewRow is Row-1,
    NewCol is Col+1,
    char_at_position(GameState, Row-Col, Piece),
    coordinates_to_position(NewRow, NewCol,  Position),
    NR is NewRow -1,
    NC is NewCol +1,
    possible_moves_down_right(GameState, NR, NC, Piece, Moves1),
    append([Position], Moves1, Moves).


possible_moves_down_right(_, Row, Col, _, []) :-
   (Row < 1; Col > 10), !.

possible_moves_down_right(GameState, Row, Col, _, []) :-
    Row < 10,
    Col > 1,
    occupied(Row, Col, GameState), !.

possible_moves_down_right(GameState, Row, Col, Piece, []) :-
    Row < 10,
    Col > 1,
    is_scared(Row, Col, Piece, GameState), !.

possible_moves_down_right(GameState, Row, Col, Piece, Moves) :-
   NewRow is Row - 1,
   NewCol is Col +1,
   possible_moves_down_right(GameState, NewRow, NewCol, Piece, MovesUp),
   coordinates_to_position(Row, Col,  Position),
   append([Position], MovesUp, Moves).

possible_moves_down_left_first(_, Row, Col, []) :-
    (Row < 2 ; Col < 2), !.
possible_moves_down_left_first(GameState, Row, Col, []) :-
    NewRow is Row-1,
    NewCol is Col-1,
    occupied(NewRow, NewCol, GameState), !.
possible_moves_down_left_first(GameState, Row, Col, []) :-
    NewRow is Row-1,
    NewCol is Col-1,
    char_at_position(GameState, Row-Col, Piece),
    is_scared(NewRow, NewCol, Piece, GameState), !.

possible_moves_down_left_first(GameState, Row, Col, Moves) :-
    NewRow is Row-1,
    NewCol is Col-1,
    char_at_position(GameState, Row-Col, Piece),
    coordinates_to_position(NewRow, NewCol,  Position),
    NR is NewRow -1,
    NC is NewCol -1,
    possible_moves_down_left(GameState, NR, NC, Piece, Moves1),
    append([Position], Moves1, Moves).


possible_moves_down_left(_, Row, Col, _, []) :-
   (Row < 1; Col < 1), !.

possible_moves_down_left(GameState, Row, Col, _, []) :-
    Row < 10,
    Col < 10,
    occupied(Row, Col, GameState), !.

possible_moves_down_left(GameState, Row, Col, Piece, []) :-
    Row < 10,
    Col < 10,
    is_scared(Row, Col, Piece, GameState), !.

possible_moves_down_left(GameState, Row, Col, Piece, Moves) :-
   NewRow is Row - 1,
   NewCol is Col -1,
   possible_moves_down_left(GameState, NewRow, NewCol, Piece, MovesUp),
   coordinates_to_position(Row, Col,  Position),
   append([Position], MovesUp, Moves).



possible_moves_orthogonally(GameState, Position, Moves):-
        position_to_coordinates(Position, Row, Col),
        possible_moves_up_first(GameState,Row,Col,MovesUp),
        possible_moves_down_first(GameState,Row,Col,MovesDown),
        possible_moves_left_first(GameState,Row,Col,MovesLeft),
        possible_moves_right_first(GameState,Row,Col,MovesRight),

        append([], MovesUp, Moves1),
        append(Moves1, MovesDown, Moves2),
        append(Moves2, MovesLeft, Moves3),
        append(Moves3, MovesRight, Moves).




possible_moves_up_first(_, Row, _, []) :-
    Row > 9, !.
possible_moves_up_first(GameState, Row, Col, []) :-
    NewRow is Row+1,
    occupied(NewRow, Col, GameState), !.
possible_moves_up_first(GameState, Row, Col, []) :-
    NewRow is Row+1,
    char_at_position(GameState, Row-Col, Piece),
    is_scared(NewRow, Col, Piece, GameState), !.

possible_moves_up_first(GameState, Row, Col, Moves) :-
    NewRow is Row+1,
    char_at_position(GameState, Row-Col, Piece),
    coordinates_to_position(NewRow, Col,  Position),
    NR is NewRow +1,
    possible_moves_up(GameState, NR, Col, Piece, Moves1),
    append([Position], Moves1, Moves).


possible_moves_up(_, Row, _, _, []) :-
   Row > 10, !.

possible_moves_up(GameState, Row, Col, _, []) :-
    Row > 1,
    occupied(Row, Col, GameState), !.

possible_moves_up(GameState, Row, Col, Piece, []) :-
    Row > 1,
    is_scared(Row, Col, Piece,GameState), !.

possible_moves_up(GameState, Row, Col, Piece, Moves) :-
   NewRow is Row + 1,
   possible_moves_up(GameState, NewRow, Col, Piece, MovesUp),
   coordinates_to_position(Row, Col,  Position),
   append([Position], MovesUp, Moves).


possible_moves_down_first(_, Row, _, []) :-
    Row < 2, !.

possible_moves_down_first(GameState, Row, Col, []) :-
    NewRow is Row-1,
    occupied(NewRow, Col, GameState), !.

possible_moves_down_first(GameState, Row, Col, []) :-
    NewRow is Row-1,
    char_at_position(GameState, Row-Col, Piece),
    is_scared(NewRow, Col, Piece, GameState), !.

possible_moves_down_first(GameState, Row, Col, Moves) :-
    NewRow is Row-1,
    char_at_position(GameState, Row-Col, Piece),
    coordinates_to_position(NewRow, Col,  Position),
    NR is NewRow-1,
    possible_moves_down(GameState, NR, Col, Piece, Moves1),
    append([Position], Moves1, Moves).

possible_moves_down(_, Row, _, _, []) :-
    Row < 1, !.

possible_moves_down(GameState, Row, Col, _, []) :-
    Row < 10,
    occupied(Row, Col, GameState), !.

possible_moves_down(GameState, Row, Col, Piece, []) :-
    Row < 10,
    is_scared(Row, Col, Piece, GameState), !.

possible_moves_down(GameState, Row, Col, Piece, Moves) :-
   NewRow is Row - 1,
   possible_moves_down(GameState, NewRow, Col, Piece, MovesUp),
   coordinates_to_position(Row, Col,  Position),
   append([Position], MovesUp, Moves).

possible_moves_left_first(_, _, Col, []) :-
    Col < 2, !.

possible_moves_left_first(GameState, Row, Col, []) :-
    NewCol is Col-1,
    occupied(Row, NewCol, GameState), !.

possible_moves_left_first(GameState, Row, Col, []) :-
    NewCol is Col-1,
    char_at_position(GameState, Row-Col, Piece),
    is_scared(Row, NewCol, Piece, GameState), !.

possible_moves_left_first(GameState, Row, Col, Moves) :-
    NewCol is Col-1,
    char_at_position(GameState, Row-Col, Piece),
    coordinates_to_position(Row, NewCol,  Position),
    NC is NewCol-1,
    possible_moves_left(GameState, Row, NC, Piece, Moves1),
    append([Position], Moves1, Moves).

possible_moves_left(_, _, Col, _, []) :-
    Col < 1, !.

possible_moves_left(GameState, Row, Col, _, []) :-
    Col > 1,
    occupied(Row, Col, GameState), !.

possible_moves_left(GameState, Row, Col, Piece, []) :-
    Col > 1,
    is_scared(Row, Col, Piece, GameState), !.

possible_moves_left(GameState, Row, Col, Piece, Moves) :-
   NewCol is Col - 1,
   possible_moves_left(GameState, Row, NewCol, Piece, MovesUp),
   coordinates_to_position(Row, Col,  Position),
   append([Position], MovesUp, Moves).

possible_moves_right_first(_, _, Col, []) :-
    Col > 9, !.
possible_moves_right_first(GameState, Row, Col, []) :-
    NewCol is Col+1,
    occupied(Row, NewCol, GameState), !.

possible_moves_right_first(GameState, Row, Col, []) :-
    NewCol is Col+1,
    char_at_position(GameState, Row-Col, Piece),
    is_scared(Row, NewCol, Piece, GameState),
    NC is NewCol +1,
    possible_moves_right_first(GameState, Row, NC, _).

possible_moves_right_first(GameState, Row, Col, Moves) :-
    NewCol is Col+1,
    char_at_position(GameState, Row-Col, Piece),
    coordinates_to_position(Row, NewCol,  Position),
    NC is NewCol+1,
    possible_moves_right(GameState, Row, NC, Piece, Moves1),
    append([Position], Moves1, Moves).

possible_moves_right(_, _, Col, _, []) :-
    Col > 10, !.

possible_moves_right(GameState, Row, Col, _, []) :-
    Col > 1,
    occupied(Row, Col, GameState), !.

possible_moves_right(GameState, Row, Col, Piece, []) :-
    Col > 1,
    is_scared(Row, Col, Piece, GameState), !.

possible_moves_right(GameState, Row, Col, Piece, Moves) :-
   NewCol is Col + 1,
   possible_moves_right(GameState, Row, NewCol, Piece,  MovesUp),
   coordinates_to_position(Row, Col,  Position),
   append([Position], MovesUp, Moves).
