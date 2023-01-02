:- use_module(library(lists)).
:- consult('utils.pl').
:- consult('board.pl').

% Given the Board, return element in the position (X, Y)
% char_at_position(+GameState, +X-+Y, -Char)
char_at_position(GameState, X-Y, Char) :-
    nth1(X, GameState, Row),
    nth1(Y, Row, Char).


% Define the occupied/3 predicate
% occupied(+Row, +Col, +Board)
occupied(X, Y, GameState) :-
    nth1(X, GameState, Row),
    nth1(Y, Row, Char),
    is_occupied(Char).

% is_occupied(-Char)
is_occupied(Char) :-
    Char \= '-' , Char \= 'O'.


% Check if Animal is trapped at that postion
% is_trapped(+GameState, +Animal, +Position)
is_trapped(GameState, mice, Position) :-
    possible_moves_orthogonally(GameState, Position, []).

is_trapped(GameState, lion, Position) :-
    possible_moves_diagonally(GameState, Position, []).

is_trapped(GameState, elephant, Position) :-
    possible_moves_elephant(GameState, Position, []).
    

% Animals from the same player dont fear each other. Only if the animal is from a different player.
% return if a animal is scared from an adjacent animal.
% is_scared(+Piece, +Row, +Y, +Board)
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

% return the possible moves from Position if is an elephant
% possible_moves_elephant(+GameState, +Position, -Moves)
possible_moves_elephant(GameState, Position, Moves) :-
    possible_moves_diagonally(GameState, Position, MovesD),
    possible_moves_orthogonally(GameState, Position, MovesO),

    append([], MovesD, Moves1),
    append(Moves1, MovesO, Moves).
    
% return the possible moves from Position if animal moves diagonally
% possible_moves_diagonally(+GameState, +Position, -Moves)
possible_moves_diagonally(GameState, Position, Moves):-
        position_to_coordinates(Position, Row, Col),
        char_at_position(GameState, Row-Col, Piece),
        possible_moves_up_right_first(GameState,Row,Col, Piece, MovesUpRight),
        possible_moves_up_left_first(GameState,Row,Col, Piece, MovesUpLeft),
        possible_moves_down_right_first(GameState,Row,Col, Piece, MovesDownRight),
        possible_moves_down_left_first(GameState,Row,Col, Piece, MovesDownLeft),

        append([], MovesUpRight, Moves1),
        append(Moves1, MovesUpLeft, Moves2),
        append(Moves2, MovesDownRight, Moves3),
        append(Moves3, MovesDownLeft, Moves).

% return the possible moves from piece in position (Row, Col) moving up right
% possible_moves_up_right_first(+GameState, +Row, +Col, +Piece,  -Moves)
possible_moves_up_right_first(_, Row, Col,_, []) :-
    (Row > 9 ; Col > 9), !.

possible_moves_up_right_first(GameState, Row, Col, _, []) :-
    NewRow is Row+1,
    NewCol is Col+1,
    occupied(NewRow, NewCol, GameState), !.

possible_moves_up_right_first(GameState, Row, Col, Piece, Moves) :-
    NewRow is Row+1,
    NewCol is Col+1,
    is_scared(NewRow, NewCol, Piece, GameState),
    NR is NewRow+1,
    NC is NewCol+1,
    possible_moves_up_right(GameState, NR, NC, Piece, Moves).
    
possible_moves_up_right_first(GameState, Row, Col, Piece, Moves) :-
    NewRow is Row+1,
    NewCol is Col+1,
    coordinates_to_position(NewRow, NewCol,  Position),
    NR is NewRow +1,
    NC is NewCol +1,
    possible_moves_up_right(GameState, NR, NC, Piece, Moves1),
    append([Position], Moves1, Moves).

% because our piece is always occupied on the position, this is a aux predicate after check next move up right
% possible_moves_up_right(+GameState, +Row, +Col, +Piece, -Moves)
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

% return the possible moves from piece in position (Row, Col) moving up left
% possible_moves_up_left_first(+GameState, +Row, +Col, +Piece,  -Moves)
possible_moves_up_left_first(_, Row, Col, _, []) :-
    (Row > 9 ; Col < 2), !.

possible_moves_up_left_first(GameState, Row, Col, _, []) :-
    NewRow is Row+1,
    NewCol is Col-1,
    occupied(NewRow, NewCol, GameState), !.

possible_moves_up_left_first(GameState, Row, Col, Piece, Moves) :-
    NewRow is Row+1,
    NewCol is Col-1,
    is_scared(NewRow, NewCol, Piece, GameState),
    NR is NewRow+1,
    NC is NewCol-1,
    possible_moves_up_left(GameState, NR, NC, Piece, Moves).

possible_moves_up_left_first(GameState, Row, Col, Piece, Moves) :-
    NewRow is Row+1,
    NewCol is Col-1,
    coordinates_to_position(NewRow, NewCol,  Position),
    NR is NewRow +1,
    NC is NewCol -1,
    possible_moves_up_left(GameState, NR, NC, Piece, Moves1),
    append([Position], Moves1, Moves).

% because our piece is always occupied on the position, this is a aux predicate after check next move up left
% possible_moves_up_left(+GameState, +Row, +Col, +Piece, -Moves)
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

% return the possible moves from piece in position (Row, Col) moving down right
% possible_moves_down_right_first(+GameState, +Row, +Col, +Piece,  -Moves)
possible_moves_down_right_first(_, Row, Col, _, []) :-
    (Row < 2 ; Col > 9), !.
possible_moves_down_right_first(GameState, Row, Col, _, []) :-
    NewRow is Row-1,
    NewCol is Col+1,
    occupied(NewRow, NewCol, GameState), !.
possible_moves_down_right_first(GameState, Row, Col, Piece, Moves) :-
    NewRow is Row-1,
    NewCol is Col+1,
    is_scared(NewRow, NewCol, Piece, GameState),
    NR is NewRow-1,
    NC is NewCol+1,
    possible_moves_down_right(GameState, NR, NC, Piece, Moves).

possible_moves_down_right_first(GameState, Row, Col, Piece, Moves) :-
    NewRow is Row-1,
    NewCol is Col+1,
    coordinates_to_position(NewRow, NewCol,  Position),
    NR is NewRow -1,
    NC is NewCol +1,
    possible_moves_down_right(GameState, NR, NC, Piece, Moves1),
    append([Position], Moves1, Moves).

% because our piece is always occupied on the position, this is a aux predicate after check next move down right
% possible_moves_down_right(+GameState, +Row, +Col, +Piece, -Moves)
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

% return the possible moves from piece in position (Row, Col) moving down left
% possible_moves_down_left_first(+GameState, +Row, +Col, +Piece,  -Moves)
possible_moves_down_left_first(_, Row, Col, _, []) :-
    (Row < 2 ; Col < 2), !.
possible_moves_down_left_first(GameState, Row, Col,  _, []) :-
    NewRow is Row-1,
    NewCol is Col-1,
    occupied(NewRow, NewCol, GameState), !.

possible_moves_down_left_first(GameState, Row, Col, Piece, Moves) :-
    NewRow is Row-1,
    NewCol is Col-1,
    is_scared(NewRow, NewCol, Piece, GameState),
    NR is NewRow-1,
    NC is NewCol-1,
    possible_moves_down_left(GameState, NR, NC, Piece, Moves).

possible_moves_down_left_first(GameState, Row, Col, Piece, Moves) :-
    NewRow is Row-1,
    NewCol is Col-1,
    coordinates_to_position(NewRow, NewCol,  Position),
    NR is NewRow -1,
    NC is NewCol -1,
    possible_moves_down_left(GameState, NR, NC, Piece, Moves1),
    append([Position], Moves1, Moves).

% because our piece is always occupied on the position, this is a aux predicate after check next move down left
% possible_moves_down_left(+GameState, +Row, +Col, +Piece, -Moves)
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


% return the possible moves from Position if animal moves orthogonally
% possible_moves_diagonally(+GameState, +Position, -Moves)
possible_moves_orthogonally(GameState, Position, Moves):-
        position_to_coordinates(Position, Row, Col),
        char_at_position(GameState, Row-Col, Piece),
        possible_moves_up_first(GameState,Row,Col, Piece, MovesUp),
        possible_moves_down_first(GameState,Row,Col, Piece, MovesDown),
        possible_moves_left_first(GameState,Row,Col, Piece, MovesLeft),
        possible_moves_right_first(GameState,Row,Col, Piece, MovesRight),

        append([], MovesUp, Moves1),
        append(Moves1, MovesDown, Moves2),
        append(Moves2, MovesLeft, Moves3),
        append(Moves3, MovesRight, Moves).



% return the possible moves from piece in position (Row, Col) moving up
% possible_moves_up_first(+GameState, +Row, +Col, +Piece,  -Moves)
possible_moves_up_first(_, Row, _, _, []) :-
    Row > 9, !.
possible_moves_up_first(GameState, Row, Col, _, []) :-
    NewRow is Row+1,
    occupied(NewRow, Col, GameState), !.
possible_moves_up_first(GameState, Row, Col, Piece, Moves) :-
    NewRow is Row+1,
    is_scared(NewRow, Col, Piece, GameState),
    NR is NewRow +1,
    possible_moves_up(GameState, NR, Col, Piece, Moves).

possible_moves_up_first(GameState, Row, Col, Piece, Moves) :-
    NewRow is Row+1,
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

% return the possible moves from piece in position (Row, Col) moving down
% possible_moves_down_first(+GameState, +Row, +Col, +Piece,  -Moves)
possible_moves_down_first(_, Row, _, _, []) :-
    Row < 2, !.

possible_moves_down_first(GameState, Row, Col, _, []) :-
    NewRow is Row-1,
    occupied(NewRow, Col, GameState), !.

possible_moves_down_first(GameState, Row, Col, Piece, Moves) :-
    NewRow is Row-1,
    is_scared(NewRow, Col, Piece, GameState),
    NR is NewRow - 1,
    possible_moves_down(GameState, NR, Col, Piece, Moves).

possible_moves_down_first(GameState, Row, Col, Piece, Moves) :-
    NewRow is Row-1,
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

% return the possible moves from piece in position (Row, Col) moving left
% possible_moves_left_first(+GameState, +Row, +Col, +Piece,  -Moves)
possible_moves_left_first(_, _, Col, _, []) :-
    Col < 2, !.

possible_moves_left_first(GameState, Row, Col, _, []) :-
    NewCol is Col-1,
    occupied(Row, NewCol, GameState), !.

possible_moves_left_first(GameState, Row, Col, Piece, Moves) :-
    NewCol is Col-1,
    is_scared(Row, NewCol, Piece, GameState),
    NC is NewCol-1,
    possible_moves_left(GameState, Row, NC, Piece, Moves).

possible_moves_left_first(GameState, Row, Col, Piece, Moves) :-
    NewCol is Col-1,
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

% return the possible moves from piece in position (Row, Col) moving right
% possible_moves_right_first(+GameState, +Row, +Col, +Piece,  -Moves)
possible_moves_right_first(_, _, Col, _, []) :-
    Col > 9, !.
possible_moves_right_first(GameState, Row, Col, _, []) :-
    NewCol is Col+1,
    occupied(Row, NewCol, GameState), !.

possible_moves_right_first(GameState, Row, Col, Piece, Moves) :-
    NewCol is Col+1,
    is_scared(Row, NewCol, Piece, GameState),
    NC is NewCol +1,
    possible_moves_right(GameState, Row, NC, Piece, Moves).

possible_moves_right_first(GameState, Row, Col, Piece, Moves) :-
    NewCol is Col+1,
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
