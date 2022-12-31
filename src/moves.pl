:- consult('utils.pl').


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

possible_moves_up_right_first(GameState, Row, Col, Moves) :-
    NewRow is Row+1,
    NewCol is Col+1,
    \+ occupied(NewRow, NewCol, GameState),
    coordinates_to_position(NewRow, NewCol,  Position),
    NR is NewRow +1,
    NC is NewCol +1,
    possible_moves_up_right(GameState, NR, NC, Moves1),
    append([Position], Moves1, Moves).


possible_moves_up_right(_, Row, Col, []) :-
   (Row > 10; Col > 10), !.

possible_moves_up_right(GameState, Row, Col, []) :-
    Row > 1,
    Col > 1,
    occupied(Row, Col, GameState), !.

possible_moves_up_right(GameState, Row, Col, Moves) :-
   NewRow is Row + 1,
   NewCol is Col +1,
   possible_moves_up_right(GameState, NewRow, NewCol, MovesUp),
   coordinates_to_position(Row, Col,  Position),
   append([Position], MovesUp, Moves).


possible_moves_up_left_first(_, Row, Col, []) :-
    (Row > 9 ; Col > 2), !.

possible_moves_up_left_first(GameState, Row, Col, []) :-
    NewRow is Row+1,
    NewCol is Col-1,
    occupied(NewRow, NewCol, GameState), !.

possible_moves_up_left_first(GameState, Row, Col, Moves) :-
    NewRow is Row+1,
    NewCol is Col-1,
    \+ occupied(NewRow, NewCol, GameState),
    coordinates_to_position(NewRow, NewCol,  Position),
    NR is NewRow +1,
    NC is NewCol -1,
    possible_moves_up_left(GameState, NR, NC, Moves1),
    append([Position], Moves1, Moves).


possible_moves_up_left(_, Row, Col, []) :-
   (Row > 10; Col < 1), !.

possible_moves_up_left(GameState, Row, Col, []) :-
    Row > 1,
    Col > 1,
    occupied(Row, Col, GameState), !.

possible_moves_up_left(GameState, Row, Col, Moves) :-
   NewRow is Row + 1,
   NewCol is Col -1,
   possible_moves_up_left(GameState, NewRow, NewCol, MovesUp),
   coordinates_to_position(Row, Col,  Position),
   append([Position], MovesUp, Moves).


possible_moves_down_right_first(_, Row, Col, []) :-
    (Row < 2 ; Col > 9), !.
possible_moves_down_right_first(GameState, Row, Col, []) :-
    NewRow is Row-1,
    NewCol is Col+1,
    occupied(NewRow, NewCol, GameState), !.

possible_moves_down_right_first(GameState, Row, Col, Moves) :-
    NewRow is Row-1,
    NewCol is Col+1,
    \+ occupied(NewRow, NewCol, GameState),
    coordinates_to_position(NewRow, NewCol,  Position),
    NR is NewRow -1,
    NC is NewCol +1,
    possible_moves_down_right(GameState, NR, NC, Moves1),
    append([Position], Moves1, Moves).


possible_moves_down_right(_, Row, Col, []) :-
   (Row < 1; Col > 10), !.

possible_moves_down_right(GameState, Row, Col, []) :-
    Row < 10,
    Col > 1,
    occupied(Row, Col, GameState), !.

possible_moves_down_right(GameState, Row, Col, Moves) :-
   NewRow is Row - 1,
   NewCol is Col +1,
   possible_moves_down_right(GameState, NewRow, NewCol, MovesUp),
   coordinates_to_position(Row, Col,  Position),
   append([Position], MovesUp, Moves).

possible_moves_down_left_first(_, Row, Col, []) :-
    (Row < 2 ; Col < 2), !.
possible_moves_down_left_first(GameState, Row, Col, []) :-
    NewRow is Row-1,
    NewCol is Col-1,
    occupied(NewRow, NewCol, GameState), !.

possible_moves_down_left_first(GameState, Row, Col, Moves) :-
    NewRow is Row-1,
    NewCol is Col-1,
    \+ occupied(NewRow, NewCol, GameState),
    coordinates_to_position(NewRow, NewCol,  Position),
    NR is NewRow -1,
    NC is NewCol -1,
    possible_moves_down_left(GameState, NR, NC, Moves1),
    append([Position], Moves1, Moves).


possible_moves_down_left(_, Row, Col, []) :-
   (Row < 1; Col < 1), !.

possible_moves_down_left(GameState, Row, Col, []) :-
    Row < 10,
    Col < 10,
    occupied(Row, Col, GameState), !.

possible_moves_down_left(GameState, Row, Col, Moves) :-
   NewRow is Row - 1,
   NewCol is Col -1,
   possible_moves_down_left(GameState, NewRow, NewCol, MovesUp),
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


%possible_moves_up(GameState, BlockingRow, BlockingCol, []) :-     
%   !, occupied(BlockingRow, BlockingCol, GameState).


possible_moves_up_first(_, Row, _, []) :-
    Row > 9, !.
possible_moves_up_first(GameState, Row, Col, []) :-
    NewRow is Row+1,
    occupied(NewRow, Col, GameState), !.

possible_moves_up_first(GameState, Row, Col, Moves) :-
    NewRow is Row+1,
    \+ occupied(NewRow, Col, GameState),
    coordinates_to_position(NewRow, Col,  Position),
    NR is NewRow +1,
    possible_moves_up(GameState, NR, Col, Moves1),
    append([Position], Moves1, Moves).


possible_moves_up(_, Row, _, []) :-
   Row > 10, !.

possible_moves_up(GameState, Row, Col, []) :-
    Row > 1,
    occupied(Row, Col, GameState), !.

possible_moves_up(GameState, Row, Col, Moves) :-
   NewRow is Row + 1,
   possible_moves_up(GameState, NewRow, Col, MovesUp),
   coordinates_to_position(Row, Col,  Position),
   append([Position], MovesUp, Moves).


possible_moves_down_first(_, Row, _, []) :-
    Row < 2, !.
possible_moves_down_first(GameState, Row, Col, []) :-
    NewRow is Row-1,
    occupied(NewRow, Col, GameState), !.

possible_moves_down_first(GameState, Row, Col, Moves) :-
    NewRow is Row-1,
    \+ occupied(NewRow, Col, GameState),
    coordinates_to_position(NewRow, Col,  Position),
    NR is NewRow-1,
    possible_moves_down(GameState, NR, Col, Moves1),
    append([Position], Moves1, Moves).

possible_moves_down(_, Row, _, []) :-
    Row < 1, !.

possible_moves_down(GameState, Row, Col, []) :-
    Row < 10,
    occupied(Row, Col, GameState), !.

possible_moves_down(GameState, Row, Col, Moves) :-
   NewRow is Row - 1,
   possible_moves_down(GameState, NewRow, Col, MovesUp),
   coordinates_to_position(Row, Col,  Position),
   append([Position], MovesUp, Moves).

possible_moves_left_first(_, _, Col, []) :-
    Col < 2, !.
possible_moves_left_first(GameState, Row, Col, []) :-
    NewCol is Col-1,
    occupied(Row, NewCol, GameState), !.

possible_moves_left_first(GameState, Row, Col, Moves) :-
    NewCol is Col-1,
    \+ occupied(Row, NewCol, GameState),
    coordinates_to_position(Row, NewCol,  Position),
    NC is NewCol-1,
    possible_moves_left(GameState, Row, NC, Moves1),
    append([Position], Moves1, Moves).

possible_moves_left(_, _, Col, []) :-
    Col < 1, !.

possible_moves_left(GameState, Row, Col, []) :-
    Col > 1,
    occupied(Row, Col, GameState), !.

possible_moves_left(GameState, Row, Col, Moves) :-
   NewCol is Col - 1,
   possible_moves_left(GameState, Row, NewCol, MovesUp),
   coordinates_to_position(Row, Col,  Position),
   append([Position], MovesUp, Moves).

possible_moves_right_first(_, _, Col, []) :-
    Col > 9, !.
possible_moves_right_first(GameState, Row, Col, []) :-
    NewCol is Col+1,
    occupied(Row, NewCol, GameState), !.

possible_moves_right_first(GameState, Row, Col, Moves) :-
    NewCol is Col+1,
    \+ occupied(Row, NewCol, GameState),
    coordinates_to_position(Row, NewCol,  Position),
    NC is NewCol+1,
    possible_moves_right(GameState, Row, NC, Moves1),
    append([Position], Moves1, Moves).

possible_moves_right(_, _, Col, []) :-
    Col > 10, !.

possible_moves_right(GameState, Row, Col, []) :-
    Col > 1,
    occupied(Row, Col, GameState), !.

possible_moves_right(GameState, Row, Col, Moves) :-
   NewCol is Col + 1,
   possible_moves_right(GameState, Row, NewCol, MovesUp),
   coordinates_to_position(Row, Col,  Position),
   append([Position], MovesUp, Moves).
