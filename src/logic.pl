:- use_module(library(lists)).
:- consult('board.pl').
:- consult('utils.pl').


% piece(?Character, ?Animal, ?Color). 
piece(e, elephant, white).
piece(m, mice, white).
piece(l, lion, white).
piece('E', elephant, black).
piece('M', mice, black).
piece('L', lion, black).

player(1, white).
player(2, black).

movement_rule(mice, orthogonal).
movement_rule(lion, diagonal).
movement_rule(elephant, both).

% The piece is scared of another piece if it is of the opposite type.
afraid_of(mice, lion).
afraid_of(lion, elephant).
afraid_of(elephant, mouse).

% The piece is scared of another piece if it is of the opposite type.
%scared_of(mouse, lion).
%scared_of(lion, elephant).
%scared_of(elephant, mouse).

char_at_position(GameState, X-Y, Char) :-
    nth1(X, GameState, Row),
    nth1(Y, Row, Char).

% The piece is represented as a tuple (AnimalType, ColorType, X, Y) where AnimalType is the type of the piece (mouse, lion, elephant),
% and Colortype is the type of the color of the piece (white, black)
% X and Y are the coordinates of the piece on the board.
piece1(AnimalType, ColorType, X, Y) :-
    member(AnimalType, [mouse, lion, elephant]),
    member(ColorType, [white, black]),
    between(1, 10, X),
    between(1, 10, Y).

% select_piece(+GameState, +Player, +X-+Y)
% print if piece is from player or not
select_piece(GameState, Player, X-Y) :-
    char_at_position(GameState, X-Y, Piece),
    is_piece_from_player(Player, Piece),
    player(Player, Color),
    piece(Piece, Animal, Color),
    write('You chose: '), write(Color), write(' '), write(Animal), nl.


select_piece(GameState, _, X-Y) :-
    char_at_position(GameState, X-Y, Piece),
    write('You chose: '), write(Piece), nl,
    write('This is not your piece!'), nl.

%possible_movements(GameState, NewGameState, Piece, X-Y) :-

% Define a predicate to update the board based on a move
% update_board(+X-+Y, +NewX-+NewY, +Board, -NewBoard)
update_board(X-Y, NewX-NewY, Board, NewBoard) :-
    char_at_position(Board, X-Y, Piece), % Check if there is a pawn at the starting position
    set_char_at_position(Board, X, Y, -, TempBoard), % Clear the starting position
    set_char_at_position(TempBoard, NewX, NewY, Piece, NewBoard). % Set the new position to a pawn
    


% Define a predicate to set a character at a position on the board
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


is_piece_from_player(Player, Piece) :-
    player(Player, Color),
    piece(Piece, _, Color).


% Define the valid_move/6 predicate
% valid_move(+Type, +Row, +Col, +Row1, +Col1, +Board)
valid_move(Type, Row, Col, Row1, Col1, Board) :-
    movement_rule(Type, Rule),
    (
      Rule = orthogonal,
      (Row = Row1 ; Col = Col1),
      \+ occupied(Row1, Col1, Board),
      \+ afraid(Type, Row1, Col1, Board)
    ;
      Rule = diagonal,
      abs(Row - Row1) =:= abs(Col - Col1),
      \+ occupied(Row1, Col1, Board),
      \+ afraid(Type, Row1, Col1, Board)
    ;
      Rule = both,
      (Row = Row1 ; Col = Col1 ; abs(Row - Row1) =:= abs(Col - Col1)),
      \+ occupied(Row1, Col1, Board),
      \+ afraid(Type, Row1, Col1, Board)
    ).
  
% Define the occupied/3 predicate
% occupied(+Row, +Col, +Board)
occupied(X, Y, GameState) :-
    nth1(X, GameState, Row),
    nth1(Y, Row, Char),
    is_occupied(Char).

is_occupied(Char) :-
    Char \= '-' , Char \= 'O'.


% Define the afraid/4 predicate.
% Animals from the same player dont fear each other. Only if the animal is from a diffrent player.
% afraid(+Piece, +Row, +Y, +Board)
afraid(Row, Y, Board) :-
    char_at_position(Board, Row-Y, P),
    piece(P, Type, Color),
    letter_is(Y, Col),
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


possible_movement_up_right(Position, Board) :-
    position_to_coordinates(Position, X, Y),
    occupied(X, Y, Board), !.

%possible_movement_up_right(Position,Board) :-
%    \+ afraid(X, Y, Board).


show_possible_moves(A, [], B) :-
    A = B.
show_possible_moves(GameState, [Position|T], D) :-
    position_to_coordinates(Position, X, Y),
    set_char_at_position(GameState, X, Y, 'X', NewGameState),
    show_possible_moves(NewGameState, T, D).


print_board_with_moves(GameState, Position) :-
    lion_moves(Position, GameState, Moves),
    show_possible_moves(GameState, Moves, NewGameState),
    display_game(NewGameState).



%find_diagonal_moves(X, Y, _, Moves) :-
%    % Base case: Position is not within bounds or is occupied
%    (X < 1; X > 8; Y < 1; Y > 8),
%    !,
%    Moves = [].
%find_diagonal_moves(X, Y, GameState, Moves) :-
%    occupied(X,Y,GameState),
%    !,
%    Moves = [].
%
%find_diagonal_moves(X, Y, GameState, Moves) :-
%    % Check up-left
%    X1 is X - 1,
%    Y1 is Y - 1,
%    find_diagonal_moves(X1, Y1, GameState, Moves1),
%    % Check up-right
%    Y2 is Y + 1,
%    find_diagonal_moves(X1, Y2, GameState, Moves2),
%    % Check down-left
%    X2 is X + 1,
%    find_diagonal_moves(X2, Y1, GameState, Moves3),
%    % Check down-right
%    find_diagonal_moves(X2, Y2, GameState, Moves4),
%    % Concatenate all lists of moves
%    coordinates_to_position(X, Y, Move),
%    append([Move|Moves1], [Move|Moves2], Temp1),
%    append(Temp1, [Move|Moves3], Temp2),
%    append(Temp2, [Move|Moves4], Moves).

% rook(+Board, +Row, +Col, -Moves)
%
%    Moves is a list of all possible moves that a rook at position (Row, Col)
%    can make on the given Board.
% rook(+Board, +Row, +Col, -Moves)
%
%    Moves is a list of all possible moves that a rook at position (Row, Col)
%    can make on the given Board.
%rook(Board, Row, Col, Moves) :-
%%    findall((NewRow, Col), (between(1, 8, NewRow), NewRow \= Row, \+ occupied(NewRow, Col, Board), (NewRow < Row; NewRow > Row)), Rows),
%    findall((Row, NewCol), (between(1, 7, NewCol), NewCol \= Col, (NewCol < Col; NewCol > Col)), Cols),
%    append([], Cols, AllMoves),
%    member((BlockingRow, BlockingCol), AllMoves),
%    occupied(BlockingRow, BlockingCol, Board), !,
%    findall((MoveRow, MoveCol), (
%                                    member((MoveRow, MoveCol), AllMoves),
%                                    (MoveRow \= BlockingRow;
%                                    MoveCol \= BlockingCol)),
%            Moves).


possible_moves_orthogonally(GameState,Row,Col,Moves):-
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




