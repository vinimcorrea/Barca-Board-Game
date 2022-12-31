:-use_module(library(random)).
:-use_module(library(lists)).

% -------------------------------------
%         EASY BOT - RANDOM   
% -------------------------------------

% Dicas do prof
% Para um jogador random podemos ter algo deste genero
% findall(X0-Y0-X-Y, can_move(Board(X0-Y0-X-Y), L).
% Para um random_member
% setof(Val-X0-Y0-X-Y, (move(Board0, X0-Y0, X-Y, Board), evaluate(Board, Val)), L).

% ------------------
%       Code 

% Choose a random play from all possible plays for the given player in the
% given game state.
choose_random_play(Player, GameState, Play) :-
    % Find all possible plays for the given player.
    findall(PossiblePlay, possible_play(Player, GameState, PossiblePlay), PossiblePlays),
    % Choose a random play from the list of possible plays.
    random_member(Play, PossiblePlays).
% Define the choose_random_play/3 predicate
% choose_random_play(+Player, +Board, -Play)
choose_random_play(Player, Board, Play) :-
    findall(P, available_play(Player, Board, P), Plays),
    random_member(Play, Plays).

% Define the piece types and their movement rules
piece_type(mouse).
piece_type(lion).
piece_type(elephant).

movement_rule(mouse, orthogonal).
movement_rule(lion, diagonal).
movement_rule(elephant, both).

% Define the fear relationships between the pieces
afraid_of(mouse, lion).
afraid_of(lion, elephant).
afraid_of(elephant, mouse).

% Define the coordinates of the watering holes
watering_hole(1, 5).
watering_hole(1, 6).
watering_hole(1, 7).
watering_hole(10, 5).
watering_hole(10, 6).
watering_hole(10, 7).

% Define the board size
board_size(10, 10).

% Define the pieces on the board
% piece(PieceType, Owner, Row, Column)
piece(mouse, 1, 2, 5).
piece(mouse, 1, 2, 6).
piece(mouse, 1, 2, 7).
piece(lion, 1, 1, 5).
piece(lion, 1, 1, 6).
piece(lion, 1, 1, 7).
piece(elephant, 1, 1, 6).

piece(mouse, 2, 9, 5).
piece(mouse, 2, 9, 6).
piece(mouse, 2, 9, 7).
piece(lion, 2, 10, 5).
piece(lion, 2, 10, 6).
piece(lion, 2, 10, 7).
piece(elephant, 2, 10, 6).

% Define the available_play/3 predicate
% available_play(+Player, +Board, -Play)
available_play(Player, Board, (Piece, Moves)) :-
  member(Piece, Board),
  Piece = piece(Type, Player, Row, Col),
  findall(move(Row1, Col1), valid_move(Type, Row, Col, Row1, Col1, Board), Moves),
  Moves \= [].

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
occupied(Row, Col, Board) :-
    nth0(Row, Board, RowList),
    nth0(Col, RowList, Piece),
    Piece \= '-'.

% Define the afraid/4 predicate.
% Animals from the same player dont fear each other. Only if the animal is from a diffrent player.
% afraid(+Type, +Row, +Col, +Board)
afraid(Type, Row, Col, Board) :-
    afraid_of(Type, AfraidType),
    (
        (Row1 is Row + 1, Col1 is Col + 1,
         occupied(Row1, Col1, Board),
         piece_type(Piece, AfraidType))
    ;
        (Row1 is Row + 1, Col1 is Col, occupied(Row1, Col1, Board), piece_type(Piece, AfraidType))
    ;
        (Row1 is Row + 1, Col1 is Col - 1, occupied(Row1, Col1, Board), piece_type(Piece, AfraidType))
    ;
        (Row1 is Row, Col1 is Col + 1, occupied(Row1, Col1, Board), piece_type(Piece, AfraidType))
    ;
        (Row1 is Row, Col1 is Col - 1, occupied(Row1, Col1, Board), piece_type(Piece, AfraidType))
    ;
        (Row1 is Row - 1, Col1 is Col + 1, occupied(Row1, Col1, Board), piece_type(Piece, AfraidType))
    ;
        (Row1 is Row - 1, Col1 is Col, occupied(Row1, Col1, Board), piece_type(Piece, AfraidType))
    ;
        (Row1 is Row - 1, Col1 is Col - 1, occupied(Row1, Col1, Board), piece_type(Piece, AfraidType))
    ).

% Define the piece_type/2 predicate
% piece_type(?Piece, ?Type)
piece_type(piece(Type, _, _, _), Type).

% Define the choose_random_play/2 predicate
% choose_random_play(+Plays, -Play)
choose_random_play(Plays, Play) :-
    random_member(Play, Plays).

/*
the findall/3 predicate is used to find all the valid plays for a mouse 
located at position (2,5) on the board, and the choose_random_play/2 
predicate is used to choose a random play from the resulting list of plays.
*/
% Example usage:
% ?- findall(Play, valid_move(mouse, 2, 5, Row1, Col1, Board), Plays),
%    choose_random_play(Plays, Play).
% Plays = [move(2,5,3,5), move(2,5,2,6)],
% Play = move(2,5,2,6).





% -------------------------------------
%      HARD BOT - DIFFICULT LEVEL  
% -------------------------------------

% DICA: ter 1 variavel para peças em perigo
% (Npieces - N'pieces)

% Podemos ter uma função que tem criterios * peso de cada jogada. 
% Assim conseguimos saber qual a melhor jogada
% Se houver 2 de igual valor, escolhe-se uma random

