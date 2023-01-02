:-use_module(library(random)).
:-use_module(library(lists)).
:- consult('logic.pl').

% return a list with the possible moves
% animal_board_with_moves(+GameState, +Animal, +Position, -Moves)
animal_board_with_moves(GameState, mice, Position, Moves) :-
    possible_moves_orthogonally(GameState, Position, Moves).

animal_board_with_moves(GameState, lion, Position, Moves) :-
    possible_moves_diagonally(GameState, Position, Moves).

animal_board_with_moves(GameState, elephant, Position, Moves) :-
    possible_moves_elephant(GameState, Position, Moves).

% -------------------------------------
%         EASY BOT - RANDOM   
% -------------------------------------

% easybot return an UpdatedBoard with the moves selected
% easybot(+Player, +GameState, -UpdatedBoard)
easybot(Player,GameState, UpdatedBoard) :-
        get_player_pieces(GameState, Player, Pieces),
        length(Pieces, Choices),
        RealChoices is Choices-1,
        random(0, RealChoices, Choice),
        nth0(Choice, Pieces, Position),
        position_to_coordinates(Position, Row, Col),
        char_at_position(GameState, Row-Col, Piece),
        piece(Piece, Animal, _),
        % If the element is a piece, get all the possible moves for that piece
        animal_board_with_moves(GameState, Animal, Position, Moves), !,
        % Choose a random move from the list of possible moves
        length(Moves, NumMoves),
        random(0, NumMoves, MoveIndex),
        nth0(MoveIndex, Moves, NewPosition),
        position_to_coordinates(NewPosition, X, Y),
        update_board(Row-Col, X-Y, GameState, UpdatedBoard).
        % Update the board with the chosen move.





