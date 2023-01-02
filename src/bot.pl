:-use_module(library(random)).
:-use_module(library(lists)).
:- consult('logic.pl').

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

animal_board_with_moves(GameState, mice, Position, Moves) :-
    possible_moves_orthogonally(GameState, Position, Moves).

animal_board_with_moves(GameState, lion, Position, Moves) :-
    possible_moves_diagonally(GameState, Position, Moves).

animal_board_with_moves(GameState, elephant, Position, Moves) :-
    possible_moves_elephant(GameState, Position, Moves).


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

% Define the greedy_steps/3 predicate
greedy_steps(Board, Player, Steps) :-
    % Get the pieces belonging to the player
    get_player_pieces(Board, Player, Pieces),
    % Get the positions of all the watering holes
    findall(Pos, watering_hole(Pos), WateringHoles),
    % Find the minimum number of steps needed to reach a watering hole
    greedy_steps(Pieces, WateringHoles, Steps).

% Define the greedy_steps/3 predicate (auxiliary predicate)
greedy_steps(_, [], inf).
greedy_steps(Pieces, [H|T], Steps) :-
    greedy_steps(Pieces, T, RestSteps),
    min_steps(Pieces, H, PieceSteps),
    Steps is min(PieceSteps, RestSteps).

% Define the min_steps/3 predicate (auxiliary predicate)
min_steps([], _, inf).
min_steps([H|T], Target, Steps) :-
    % Find the minimum number of steps needed to reach the target position
    % from the current position
    greedy_plays(H, Target, Plays),
    length(Plays, PlaySteps),
    min_steps(T, Target, RestSteps),
    Steps is min(PlaySteps, RestSteps).

% Define the greedy_plays/3 predicate
greedy_plays(Plays, Target, Sequence) :-
    greedy_plays(Plays, Target, [], Sequence).

% Define the greedy_plays/4 predicate (auxiliary predicate)
greedy_plays([], _, Acc, Acc) :- !.
greedy_plays([Play|T], Target, Acc, Sequence) :-
    (   Play = Target
    ->  Sequence = [Play|Acc]
    ;   greedy_plays(T, Target, [Play|Acc], Sequence)
    ).



% Define the choose_random_play/2 predicate
% choose_random_play(+Plays, -Play)
choose_random_play(Plays, Play) :-
    random_member(Play, Plays).



