:- consult('board.pl').
:- consult('logic.pl').

% returns the number of pieces that the Player have on the watering hole.
% player_piece_in_wh(+Board, +Player, -Count)
game_over(Board, Winner) :-
    player_piece_in_wh(Board, Winner, Count),
    Count >= 3.
    

player_piece_in_wh(Board, Player, Count) :-
        get_player_pieces(Board, Player, Pieces),
        count_positions([d4,d7,g4,g7], Pieces, Count),
        (Count > 0 ->
        write('Player '), write(Player), write(' has '), write(Count), write(' pieces in watering holes!'), nl).

count_positions(_, [], 0).
count_positions(WateringHoles, [Pos|T], Count) :-
    member(Pos, WateringHoles),
    count_positions(WateringHoles, T, RestCount),
    Count is 1 + RestCount, !.

count_positions(WateringHoles, [_|T], Count) :-
    count_positions(WateringHoles, T, Count).

% Define the count_position/3 predicate
%count_position(_, _, 0).
%count_position(WateringHoles, Pos, Count) :-
%    member(Pos, WateringHoles),
%    Count is 1.


%count_occurrences(Array, Player, Count) :-
%        findall(X,
%                (member(Row, Array),
%                 member(X, Row),
%                 X = watering_hole(_)),
%                Occurrences),
%        write('Player '), write(Player), write(' Has '), write(Occurrences),nl,
%        length(Occurrences, Count).
%                

