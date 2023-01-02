:- consult('board.pl').
:- consult('logic.pl').

% check if Player have more than or equal to 3 pieces in the watering holes
% game_over(+Board, +Winner)
game_over(Board, Winner) :-
    player_piece_in_wh(Board, Winner, Count),
    Count >= 3.
    
% returns the number of pieces that the Player have on the watering hole.
% player_piece_in_wh(+Board, +Player, -Count)
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


