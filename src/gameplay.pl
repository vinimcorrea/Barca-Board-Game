:- consult('logic.pl').
:- consult('utils.pl').
:- consult('win.pl').

filter_input(Input) :-
    (   Input = 0
    ;   
        atom_chars(Input, A),
        A = [Char, Number],
        char_type(Number, digit),
        char_type(Char, lower)
    ).

char_type(Char, lower) :-
    member(Char, ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']).

char_type(Char, digit) :-
    member(Char, ['1','2','3','4','5','6','7','8','9','10']).


% valid_move(+Board, +Animal, +X, +Y, +X1, +Y1)
valid_move(Board, lion, X, Y, X1, Y1) :-
        coordinates_to_position(X,Y, Position),
        coordinates_to_position(X1,Y1, Position1),
        possible_moves_diagonally(Board, Position, Moves),
        member(Position1, Moves).

valid_move(Board, mice, X, Y, X1, Y1) :-
        coordinates_to_position(X,Y, Position),
        coordinates_to_position(X1,Y1, Position1),
        possible_moves_orthogonally(Board, Position, Moves),
        member(Position1, Moves).
        
valid_move(Board, elephant, X, Y, X1, Y1) :-
        coordinates_to_position(X,Y, Position),
        coordinates_to_position(X1,Y1, Position1),
        possible_moves_elephant(Board, Position, Moves),
        member(Position1, Moves).


read_filter_input(Board, Player, Input) :-
   write('Select your piece in the format (d8):  '), nl,
   read(Input),
   position_to_coordinates(Input, X, Y),
   select_piece(Board, Player, X-Y), !.
   
read_filter_input(Board, Player, _) :-
   write('Invalid Input! try on this format (a2): '), nl, nl,
   read_filter_input(Board, Player, _).

check_piece(Board, Player, X-Y) :-
    select_piece(Board, Player, X-Y), !.

check_piece(Board, Player, _-_) :-
    write('Invalid piece or not yours! Try Again!'), nl,
    read_piece(Board, Player, _, _, _).

%check_afraid(Board, _, X-Y) :-
%    \+ afraid(X, Y, Board), !.

%check_afraid(Board, Player, _-_) :-
%    write('Your piece is afraid! Try Again!'), nl,
%    read_piece(Board, Player, _, _, _).
    

%check_move(Board, Animal, X, Y, ToX, ToY) :-
%    \+valid_move(Board, Animal, X, Y, ToX, ToY), !.

    
read_move(Board, Animal, Row, Col, X1, Y1) :-
        write('Choose your movement with the piece in the format (d8):  '), nl,
        read(Input),
        position_to_coordinates(Input, X1, Y1),
        valid_move(Board, Animal, Row, Col, X1, Y1), !.

                                        
read_move(Board, Animal, Row, Col, X1, Y1) :-
    write('Not a valid move! Try again!'), nl,
    read_move(Board, Animal, Row, Col, X1, Y1).


read_piece(Board, Player, Row, Col, Animal) :-
        read_filter_input(Board, Player, Input),
        position_to_coordinates(Input, Row, Col),
%        check_piece(Board, Player, Row-Col),
        char_at_position(Board, Row-Col, Element),
%        check_afraid(Board, Player, Row-Col),
        player(Player, Color),
        piece(Element, Animal, Color),
        coordinates_to_position(Row, Col, Position),
        print_board_with_moves(Board, Animal, Position), !,
        write('Your possible moves with this piece is marked with a X.'), nl, nl.
%        write('Choose your movement with the piece in the format (d8):  '), nl.

turn(Player, Board, UpdatedBoard):-
        write('Player:  '), write(Player), nl,
        get_player_pieces(Board, Player, Pieces),
        \+ is_any_piece_scared(Board, Pieces), !,
        read_piece(Board, Player, X, Y, Animal),
        read_move(Board, Animal, X, Y, ToX, ToY),
        update_board(X-Y, ToX-ToY, Board, UpdatedBoard),
        nl.

turn(Player, Board, UpdatedBoard):-
        get_player_pieces(Board, Player, Pieces),
        get_piece_scared(Board, Pieces, Position),
        position_to_coordinates(Position, X, Y),
        char_at_position(Board, X-Y, Piece),
        piece(Piece, Animal, _),
        print_board_with_moves(Board, Animal, Position), !,
        write('Oh no! Your piece is scared, move it to unscared it!'), nl,
        read_move(Board, Animal, X, Y, ToX, ToY),
        update_board(X-Y, ToX-ToY, Board, UpdatedBoard),
        nl.

game_loop(Player,Board) :-
        turn(Player,Board,NewBoard),
        NextPlayer is (Player mod  2) + 1,
        \+ game_over(NewBoard, Player),
        display_game(NewBoard),
        game_loop(NextPlayer,NewBoard).

game_loop(Player, Board) :-
    display_game(Board),
    write('Player '), write(Player), write(' has won!'), nl,
    write('End of game. Thanks for playing!').