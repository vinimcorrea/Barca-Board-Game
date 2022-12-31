:- consult('logic.pl').
:- consult('utils.pl').

% valid_move(+Board, +Animal, +X, +Y, +X1, +Y1)
valid_move(Board, lion, X, Y, X1, Y1) :-
        coordinates_to_position(X,Y, Position),
        coordinates_to_position(X1,Y1, Position1),
        possible_moves_diagonally(Board, Position, Moves),
        member(Position1, Moves).
        
read_move(Board, Player, Row, Col, NewRow, NewCol) :-
        write('Select your piece in the format (d8):  '), nl,       
        read(Input),
        position_to_coordinates(Input, Row, Col),
        select_piece(Board, Player, Row-Col),
        char_at_position(Board, Row-Col, Element),
        \+ afraid(Row, Col, Board),
        player(Player, Color),
        piece(Element, Animal, Color),
        print_board_with_moves(Board, Animal, Input),
        write('Your possible moves with this piece is marked with a X.'), nl, nl,
        write('Choose your movement with the piece in the format (d8):  '), nl,
        write('0. Back'), nl,   
        read(Input2),  
        position_to_coordinates(Input2, NewRow, NewCol).

turn(Player, Board, UpdatedBoard):-
        write('Player:  '), write(Player), nl,
        read_move(Board, Player, X, Y, ToX, ToY),
        write('valid read'),nl,
        select_piece(Board, Piece, X-Y), %get Piece
        char_at_position(Board, X-Y, Element),
        player(Player, Color),
        piece(Element, Animal, Color),
        char_at_position(Board,ToX-ToY, Piece),
        (\+valid_move(Board, Animal, X, Y, ToX, ToY) -> write('Invalid Move!, try again'), nl, turn(Player, Board,UpdatedBoard);
         write('valid move'),nl,
        update_board(X-Y, ToX-ToY, Board, UpdatedBoard),
        write('valid New Board'),nl).

game_loop(Player,Board) :-
        turn(Player,Board,NewBoard),
        NextPlayer is (Player mod  2) + 1,
        %(game_over(GameState,Winner) -> break; true),
        display_game(NewBoard),
        game_loop(NextPlayer,Board).