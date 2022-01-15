:- use_module(library(random)).
:- use_module(library(lists)).

/*
Function: Depending on the difficulty level the pc chooses one move from the available moves for the palyer it is representing. 

choose_move(+Level, +GameState, +Moves, -Move)
Parameters: 
    1. Level 1 or 2 indicating the difficulty of the game
    2. Current board and player to play
    3. List of Moves from where to choose
    4. Move Chosen
*/
choose_move(1, _GameState, Moves, Move):- random_select(Move, Moves, _Rest).

choose_move(2, GameState, Moves, Move):-  setof(Value-Mv, NewState^( member(Mv, Moves),
                                          move(GameState, Mv, NewState),
                                          evaluate_board(NewState, Value) ), [_V-Move|_]).

/*
Function: Depending on the next player to play pieces, the pc has set of valid moves on the current board situation. 

valid_moves(+GameState, -Moves)
Parameters: 
    1. Current board and player to play
    2. List of Valid Moves depending on the player to play
*/
valid_moves([Player | Board], Moves):- player_pieces(Player, Board, List), 
                                       go_through_moves([Player | Board], List, Moves). 

/*
Function: To a current game state, goes through each piece of the player whose playing coming up only with the valid moves. 

go_through_moves(+GameState, +Pieces, -ValidMoves)
Parameters: 
    1. Current board and player to play
    2. List of Moves from where to choose
    3. List of Valid Moves from Moves
*/
go_through_moves(_,[],_).
go_through_moves(GameState,[PiecePos | Pieces],ValidMoves):- findall([PiecePos, [X,Y]], validate_move(GameState, [PiecePos, [X, Y]]), NewList),    
                                                        go_through_moves(GameState, Pieces, OldList),
                                                        append(NewList, OldList, ValidMoves).

/*
Function: Gives the positions in board where the player have pieces.  

player_pieces(+Player, +Board, -Positions)
Parameters: 
    1. Player piece to find
    2. Board with pieces
    3. Positions where the player have pieces
*/
player_pieces(_,[],_).
player_pieces(Player, [Line | Board], List):- length(Line,L),
                                              length(Board,B),
                                              Y is L- B - 1,
                                              find_in_line(Player, Line, 0, Y, OldList),
                                              player_pieces(Player,Board,NewList),
                                              append(OldList,NewList,List).



/*
Function: Gives the positions where the player have pieces in a given board line.  

find_in_line(+Player, +BoardLine, +PieceX, +PieceY, -Positions)
Parameters: 
    1. Player piece to find
    2. Line of board with pieces
    3. Current piece x position
    4. Current piece y position
    5. Positions where the player have pieces
*/
find_in_line(_,[],_,_,_).
find_in_line(Player, [Player | Line], X, Y, [[X,Y] | List]):- X1 is X+1, !,
                                                              find_in_line(Player, Line, X1,Y,List).
find_in_line(Player, [_ | Line], X, Y, List):- X1 is X+1, !, 
                                               find_in_line(Player, Line, X1,Y,List).


/*
Function: Evalutes a given GameState.  

evaluate_board(+GameState, -Value)
Parameters: 
    1. Board with player and pieces display 
    2. Value given to the game state
*/
/*
vejo quais os empty dos que são o meu objetivo final e quais as minhas peças que ainda não estão no objetivo final. 
*/
%evaluate_board(GameState, Value).


/*
Function: Fills a list with the coordinates from the final places where the player's final pieces should be in order to finnish the game. 

get_empty_final(+Player, +Board, -List)
Parameters: 
    1. Player whose playing
    2. Actual Board 
    3. List of Coordinates not filled by the Player's pieces
*/
%last line (Y = Size -1)
get_empty_final('B', [Line | []], Empty):-  length(Line, L),
                                            Y is L-1, 
                                            get_empty_place('B', Line, 0, Y, Empty).

%penultimate line (Y = Size-2)
get_empty_final('B', [Line | Board], Empty):- length(Board, 1), 
                                              length(Line, L), 
                                              Y is L-2, 
                                              add_empty_first_last('B',Line, 0, Y, FirstEmpty),
                                              get_empty_final('B', Board, EmptyLastLine), 
                                              append(FirstEmpty,EmptyLastLine, Empty). 

%first line (Y = 0)
get_empty_final('W', [Line | Board], Empty):-  length(Line, Size), 
                                               Ind is Size-1, 
                                               length(Board, Ind), 
                                               get_empty_place('W', Line, 0,0, FirstEmpty),
                                               get_empty_final('W', Board,EmptyLastLine),
                                               append(FirstEmpty,EmptyLastLine, Empty). 
%second line (Y = 1)                                   
get_empty_final('W', [Line | Board], Empty):-  length(Line, Size), 
                                               Ind is Size-2, 
                                               length(Board, Ind), 
                                               add_empty_first_last('W',Line, 0, 1, Empty).
                                               

get_empty_final(Player, [Line | Board], Empty):- get_empty_final(Player, Board, Empty). 


/*
Function: In a specific board line get the coordinates of the players spots to be filled.  

get_empty_place(+Player, +BoardLine, +X,+Y, -List)
Parameters: 
    1. Player whose playing
    2. Actual Board Line to be inspected
    3. Current board X
    4. Current board Y
    5. List of Coordinates not filled by the Player's pieces
*/
get_empty_place(_,[],_,_,_). 
get_empty_place(Player, [Player | Line], X,Y, Empty):- X1 is X+1, 
                                                       get_empty_place(Player, Line, X1,Y, Empty). 
get_empty_place(Player, [Piece | Line], X,Y, [X-Y| Empty]):- X1 is X+1, 
                                                             get_empty_place(Player, Line, X1,Y, Empty). 

/*
Function: Verify if the first and last possitions of the board can receive a Player piece

add_empty_first_last(+Player, +Board, +X,+Y, -Empty)
Parameters: 
    1. Player whose playing
    2. Actual Board
    3. Current board X
    4. Current board Y
    5. List of Coordinates not filled by the Player's pieces
*/
add_empty_first_last(_,[],_,_,_).
add_empty_first_last(Player,[Piece | Line], 0, Y,  [0-Y |Empty]):- Player \= Piece,
                                                                   add_empty_first_last(Player, Line, 1,Y, Empty).

add_empty_first_last(Player,[Piece | []], X, Y, [X-Y | Empty]):- Player \= Piece.  

add_empty_first_last(Player, [Piece | Line], X,Y, Empty):-  X1 is X+1,
                                                            add_empty_first_last(Player, Line, X1,Y, Empty).



bot(X):- valid_moves(['B',['B','B','E','B','B'],['B','E','E','E','B'],['E','E','B','E','E'],['W','E','E','E','W'],['W','W','W','W','W']],X). 

empty(Empty) :- get_empty_final('W',[['B','B','E','W','B'],['B','E','E','E','W'],['E','E','B','E','E'],['W','E','E','E','W'],['W','W','W','W','W']], Empty).

