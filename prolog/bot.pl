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

%last line
get_empty_final('B', [Line | []], Empty):-  
%penultimate line
get_empty_final('B', [Line | Board], Empty):- length(Board, 1),
                                              pos_element(Line, 0, Piece), 
                                              
                                              fail. 

get_empty_final('W', [Line | Board], Empty):-  length(Line, Size), 
                                               Ind is Size-1, 
                                               length(Board, Ind), 
                                               fail. 
get_empty_final('W', [Line | Board], Empty):-  length(Line, Size), 
                                               Ind is Size-2, 
                                               length(Board, Ind). 

get_empty_final(Player, [Line | Board], Empty):- get_empty_final(Player, Board, Empty). 

get_empty_place(Player, [Piece | Line], X,Y, [X-Y| Empty]):- Player =/= Piece, 
                                                             X1 is X-1, 
                                                             get_empty_place(Player, Line, X1,Y, Empty). 

get_pieces_()


bot(X):- valid_moves(['B',['B','B','E','B','B'],['B','E','E','E','B'],['E','E','B','E','E'],['W','E','E','E','W'],['W','W','W','W','W']],X). 

