:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(system)).

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

choose_move(2, GameState, Moves, Move):-  setof(Value-Mv, (Moves,GameState,NewState)^( member(Mv, Moves),
                                                                                        move(GameState, Mv, NewState),
                                                                                        evaluate_board(NewState, Value)),
                                                List),
                                                randomize_best_moves(List, Move).

/*Function: Receives the list of valid moves and returns a random move with the same value

get_same_values(+List, +Value, -Result)
Parameters: 
    1. List with all the possibel moves and respective board values
    3. Random move that have the best possibel value
*/
randomize_best_moves([V-M |List], Move):- get_same_values([V-M |List], V, Moves),
                                             random_select(Move, Moves, _Rest).
                                             
/*Function: Receives the list of valid moves and returns the moves that have the same value

get_same_values(+List, +Value, -Result)
Parameters: 
    1. List with all the possibel moves and respective board values
    2. Smallest board value
    3. Moves with the same values
*/
get_same_values([], _, []).
get_same_values([Value-Move|List], Value, [Move | Moves]):- get_same_values(List, Value, Moves).
get_same_values(_, _, []).

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
go_through_moves(_,[],[]).
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
Function: Evaluates a given GameState.  

evaluate_board(+GameState, -Value)
Parameters: 
    1. Board with player and pieces display 
    2. Value given to the game state
*/
/*
vejo quais os empty dos que são o meu objetivo final e quais as minhas peças que ainda não estão no objetivo final. 
*/
evaluate_board(['W' | Board], Value):- get_empty_final('B', Board, Empty),  
                                         get_filled_board('B',Board, Filled), !, 
                                         get_value(Empty,Filled,Value), !.

evaluate_board(['B' | Board], Value):-   get_empty_final('W', Board, Empty), 
                                         get_filled_board('W',Board, Filled),!,
                                         get_value(Empty,Filled,Value), !.


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
                                               

get_empty_final(Player, [_ | Board], Empty):- get_empty_final(Player, Board, Empty). 


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
get_empty_place(_,[],_,_,[]). 
get_empty_place(Player, [Player | Line], X,Y, Empty):- X1 is X+1, 
                                                       get_empty_place(Player, Line, X1,Y, Empty). 
get_empty_place(Player, [_ | Line], X,Y, [X-Y| Empty]):- X1 is X+1, 
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
add_empty_first_last(_,[],_,_,[]).
add_empty_first_last(Player,[Piece | Line], 0, Y,  [0-Y |Empty]):- Player \= Piece,
                                                                   add_empty_first_last(Player, Line, 1,Y, Empty).

add_empty_first_last(Player,[Piece | []], X, Y, [X-Y | _]):- Player \= Piece.  

add_empty_first_last(Player, [_ | Line], X,Y, Empty):-  X1 is X+1,
                                                            add_empty_first_last(Player, Line, X1,Y, Empty).


/*
Function: In a specific board get the coordinates of the players pieces.  
get_filled_board(+Player, +Board, -Fill)

Parameters:
    1. Player whose playing
    2. Actual Board to be inspected
    3. List of Coordinates where the Player's pieces are
*/
get_filled_board(_, [],[]).
get_filled_board(Player, [Line | Board], Fill):- length(Line, L),
                                                 length(Board, B),
                                                 Y is L-B-1,
                                                 get_filled_places(Player, Line,0,Y, L, FirstFill), 
                                                 get_filled_board(Player, Board, LastFill), 
                                                 append(FirstFill, LastFill, Fill). 



/*
Function: In a specific board line get the coordinates of the players pieces.  

get_filled_places(+Player, +BoardLine, +X,+Y, +Size, -List)
Parameters: 
    1. Player whose playing
    2. Actual Board Line to be inspected
    3. Current board X
    4. Current board Y
    5. Size of board
    6. List of Coordinates where the Player's pieces are
*/
get_filled_places(_, [], _, _, _, []).
%first line, pieces already in final spot (do not add to list)
get_filled_places('W', _, _, 0,_,[]). 
%second line, pieces already in final spots in the end and the beginning of the line (do not add to list)
get_filled_places('W', [_ | Line],0,1, Size,Fill):- get_filled_places('W', Line, 1,1,Size, Fill). 
get_filled_places('W', [_ | []],X,1, Size,Fill):- X1 is X+1, 
                                                  get_filled_places('W', [], X1,1,Size, Fill). 

%last line, pieces already in final spot
get_filled_places('B', _, _, Y, Size,[]):- Y =:= Size-1. 
%penultimate line, pices already in final spot
get_filled_places('B', [_ | Line],0,Y,Size,Fill):-  Y =:= Size-2,
                                                    get_filled_places('B', Line, 1,Y,Size, Fill). 
get_filled_places('B', [_ | []],X,Y,Size,Fill):- Y =:= Size-2, 
                                                 X1 is X+1,
                                                 get_filled_places('B', [], X1,1,Size, Fill). 

                                                                           
get_filled_places(Player,[Player | Line], X, Y, Size,[X-Y | Fill]):- X1 is X+1, 
                                                                     get_filled_places(Player, Line, X1,Y, Size,Fill). 
                                                      
get_filled_places(Player,[_ | Line], X, Y, Size,Fill):- X1 is X+1, 
                                                        get_filled_places(Player, Line, X1,Y, Size,Fill). 


/*
Function: Calculates the value of the board, by calculating the distances between the pieces that are not yet in a final spot and the spots available. 

get_value(+Empty, +Filled, -Value)
Parameters:
    1. Coordinates of the empty places
    2. Coordinates of the filled places
    3. Value of the board
*/
get_value([],Filled,0).
get_value([X-Y | Empty],Filled,Value):- small_distance(X,Y,Filled,X1-Y1/Dist),
                                        remove_elem(Filled,X1-Y1,Rest), 
                                        get_value(Empty,Rest,V),
                                        Value is Dist + V + 100,!.


remove_elem([],_,[]). 
remove_elem([Elem | List], Elem, List). 
remove_elem([Value |List], Elem,[Value | Ret]):- remove_elem(List, Elem, Ret). 

my_min(X1-Y1/D1, X2-Y2/D2, X1-Y1/D1):- D1 < D2, !. 
my_min(X1-Y1/D1, X2-Y2/D2, X2-Y2/D2). 
                 
/*
Function: Calculates the smallest distance of the point to one of the valid houses. 

get_value(+X, +Y, +Filled, -Dist)
Parameters:
    1. X of the place that is being analised
    2. Y of the place that is being analised
    3. Coordinates of the filled places
    4. Smallest distance to one of the houses
*/

small_distance(_,_,[],_-_/10000).
small_distance(X,Y,[X1-Y1 | Filled], Dist):- (((X mod 2) + (Y mod 2)) mod 2) =:= (((X1 mod 2) + (Y1 mod 2)) mod 2), 
                                             Distance is (X1-X)^2 + (Y1-Y)^2,
                                             small_distance(X,Y,Filled, X2-Y2/SavedDist),!,
                                             my_min(X1-Y1/Distance,X2-Y2/SavedDist,Dist).                                               
                                             
small_distance(X,Y,[_ | Filled],Dist):-small_distance(X,Y,Filled,Dist).
                                             


bot(X):- valid_moves(['B',['B','B','E','B','B'],['B','E','E','E','B'],['E','E','B','E','E'],['W','E','E','E','W'],['W','W','W','W','W']],X). 

empty(Empty) :- get_empty_final('W',[['B','B','E','W','B'],['B','E','E','E','W'],['E','E','B','E','E'],['W','E','E','E','W'],['W','W','W','W','W']], Empty).

pieces(F) :- get_filled_board('B',[['B','B','E','W','W'],['B','E','E','E','W'],['E','E','B','E','E'],['B','E','E','E','W'],['B','W','E','W','W']], F).

evalu(V) :- evaluate_board(['B',['B','W','E','W','W'],['B','E','E','E','W'],['E','E','B','W','E'],['B','E','E','E','E'],['B','E','E','W','W']], V).

smDist(Dist):- small_distance(0,0,[2-3, 3-4, 2-2, 4-4], Dist). 