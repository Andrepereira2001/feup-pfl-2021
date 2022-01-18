:- ensure_loaded(display).
:- ensure_loaded(bot).

/*
Function: Given a list and a position indicates the list element in that position. 

pos_element(+List, +Pos, -Elem).
Parameters: 
    1. List
    2. Position (Index)
    3. Element in the correspondent index of the list
*/
pos_element([H | _], 0, H).
pos_element([_ | T], P, E):- P > 0,
                            P1 is P - 1,
                            pos_element(T, P1 ,E).

/*
Function: Replaces a the element in the certain list position with a new one. 

replace_element(+Elem, +List, +Pos, -NewList).
Parameters: 
    1. Element to insert in the index position of the list
    2. List
    3. Position (Index)
    4. List with the Element updated
*/
replace_element(E, [_ | T], 0 , [E | T]).
replace_element(E, [H | T], P , [H | NL]):- P > 0,
                                           P1 is P - 1,
                                           replace_element(E, T, P1, NL).  

/*
Function: Verifies equality of elem with elements of array

equalElement(+Elem, +List).
Parameters: 
    1. Element to be equal
    2. List of elements to compare
*/
equalElement(_,[]). 
equalElement(E, [E | T]) :- equalElement(E,T). 

/*
Function: Validates if the next player to play corresponds to the one making the move. 

validate_player(+GameState, +Pos).
Parameters: 
    1. Board with nextplayer to play and pieces display
    2. List with the Piece's actual position
*/
validate_player([Player | Board],[ PieceX , PieceY | _ ]):- pos_element(Board, PieceY, Row), 
                                                         pos_element(Row, PieceX, Piece),
                                                         Player == Piece.

/*
Function: Validates if the move make the pieces go to a valid position in the board, valid meaning empty and in the diagonal of the actual position. 

validate_shift(+Board, +Pos).
Parameters: 
    1. Board with actual pieces display
    2. List with the suposed next Piece position
*/
validate_shift(Board, [NextX, NextY | _ ]):- pos_element(Board, NextY, Row), 
                                            pos_element(Row, NextX, Piece),
                                            Piece == 'E'.

/*
Function: Possible moves in x and y axis. 

delta(+X, +Y).
Parameters: 
    1. X coordinate. 
    2. Y coordinate. 
*/
delta(1,-1). 
delta(1,1).
delta(-1,1).
delta(-1,-1).

/*
Function: Validates if the move is valid, recurring to validate_player and validate_shift. 

validate_move(+GameState, +Move).
Parameters: 
    1. 
    2. List with actual Piece position and shift move
*/
validate_move([Player | Board], [ [PieceX , PieceY | [] ] , [DeltaX, DeltaY | [] ] | [] ]):- delta(DeltaX,DeltaY), 
                                                                                            validate_player([Player | Board], [PieceX , PieceY]), 
                                                                                            NextX is PieceX + DeltaX,
                                                                                            NextY is PieceY + DeltaY,
                                                                                            validate_shift(Board, [NextX , NextY]). 

/*
Function: Modifies the board in order to make the given move. 

make_move(+GameState, +Move, -NewGameState).
Parameters:
    1. Board with player and pieces display
    2. List with actual Piece position and shift move
    3. Board after move
*/
make_move([Player | Board], [ [PieceX , PieceY | [] ] , [DeltaX, DeltaY | [] ] | [] ], NewBoard) :- NextX is PieceX + DeltaX,
                                                                                                NextY is PieceY + DeltaY,
                                                                                                %set chosen house to player turn piece
                                                                                                pos_element(Board, NextY, Row), 
                                                                                                replace_element(Player, Row, NextX, NewRow),
                                                                                                replace_element(NewRow, Board, NextY, NewBoard1),
                                                                                                %set origin house to empty
                                                                                                pos_element(NewBoard1, PieceY, Row2), 
                                                                                                replace_element('E', Row2, PieceX, NewRow2),
                                                                                                replace_element(NewRow2, NewBoard1, PieceY,NewBoard). 

/*
Function: Modifies the game state in order to complete a player move.

move(+GameState, +Move, -NewGameState)
Parameters:
    1. Board with player and pieces display
    2. List with actual Piece position and shift move
    3. Game state after move
*/
move(['W'| Board], Move, ['B'|NB ]):- validate_move(['W' | Board], Move),
                                      make_move(['W' | Board], Move, NB). 

move(['B'| Board], Move, ['W'|NB]):- validate_move(['B' | Board], Move),
                                     make_move(['B' | Board], Move, NB).


/*
Function: If the player that has just moved has all of its pieces in the enemy initial pieces's position

game_over(+GameState, -Winner)
Parameters:
    1. Board with next player (that does not have the chance to play) and final pieces display
    2. Winner being the player that has just played.
*/
game_over(['W' | Board], 'B'):- length(Board,BoardN),
                                LastRowN is BoardN - 1,
                                pos_element(Board, LastRowN, LastRow), 
                                equalElement('B', LastRow),
                                PenultimateRowN is BoardN - 2,
                                pos_element(Board, PenultimateRowN, PenultimateRow),
                                pos_element(PenultimateRow, 0, FirstElement), 
                                FirstElement == 'B',
                                length(PenultimateRow,N),
                                LastPos is N-1,
                                pos_element(PenultimateRow, LastPos, LastElement), 
                                LastElement == 'B'. 


game_over(['B' | Board], 'W'):- pos_element(Board, 0, FirstRow), 
                                equalElement('W', FirstRow), 
                                pos_element(Board, 1, SecondRow),
                                pos_element(SecondRow, 0, FirstElement), 
                                FirstElement == 'W',
                                length(SecondRow,N),
                                LastPos is N-1,
                                pos_element(SecondRow, LastPos, LastElement), 
                                LastElement == 'W'. 

/*
Function: Builds and initiates a Board with the dimensions given. 

initial_state(+Size, -GameState)
Parameters:
    1. Number of columns or lines for the board to have
    2. Board with next player (that does not have the chance to play) and final pieces display
*/
initial_state(Size, Game):- length(List, Size), 
                            initial_line(List, Size),
                            fill_board(List, 0, Size, Board), 
                            append(['W'], Board, Game).

/*
Function: Initiates a Line with the size given. 

initial_line(+Size, -GameState)
Parameters:
    1. List with length of the Size
    2. Size of the row of the board
*/                            
initial_line([], _):- !. 
initial_line([Line | List ], Size):- length(Line,Size), 
                                     initial_line(List, Size). 

/*
Function: Fills the board with its initial state. 

fill_board(+Board, +Index, +Size , -NewBoard)
Parameters:
    1. Empty Board
    2. Current Index of the line to be filled
    3. Number of rows of the board
    4. Board to be completed
*/
fill_board([Line | _ ], N , Size , [NewLine | []]):-  N =:= Size - 1,
                                                        fill_line(Line, Size, N , NewLine).
fill_board([Line | Board], N , Size , [NewLine | NewBoard]):- fill_line(Line, Size, N , NewLine), 
                                                              N1 is N+1, 
                                                              fill_board(Board, N1, Size , NewBoard).
/*
Function: Fills the line according with the line to be filled.  

fill_line(+Line, +Size, +Index , -NewLine)
Parameters:
    1. Empty Board
    2. Number of columns of the line 
    3. Current Index of the element to be filled
    4. Board to be completed
*/
fill_line([], _, _, []).

/*First line*/
fill_line([_ | Line], Size, 0, ['B' | LineNumber] ):- fill_line(Line, Size, 0, LineNumber).

/*Second line - First Element*/
fill_line([_ | Line], Size, 1, ['B' | LineNumber] ):-  length(Line, L), L =:= Size-1,
                                                        fill_line(Line, Size, 1, LineNumber).
/*Second line - Last Element*/
fill_line([_ | Line], Size, 1, ['B' | LineNumber] ):-  length(Line, L), L =:= 0,
                                                        fill_line(Line, Size, 1, LineNumber).
/*Penultimate line - First Element*/
fill_line([_ | Line], Size, N, ['W' | LineNumber] ):-   N =:= Size - 2,
                                                        length(Line, L), L =:= Size-1,
                                                        fill_line(Line, Size, N, LineNumber).
/*Penultimate line - Last Element*/
fill_line([_ | Line], Size, N, ['W' | LineNumber] ):-   N =:= Size - 2,
                                                        length(Line, L), L =:= 0,
                                                        fill_line(Line, Size, N, LineNumber).
/*Last line*/                                    
fill_line([_ | Line], Size, N, ['W' | LineNumber] ):-   N =:= Size - 1,
                                                        fill_line(Line, Size, N, LineNumber).
/*Nth line*/
fill_line([_ | Line], Size, N, ['E' | LineNumber] ):-fill_line(Line, Size, N, LineNumber).

/*
Function: Fills the line according with the line to be filled.  

choose_move(+GameState, +Player, -Move)
Parameters:
    1. Board with actual state of the game and information about next player.
    2. Type of Player to play (human or pc)
    3. Move to receive
*/
choose_move([Player | Board], human, [[PieceX,PieceY],[DeltaX,DeltaY]]):- ask_piece(PieceX, PieceY), 
                                                                          validate_player([Player | Board], [PieceX, PieceY]),
                                                                          ask_move(MoveX, MoveY), 
                                                                          DeltaX is MoveX - PieceX,
                                                                          DeltaY is MoveY - PieceY,
                                                                          validate_move([Player | Board],[[PieceX,PieceY],[DeltaX,DeltaY]]). 

choose_move(GameState, computer-Level, Move):- valid_moves(GameState, Moves),
                                               choose_move(Level, GameState, Moves, Move).
                        

choose_move(Board,Player,Move):-write('Invalid input'),
                                nl,
                                choose_move(Board,Player,Move).


/*
Function: Function to start the game.  

play_game(+WhiteP, +BlackP, +Size)
Parameters:
    1. Current black player
    2. Current board size
    3. Board size
*/
play_game(WhiteP, BlackP, Size):-
            initial_state(Size,GameState),
            display_game(GameState), !, 
            game_cycle(GameState,WhiteP,BlackP).

/*
Function: Show valid moves if answer was 'y'.  

show_moves(+Answer, +GameState)
Parameters:
    1. Answer of player
    2. Current game state
*/
show_moves('y', GameState):- valid_moves(GameState, Moves),
                             show_moves(Moves).
show_moves(_,_).

/*
Function: Game loop function.  

game_cycle(+GameState,+WhiteP,+BlackP)
Parameters:
    1. Board with actual state of the game and information about next player.
    2. Current black player
    3. Current board size
*/
game_cycle(GameState,_,_):-
            game_over(GameState, Winner), !,
            congratulate(Winner).
            
game_cycle(['B' | Board],WhiteP,BlackP):-
            ask_move_show(Show,BlackP),
            show_moves(Show,['B' | Board]),
            choose_move(['B' | Board], BlackP, Move),
            move(['B' | Board], Move, NewGameState),
            display_game(NewGameState), !,
            game_cycle(NewGameState,WhiteP,BlackP).

game_cycle(['W' | Board],WhiteP,BlackP):-
            ask_move_show(Show,WhiteP),
            show_moves(Show,['W' | Board]),
            choose_move(['W' | Board], WhiteP, Move),
            move(['W' | Board], Move, NewGameState),
            display_game(NewGameState), !,
            game_cycle(NewGameState,WhiteP,BlackP).

/*
Function: Loops in the menu.  

menu_loop(+WhiteP, +BlackP, +Size)
Parameters: 
    1. Current white player
    2. Current black player
    3. Current board size
*/
menu_loop(WhiteP,BlackP,Size):- display_menu(WhiteP,BlackP,Size),
                                input_opt(Opt),
                                make_opt(WhiteP,BlackP,Size,Opt).

menu_loop(WhiteP,BlackP,Size):- menu_loop(WhiteP,BlackP,Size), !.
 
/*
Function: Given an option number from the menu makes the action.  

make_opt(+WhiteP, +BlackP, +Size, +Option)
Parameters: 
    1. Current white player
    2. Current black player
    3. Current board size
    4. Chosen option
*/
make_opt(WhiteP,BlackP,Size,1):- play_game(WhiteP,BlackP,Size), !,
                                 menu_loop(WhiteP,BlackP,Size).

make_opt(_,BlackP,Size,2):-  input_player(WhiteP),
                             menu_loop(WhiteP,BlackP,Size).

make_opt(WhiteP,_,Size,3):-  input_player(BlackP),
                             menu_loop(WhiteP,BlackP,Size).

make_opt(WhiteP,BlackP,_,4):- input_board_size(Size),
                              menu_loop(WhiteP,BlackP,Size), !.

make_opt(_,_,_,5).


/*
Function: First function to be called

play()
*/
play:- menu_loop(human,human,5).
        


func(X,Y):- validate_move(['B',['B','B','B','B','B'],['B','E','E','E','B'],['E','E','E','E','E'],['W','E','E','E','W'],['W','W','W','W','W']],[[2,0],[X,Y]]). 

func2(M,G):- validate_player(['W',['B','B','B','B','B'],['B','E','E','E','B'],['E','E','E','E','E'],['W','E','E','E','W'],['W','W','W','W','W']],M). 

overW(W):- game_over(['B',['W','W','W','W','W'],['W','E','E','E','W'],['E','E','B','E','E'],['B','E','E','E','B'],['E','B','B','B','B']],W). 

overB(W):- game_over(['W',['W','W','E','W','W'],['W','E','W','E','W'],['E','E','E','E','E'],['B','E','E','E','B'],['B','B','B','B','B']],W). 

choseMe(Moves):-findall(Move, move(['B',['B','B','B','B','B'],['B','E','E','E','B'],['E','E','E','E','E'],['W','E','E','E','W'],['W','W','W','W','W']], Move, NewState), Moves).