/*
Function: Given a list and a position indicates the list element in that position. 
Parameters: 
    1. List
    2. Position (Index)
    3. Element in the correspondent index of the list
*/
posElement([H | _], 0, H).
posElement([_ | T], P, E):- P > 0,
                            P1 is P - 1,
                            posElement(T, P1 ,E).

/*
Function: Replaces a the element in the certain list position with a new one. 
Parameters: 
    1. Element to insert in the index position of the list
    2. List
    3. Position (Index)
    4. List with the Element updated
*/
replaceElement(E, [_ | T], 0 , [E | T]).
replaceElement(E, [H | T], P , [H | NL]):- P > 0,
                                           P1 is P - 1,
                                           replaceElement(E, T, P1, NL).  

/*
Function: Verifies equality of elem with elements of array
Parameters: 
    1. Element to be equal
    2. List of elements to compare
*/
equalElement(_,[]). 
equalElement(E, [E | T]) :- equalElement(E,T). 

/*
Function: Validates if the next player to play corresponds to the one making the move. 
Parameters: 
    1. Board with nextplayer to play and pieces display
    2. List with the Piece's actual position
*/
validatePlayer([Player | Board],[ PieceX , PieceY | _ ]):- posElement(Board, PieceX, Row), 
                                                         posElement(Row, PieceY, Piece),
                                                         Player == Piece.

/*
Function: Validates if the move make the pieces go to a valid position in the board, valid meaning empty and in the diagonal of the actual position. 
Parameters: 
    1. Board with actual pieces display
    2. List with the suposed next Piece position
*/
validateShift(Board, [NextX, NextY | _ ]):- posElement(Board, NextX, Row), 
                                            posElement(Row, NextY, Piece),
                                            Piece == 'E'.
/*
Function: Validates if the move is valid, recurring to validatePlayer and validateShift. 
Parameters: 
    1. Board with nextplayer to play and pieces display
    2. List with actual Piece position and shift move
*/
validateMove([Player | Board], [ [PieceX , PieceY | _ ] , [DeltaX, DeltaY | _ ] | _ ]):- DeltaX == -1; DeltaX == 1,
                                                                                         DeltaY == -1; DeltaY == 1,
                                                                                        validatePlayer([Player | Board], [PieceX , PieceY]), 
                                                                                        NextX is PieceX + DeltaX,
                                                                                        NextY is PieceY + DeltaY,
                                                                                        validateShift(Board, [NextX , NextY]). 

/*
Function: Modifies the board in order to make the given move. 
Parameters:
    1. Board with player and pieces display
    2. List with actual Piece position and shift move
    3. Board after move
*/
makeMove([Player | Board], [ [PieceX , PieceY | _ ] , [DeltaX, DeltaY | _ ] | _ ], NewBoard) :- NextX is PieceX + DeltaX,
                                                                                                NextY is PieceY + DeltaY,
                                                                                                %set chosen house to player turn piece
                                                                                                posElement(Board, NextX, Row), 
                                                                                                replaceElement(Player, Row, NextY, NewRow),
                                                                                                replaceElement(NewRow, Board, NextX, NewBoard1),
                                                                                                %set origin house to empty
                                                                                                posElement(NewBoard1, PieceX, Row2), 
                                                                                                replaceElement('E', Row2, PieceY, NewRow2),
                                                                                                replaceElement(NewRow2, NewBoard1, PieceX,NewBoard). 

/*
Function: Modifies the game state in order to complete a player move.
Parameters:
    1. Board with player and pieces display
    2. List with actual Piece position and shift move
    3. Game state after move
*/
move(['W'| Board], Move, ['B'|NB ]):- validateMove(['W' | Board], Move),
                                      makeMove(['W' | Board], Move, NB). 

move(['B'| Board], Move, ['W'|NB]):- validateMove(['B' | Board], Move),
                                     makeMove(['B' | Board], Move, NB).

func(M,G):- move(['B',['B','B','B','B','B'],['B','E','E','E','B'],['E','E','E','E','E'],['W','E','E','E','W'],['W','W','W','W','W']],M,G). 

/*
Function: If the player that has just moved has all of its pieces in the enemy initial pieces's position
Parameters:
    1. Board with next player (that does not have the chance to play) and final pieces display
    2. Winner being the player that has just played.
*/
game_over(['W' | Board], 'B'):- length(Board,BoardN),
                                LastRowN is BoardN - 1,
                                posElement(Board, LastRowN, LastRow), 
                                equalElement('B', LastRow),
                                PenultimateRowN is BoardN - 2,
                                posElement(Board, PenultimateRowN, PenultimateRow),
                                posElement(PenultimateRow, 0, FirstElement), 
                                FirstElement == 'B',
                                length(PenultimateRow,N),
                                LastPos is N-1,
                                posElement(PenultimateRow, LastPos, LastElement), 
                                LastElement == 'B'. 


game_over(['B' | Board], 'W'):- posElement(Board, 0, FirstRow), 
                                equalElement('W', FirstRow), 
                                posElement(Board, 1, SecondRow),
                                posElement(SecondRow, 0, FirstElement), 
                                FirstElement == 'W',
                                length(SecondRow,N),
                                LastPos is N-1,
                                posElement(SecondRow, LastPos, LastElement), 
                                LastElement == 'W'. 


overW(W):- game_over(['B',['W','W','W','W','W'],['W','E','E','E','W'],['E','E','B','E','E'],['B','E','E','E','B'],['E','B','B','B','B']],W). 

overB(W):- game_over(['W',['W','W','E','W','W'],['W','E','W','E','W'],['E','E','E','E','E'],['B','E','E','E','B'],['B','B','B','B','B']],W). 
