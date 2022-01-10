/*
Function: Given a Game State prints the Board, next player and title.  

display_game(+GameState)
Parameters: 
    1. Board with player and pieces display 
*/
display_game([Player | Board]) :- write('**         Five Field Kono        **'),
                                  nl,
                                  nl,
                                  length(Board,N), 
                                  space_begin_big(N),
                                  display_column_indexes(N, N),
                                  nl,
                                  space_begin_big(N),
                                  display_separator(N),
                                  display_board(Board),
                                  nl,
                                  nl,
                                  display_player(Player).

/*
Function: Given a list and a position indicates the list element in that position. 

display_column_indexes(+N,+L)
Parameters: 
    1. N is the current index of the column being printed
    2. L is the fixed number of columns of the board
*/
display_column_indexes(0, _). 
display_column_indexes(N, L):-  Char is (L-N) + 65, 
                                char_code(C,Char),
                                write('  '),
                                write(C),
                                write(' '),
                                N1 is N - 1,
                                display_column_indexes(N1, L).

/*
Function: Writes the line separator of the board.  

display_separator(+N)
Parameters: 
    1. N is the current index of the column being printed
*/
display_separator(0):- write('+'). 
display_separator(N):- write('+---'),
                       N1 is N - 1,
                       display_separator(N1). 

/*
Function: Displays the actual board situation.  

display_board(+Board)
Parameters: 
    1. Board is the actual game state with the pices in its places. 
*/
display_board([]). 
display_board([Line | Board]):-  nl,
                                length(Board,N),
                                length(Line,L),
                                space_begin_small(L),
                                Char is (L-N) + 96, 
                                char_code(C,Char),
                                write(C),
                                write(' |'), 
                                display_line(Line),
                                nl,          
                                space_begin_big(L),
                                display_separator(L), 
                                display_board(Board).  

/*
Function: Displays a line of the board.  

display_line(+Line)
Parameters: 
    1. Line is the actual board line to be printed with the pices in its places. 
*/
display_line([]).
display_line([Piece | Line]):- write(' '),
                               display_piece(Piece), 
                               write(' |'),
                               display_line(Line).

/*
Function: Displays a line of the board.  

display_piece(+Piece)
Parameters: 
    1. Piece is the piece to be written. If empty prints a space.  
*/
display_piece('B'):- write('B'), !. 
display_piece('W'):- write('W'), !. 
display_piece('E'):- write(' '), !. 

/*
Function: Depending on the size of the board, the space from the the bar to the board depends and it is calculated and printed in this function.  

space_begin_big(+N)
Parameters: 
    1. N is the current index of the space to be printed.    
*/
space_begin_big(N):- write('  '),
                    space_begin_small(N).

/*
Function: Depending on the size of the board, the space from the bar to the board depends and it is calculated and printed in this function.  

space_begin_small(+N)
Parameters: 
    1.  N is the current index of the space to be printed.  
*/
space_begin_small(0).           
space_begin_small(N):-write(' '),
                      N1 is N-1,
                      space_begin_small(N1).

/*
Function: Prints next player to play.  

display_player(+Player)
Parameters: 
    1. Player next to play 
*/
display_player('W'):- write('Next Player: WHITE'), !. 
display_player('B'):- write('Next Player: BLACK'), !. 


/*
Function: Aks for input on what pieace to move.  

ask_piece(-X, -Y)
Parameters: 
    1. Column where the piece is
    2. Row where the piece is 
*/
ask_piece(X, Y):-   nl,      
                    write('PIECE TO MOVE (a-A): '),
                    get_code(_Y),
                    Y is _Y - 97,
                    get_code(_T),
                    get_code(_X),
                    X is _X - 65,
                    write(X/Y), %to remove-----------------------------------------------------
                    peek_code(10), !, 
                    skip_line. 

/*
Function: Aks for input on where to move the pieace.  

ask_move(-X, -Y)
Parameters: 
    1. Column of the next piece position
    2. Row of the next piece position
*/
ask_move(X, Y):-    nl,
                    write('WHERE TO MOVE THE CHOSEN PIECE (a-A): '),
                    get_code(_Y),
                    Y is _Y - 97,
                    get_code(_T),
                    get_code(_X),
                    X is _X - 65,
                    write(X/Y), %to remove-----------------------------------------------------
                    peek_code(10), !, 
                    skip_line. 

%call:- display_game(['B',['W','W','W','W','W'],['W','E','E','E','W'],['E','E','B','E','E'],['B','E','E','E','B'],['E','B','B','B','B']]).


/*
**              Five Field Kono              **  
 
            A   B   C   D   E   F   
          +---+---+---+---+---+---+                                                                  
        a | B | B | B | B | B | B |
          +---+---+---+---+---+---+                               
        b | B |   |   |   |   | B |
          +---+---+---+---+---+---+                               
        c |   |   |   |   |   |   |
          +---+---+---+---+---+---+                               
        d |   |   |   |   |   |   |
          +---+---+---+---+---+---+                               
        e | W |   |   |   |   | W |
          +---+---+---+---+---+---+                               
        f | W | W | W | W | W | W |
          +---+---+---+---+---+---+                               

NEXT PLAYER: WHITE/BLACK

PIECE TO MOVE (a-A): 

WHERE TO MOVE (b-B):
*/

