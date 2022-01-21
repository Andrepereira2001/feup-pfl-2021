/*
Function: Reads a number from the input  

display_game(-FinalNumber,+StartNumber)
Parameters: 
    1. Number read from the input
    2. Base number
*/
read_number(N,N):- peek_code(10), !,
                    skip_line.
read_number(X,N):- get_code(_Y),
                   char_code('0',C),
                   V is _Y - C,
                   N1 is N*10,
                   N2 is N1 + V,
                   read_number(X,N2).

/*
Function: Given a Game State prints the Board, next player and title.  

display_game(+GameState)
Parameters: 
    1. Board with player and pieces display 
*/
display_game([Player | Board]) :- nl,nl,nl,nl,
                                  write('**         Five Field Kono        **'),
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
Function: Ask for input on what pieace to move.  

ask_piece(-X, -Y)
Parameters: 
    1. Column where the piece is
    2. Row where the piece is 
*/
ask_piece(X, Y):-   nl,      
                    write('PIECE TO MOVE (a-A): '),
                    get_code(_Y),
                    Y is _Y - 97,
                    get_code(45),
                    get_code(_X),
                    X is _X - 65,
                    peek_code(10),
                    skip_line,!.

ask_piece(_,_):- skip_line,
                 fail.

/*
Function: Ask for input on where to move the pieace.  

ask_move(-X, -Y)
Parameters: 
    1. Column of the next piece position
    2. Row of the next piece position
*/
ask_move(X, Y):-    nl,
                    write('WHERE TO MOVE THE CHOSEN PIECE (a-A): '),
                    get_code(_Y),
                    Y is _Y - 97,
                    get_code(45),
                    get_code(_X),
                    X is _X - 65,
                    peek_code(10),
                    skip_line,!.
                    
ask_move(_,_):- skip_line,
                fail.

/*
Function: Display the menu screen.  

display_menu(+WhiteP, +BlackP, +Size)
Parameters: 
    1. Current white player
    2. Current black player
    3. Current board size
*/
display_menu(White,Black,Size):- nl,nl,nl,nl,nl,nl,
                                  write('  ______ _             ______ _      _     _   _  __'),
                                  nl,
                                  write(' |  ____(_)           |  ____(_)    | |   | | | |/ /'),
                                  nl,
                                  write(' | |__   ___   _____  | |__   _  ___| | __| | | \' / ___  _ __   ___  '),
                                  nl,
                                  write(' |  __| | \\ \\ / / _ \\ |  __| | |/ _ \\ |/ _\' | |  < / _ \\| \'_ \\ / _ \\ '),
                                  nl,
                                  write(' | |    | |\\ V /  __/ | |    | |  __/ | (_| | | . \\ (_) | | | | (_) | '),
                                  nl,
                                  write(' |_|    |_| \\_/ \\___| |_|    |_|\\___|_|\\__,_| |_|\\_\\___/|_| |_|\\___/ '),
                                  nl,nl,nl,nl,nl,
                                  write('White Player: '),
                                  write(White),
                                  write('          Black Player: '),
                                  write(Black),
                                  nl,
                                  write('Board size: '),
                                  write(Size),
                                  nl,nl,
                                  write('MENU'),
                                  nl,
                                  write('1. Start Game'),
                                  nl,
                                  write('2. Change White Player'),
                                  nl,
                                  write('3. Change Black Player'),
                                  nl,
                                  write('4. Change Board Size'),
                                  nl,
                                  write('5. Quit').

/*
Function: Asks the user for an option and validate it (between 1-5).  

input_opt(-Opt)
Parameters: 
    1. Option chosen by the user
*/
input_opt(Opt):-nl,nl,
                write('Option: '),
                get_code(_O),
                Opt is _O - 48,
                Opt >= 1,
                Opt =< 5,
                peek_code(10),
                skip_line.

input_opt(_):- skip_line,
               fail.

                

/*
Function: Asks the user for the board size and validate it.  

input_board_size(-Size)
Parameters: 
    1. Chosen board number of columns/rows
*/
input_board_size(Size):- nl,nl,
                        write('Size of board must be an integer between 4 and 15'),
                        nl,
                        write('Board Size: '),
                        read_number(Size,0),
                        Size >= 4,
                        Size =< 15.
input_board_size(Size):-write('Invalid Input'),
                        input_board_size(Size).

/*
Function: Asks the user for player type and validate it.  

input_player(-Player)
Parameters: 
    1. Chosen player type (between human and computer)
*/
input_player(Player):- nl,nl,
                        write('1. Human'), 
                        nl,
                        write('2. Computer'),
                        nl,
                        write('Change Player to: '),
                        get_code(_O),
                        Opt is _O - 48,
                        Opt >= 1,
                        Opt =< 2,
                        peek_code(10),
                        skip_line,
                        set_player(Opt,Player).
                        
input_player(Player):-skip_line,
                      write('Invalid Input'),
                      input_player(Player).
                      

/*
Function: Sets the player to the chosen (human or computer) and if it is a computer asks for the difficulty level of the game mode (1 or 2).  

set_player(+Opt,-Player)
Parameters: 
    1. Option chosen
    2. Chosen player type (human or computer and level)
*/
set_player(1,human).
set_player(2,computer-Level):- nl,nl,
                                write('1. Difficulty Level 1'), 
                                nl,
                                write('2. Difficulty Level 2'),
                                nl,
                                write('Change Level to: '),
                                get_code(_O),
                                Opt is _O - 48,
                                Opt >= 1,
                                Opt =< 2,
                                peek_code(10),
                                skip_line,
                                Level is Opt.
                        
set_player(2, computer-Level):-skip_line,
                            write('Invalid Input'),
                            set_player(2, computer-Level).

/*
Function: Writes whose the winner.  

congratulate(+Winner)
Parameters: 
    1. 'B' - Black or 'W' - White
*/
congratulate(Winner):- nl,
                       write('                     _   _   _           __          ___                         _         '),
                       nl, 
                       write('     /\\             | | | | | |          \\ \\        / (_)                       (_) '),
                       nl,
                       write('    /  \\   _ __   __| | | |_| |__   ___   \\ \\  /\\  / / _ _ __  _ __   ___ _ __   _ ___  (_)'),
                       nl,
                       write('   / /\\ \\ | \'_ \\ / _` | | __| \'_ \\ / _ \\   \\ \\/  \\/ / | | \'_ \\| \'_ \\ / _ \\ \'__| | / __|'),
                       nl,
                       write('  / ____ \\| | | | (_| | | |_| | | |  __/    \\  /\\  /  | | | | | | | |  __/ |    | \\__ \\ '),
                       nl,
                       write(' /_/    \\_\\_| |_|\\__,_|  \\__|_| |_|\\___|     \\/  \\/   |_|_| |_|_| |_|\\___|_|    |_|___/ (_)'),
                       nl,nl,
                       fail. 

congratulate('W'):-    nl,
                       write('         __          ___     _ _         _____  _                         _ '),
                       nl, 
                       write('         \\ \\        / / |   (_) |       |  __ \\| |                       | |'),
                       nl,
                       write('          \\ \\  /\\  / /| |__  _| |_ ___  | |__) | | __ _ _   _  ___ _ __  | |'),
                       nl,
                       write('           \\ \\/  \\/ / | \'_ \\| | __/ _ \\ |  ___/| |/ _` | | | |/ _ \\ \'__| | |'),
                       nl,
                       write('            \\  /\\  /  | | | | | ||  __/ | |    | | (_| | |_| |  __/ |    |_|'),
                       nl,
                       write('             \\/  \\/   |_| |_|_|\\__\\___| |_|    |_|\\__,_|\\__, |\\___|_|    (_)'),
                       nl,
                       write('                                                         __/ |              '),
                       nl,
                       write('                                                        |___/              '),
                       nl.
                       
congratulate('B'):- nl,
                    write('             ____  _            _      _____  _                         _'),
                    nl,
                    write('            |  _ \\| |          | |    |  __ \\| |                       | |'),
                    nl,
                    write('            | |_) | | __ _  ___| | __ | |__) | | __ _ _   _  ___ _ __  | |'),
                    nl,
                    write('            |  _ <| |/ _` |/ __| |/ / |  ___/| |/ _` | | | |/ _ \\ \'__| | |'),
                    nl,
                    write('            | |_) | | (_| | (__|   <  | |    | | (_| | |_| |  __/ |    |_|'),
                    nl,
                    write('            |____/|_|\\__,_|\\___|_|\\_\\ |_|    |_|\\__,_|\\__, |\\___|_|    (_)'),
                    nl,
                    write('                                                       __/ |              '),
                    nl,
                    write('                                                      |___/     ').

                
/*
Function: Gets input when the player is human if he wants or not to see its valid moves. In case of a computer player it does not show.  

ask_move_show(-Show, +Player)
Parameters: 
    1. Bollean indicating wether it is suposed to show the valid moves. 
    2. Player who is currently playing
*/
ask_move_show('n',computer-_).                          
ask_move_show(Show,_):- nl, nl, 
                        write('Want to see which are your valid moves (y)? '),
                        get_char(Show),
                        peek_code(10),
                        skip_line.
                    
ask_move_show('n',_):- skip_line.

/*
Function: Show valid moves.  

show_moves(+Moves)
Parameters:
    1. List with all available moves
*/
show_moves([]):- nl.
show_moves([[[X,Y],[DX,DY]] | Moves]):- nl,
                                        write('Piece on: '), 
                                        PX is X + 65, 
                                        PY is Y + 97, 
                                        put_code(PY),
                                        write(-),
                                        put_code(PX),
                                        write(' to '),
                                        NX is X + DX + 65, 
                                        NY is Y + DY + 97,  
                                        put_code(NY),
                                        write(-),
                                        put_code(NX),
                                        show_moves(Moves).






/*
**              Five Field Kono              **  
 
            A   B   C   D   E   F   
          +---+---+---+---+---+---+                                                                  
        a | X | B | B | B | B | B |          0-0 -> 5
          +---+---+---+---+---+---+                               
        b | B |   |   |   | Y | B |  4-1 -> 4  (4-0)^2 + (1-5)^2 = 16+16=32
          +---+---+---+---+---+---+                               
        c |   |   | Z |   |   |   |          2-2 -> 3 moves (1-2)^2 + (5-2)^2 = 1+9=10
          +---+---+---+---+---+---+                               
        d |   |   |   |   |   |   |
          +---+---+---+---+---+---+                               
        e | W |   |   |   | Z | W |          4-4 -> 3 moves (1-4)^2 + (4-5)^2 = 9+1=10
          +---+---+---+---+---+---+                               
        f | Y | Z | W | W | W | W |  0-5     1-5
          +---+---+---+---+---+---+   

X->par Y-par --- X->impar Y->impar  (x mod 2 + y mod 2) mod 2
X->impar Y->par --- X->par Y->impar                        

NEXT PLAYER: WHITE/BLACK

PIECE TO MOVE (a-A): 

WHERE TO MOVE (b-B):
*/