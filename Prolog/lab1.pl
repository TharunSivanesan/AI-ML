:- use_module(library(clpfd)).
n_queens(N, Qs) :-
        length(Qs, N),
        Qs ins 1..N,
        safe_queens(Qs).

safe_queens([]).
safe_queens([Q|Qs]) :- safe_queens(Qs, Q, 1), safe_queens(Qs).

safe_queens([], _, _).
safe_queens([Q|Qs], Q0, D0) :-
    Q0 #\= Q,
    abs(Q0 - Q) #\= D0,
    D1 #= D0 + 1,
    safe_queens(Qs, Q0, D1).


fact(0, R):-
    R is 1.
fact(N, F):-
    N #> 0,
    N1 is N-1,
    fact(N1,F1),
    F is N*F1.


:- use_module(library(clpfd)).

% Initialize the 4x4 board
init_board(Board) :-
    length(Board, 4),
    maplist(same_length(Board), Board),
    append(Board, Cells),
    Cells ins 0..1,  % 0 for empty, 1 for mine
    place_mines(Cells, 4).

% Place 4 mines randomly
place_mines(Cells, 0) :- !.
place_mines(Cells, N) :-
    N > 0,
    random_select(1, Cells, Rest),
    N1 #= N - 1,
    place_mines(Rest, N1).

% Print the board
print_board([]).
print_board([Row|Rows]) :-
    print_row(Row),
    nl,
    print_board(Rows).

print_row([]).
print_row([Cell|Cells]) :-
    ( Cell == 1 -> write('*') ; write('.') ),
    write(' '),
    print_row(Cells).

% Check if a cell is within bounds
in_bounds(X, Y) :-
    X >= 1, X =< 4,
    Y >= 1, Y =< 4.

% Reveal a cell
reveal(Board, X, Y) :-
    in_bounds(X, Y),
    nth1(X, Board, Row),
    nth1(Y, Row, Cell),
    ( Cell == 1 ->
        write('Boom! You hit a mine.'), nl, print_board(Board)
    ; count_adjacent_mines(Board, X, Y, Count),
      write('Safe. Adjacent mines: '), write(Count), nl ).

% Count adjacent mines
count_adjacent_mines(Board, X, Y, Count) :-
    findall(1,
        ( adj(X, Y, AdjX, AdjY),
          in_bounds(AdjX, AdjY),
          nth1(AdjX, Board, Row),
          nth1(AdjY, Row, Cell),
          Cell == 1
        ), Mines),
    length(Mines, Count).

% Get all adjacent positions
adj(X, Y, AdjX, AdjY) :-
    DX in -1..1, DY in -1..1,
    DX #\= 0; DY #\= 0,
    AdjX #= X + DX,
    AdjY #= Y + DY.

% Start the game
start :-
    init_board(Board),
    print_board(Board),
    write('Enter row (1-4): '), read(Row),
    write('Enter column (1-4): '), read(Col),
    reveal(Board, Row, Col).


% Generate a 3x3 magic square
magic_square(Square) :-
    Square = [A, B, C,
              D, E, F,
              G, H, I],
    Square ins 1..9,
    all_distinct(Square),
    A + B + C #= 15,
    D + E + F #= 15,
    G + H + I #= 15,
    A + D + G #= 15,
    B + E + H #= 15,
    C + F + I #= 15,
    A + E + I #= 15,
    C + E + G #= 15,
    label(Square).

% Print the magic square
print_square([A, B, C,
              D, E, F,
              G, H, I]) :-
    format('~w ~w ~w~n', [A, B, C]),
    format('~w ~w ~w~n', [D, E, F]),
    format('~w ~w ~w~n', [G, H, I]).

% Main predicate to generate and print the magic square
generate_magic_square :-
    magic_square(Square),
    print_square(Square).

% Initialize the empty board
init_board([[' ', ' ', ' '],
            [' ', ' ', ' '],
            [' ', ' ', ' ']]).

% Print the board
print_board([A, B, C]) :-
    format('~w | ~w | ~w~n', A),
    write('---------'), nl,
    format('~w | ~w | ~w~n', B),
    write('---------'), nl,
    format('~w | ~w | ~w~n', C), nl.

% Start the game
start :-
    init_board(Board),
    print_board(Board),
    play(Board, 'X').

% Check if the board is full (draw)
board_full(Board) :-
    \+ (member(Row, Board), member(' ', Row)).

% Check if a move is valid
valid_move(Board, Row, Col) :-
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Cell),
    Cell = ' '.

% Update the board with a move
make_move(Board, Row, Col, Player, NewBoard) :-
    nth1(Row, Board, RowList),
    replace(RowList, Col, Player, NewRowList),
    replace(Board, Row, NewRowList, NewBoard).

% Replace an element in a list
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    replace(T, I1, X, R).

% Check for a win
win(Board, Player) :-
    ( row_win(Board, Player)
    ; col_win(Board, Player)
    ; diag_win(Board, Player) ).

% Check rows for a win
row_win(Board, Player) :-
    member(Row, Board),
    all_same(Row, Player).

% Check columns for a win
col_win(Board, Player) :-
    transpose(Board, Transposed),
    row_win(Transposed, Player).

% Check diagonals for a win
diag_win(Board, Player) :-
    Board = [[A, _, B],
             [_, C, _],
             [D, _, E]],
    (A = Player, C = Player, E = Player;
     B = Player, C = Player, D = Player).

% Check if all elements in a list are the same
all_same([X, X, X], X).

% Transpose a matrix
transpose([[A, B, C],
           [D, E, F],
           [G, H, I]],
          [[A, D, G],
           [B, E, H],
           [C, F, I]]).

% Play the game
play(Board, Player) :-
    ( win(Board, 'X') -> write('Player X wins!'), nl, print_board(Board)
    ; win(Board, 'O') -> write('Player O wins!'), nl, print_board(Board)
    ; board_full(Board) -> write('It\'s a draw!'), nl, print_board(Board)
    ; write('Player '), write(Player), write(', enter row (1-3): '), read(Row),
      write('Player '), write(Player), write(', enter column (1-3): '), read(Col),
      ( valid_move(Board, Row, Col) ->
          make_move(Board, Row, Col, Player, NewBoard),
          print_board(NewBoard),
          next_player(Player, NextPlayer),
          play(NewBoard, NextPlayer)
      ; write('Invalid move, try again.'), nl,
        play(Board, Player)
      )
    ).

% Switch players
next_player('X', 'O').
next_player('O', 'X').
