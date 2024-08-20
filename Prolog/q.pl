:- use_module(library(clpfd)).
n_queens(N, Qs) :-
        length(Qs, N),
        Qs ins 1..N,
        safe_queens(Qs),
        label(Qs).

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

sudoku(Rows) :-
        length(Rows, 9), maplist(same_length(Rows), Rows),
        append(Rows, Vs), Vs ins 1..9,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns),
        Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
        blocks(As, Bs, Cs),
        blocks(Ds, Es, Fs),
        blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
        all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
        blocks(Ns1, Ns2, Ns3).

problem(1, [[_,_,_,_,_,_,_,_,_],
            [_,_,_,_,_,3,_,8,5],
            [_,_,1,_,2,_,_,_,_],
            [_,_,_,5,_,7,_,_,_],
            [_,_,4,_,_,_,1,_,_],
            [_,9,_,_,_,_,_,_,_],
            [5,_,_,_,_,_,_,7,3],
            [_,_,2,_,1,_,_,_,_],
            [_,_,_,_,4,_,_,_,9]]).

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
