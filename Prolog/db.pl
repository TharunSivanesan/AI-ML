fact(0,Result):-
    Result is 1.
fact(N,Result):-
    N>0,
    N1 is N-1,
    fact(N1,R1),
    Result is R1*N.


:- use_module(library(clpfd)).

% Initialize the 4x4 board with hidden cells
init_board(Board, Revealed) :-
    length(Board, 4),
    maplist(same_length(Board), Board),
    append(Board, Cells),
    Cells ins 0..1,  % 0 for empty, 1 for mine
    place_mines(Cells, 4),
    length(Revealed, 4),
    maplist(same_length(Revealed), Revealed),
    maplist(maplist(=(hidden)), Revealed).  % All cells start as hidden

% Place 4 mines randomly
place_mines(Cells, 0) :- !.
place_mines(Cells, N) :-
    N > 0,
    random_select(1, Cells, Rest),
    N1 #= N - 1,
    place_mines(Rest, N1).

% Print the board with hidden and revealed cells
print_board([], []).
print_board([Row|Rows], [RevRow|RevRows]) :-
    print_row(Row, RevRow),
    nl,
    print_board(Rows, RevRows).

print_row([], []).
print_row([Cell|Cells], [hidden|RevCells]) :-
    write('. '),  % Hidden cell
    print_row(Cells, RevCells).
print_row([Cell|Cells], [revealed|RevCells]) :-
    ( Cell == 1 -> write('* ') ; write('0 ') ),  % Revealed cell
    print_row(Cells, RevCells).

% Check if a cell is within bounds
in_bounds(X, Y) :-
    X >= 1, X =< 4,
    Y >= 1, Y =< 4.

% Reveal a cell
reveal(Board, Revealed, X, Y, NewRevealed) :-
    in_bounds(X, Y),
    nth1(X, Board, Row),
    nth1(Y, Row, Cell),
    nth1(X, Revealed, RevRow),
    nth1(Y, RevRow, CellState),
    ( CellState == hidden ->
        ( Cell == 1 ->
            write('Boom! You hit a mine.'), nl, % Game over
            maplist(maplist(=(revealed)), Revealed, NewRevealed), % Reveal all cells
            print_board(Board, NewRevealed)
        ; count_adjacent_mines(Board, X, Y, Count),
          write('Safe. Adjacent mines: '), write(Count), nl,
          reveal_cell(Revealed, X, Y, NewRevealed) % Update revealed state
        )
    ; NewRevealed = Revealed, % Cell already revealed
      write('Cell already revealed, choose another.'), nl ).

% Count adjacent mines
count_adjacent_mines(Board, X, Y, Count) :-
    findall(1,
        ( adj(X, Y, AdjX, AdjY),
          in_bounds(AdjX, AdjY),
          nth1(AdjX, Board, AdjRow),
          nth1(AdjY, AdjRow, Cell),
          Cell == 1
        ), Mines),
    length(Mines, Count).

% Get all adjacent positions
adj(X, Y, AdjX, AdjY) :-
    DX in -1..1, DY in -1..1,
    DX #\= 0; DY #\= 0,
    AdjX #= X + DX,
    AdjY #= Y + DY.

% Update the revealed state
reveal_cell(Revealed, X, Y, NewRevealed) :-
    nth1(X, Revealed, RevRow),
    nth1(Y, RevRow, _),
    replace(RevRow, Y, revealed, NewRevRow),
    replace(Revealed, X, NewRevRow, NewRevealed).

% Replace an element in a list
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1, 
    I1 is I - 1, 
    replace(T, I1, X, R).

% Start the game
start :-
    init_board(Board, Revealed),
    game_loop(Board, Revealed).

% Game loop
game_loop(Board, Revealed) :-
    print_board(Board, Revealed),
    write('Enter row (1-4): '), read(Row),
    write('Enter column (1-4): '), read(Col),
    reveal(Board, Revealed, Row, Col, NewRevealed),
    ( maplist(maplist(=(revealed)), NewRevealed) ->
        write('Game over.')
    ; game_loop(Board, NewRevealed)
    ).

:- use_module(library(clpfd)).

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

