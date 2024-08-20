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

man(frank).
man(carl).

ok:-
    man(carl),
    write('Hello').
ok:-
    write('Hi').