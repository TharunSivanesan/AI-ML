% Define the initial state (empty board)
initial_state([]).
% Define the final state (a board with 8 queens)
final_state(Board) :-
 length(Board, 8).
% Generate a list of numbers 1 through 8
range(1, [1]).
range(N, [N|Rest]) :- 
    N > 1, 
    N1 is N - 1, 
    range(N1, Rest).
% Check if the position is safe from attack by other queens
safe([],_).
safe([Q|Qs], (Row, Col)) :-
 Q = (Row1, Col1),
 Col =\= Col1,
 abs(Row - Row1) =\= abs(Col - Col1),
 safe(Qs, (Row, Col)).

% Generate successors by placing a queen in each column of the next row
successor(Board, NextBoard) :-
 length(Board, Row),
 Row1 is Row + 1,
 range(8, Columns),
 member(Col, Columns),
 safe(Board, (Row1, Col)),
NextBoard = [(Row1, Col)|Board].

% Implement UCS using a priority queue (which behaves similarly to BFS)
ucs([[State|Path]|_], [State|Path]) :-
 final_state(State).
ucs([Path|Paths], Solution) :-
 Path = [State|_],
 findall([NextState, State|Path],
 (successor(State, NextState),
 \+ member(NextState, [State|Path])),
 NewPaths),
 append(Paths, NewPaths, Queue),
 sort(1, @=<, Queue, SortedQueue), 
 ucs(SortedQueue, Solution).
 
% Start UCS with the initial state
solve_ucs(Solution) :-
 initial_state(Init),
 ucs([[Init]], RevSolution),
 reverse(RevSolution, Solution).

