% Start state
start(state(6,1,3,4,*,5,7,2,0)).

% Goal state
goal(state(*,0,1,2,3,4,5,6,7)).

% Cost function: lower cost for higher numbers
move_cost(1, 7).
move_cost(2, 6).
move_cost(3, 5).
move_cost(4, 4).
move_cost(5, 3).
move_cost(6, 2).
move_cost(7, 1).

% Move definitions with cost calculation
move(state(*,B,C,D,E,F,G,H,J), state(B,*,C,D,E,F,G,H,J), right, Cost) :-
    move_cost(B, Cost).
move(state(*,B,C,D,E,F,G,H,J), state(D,B,C,*,E,F,G,H,J), down, Cost) :-
    move_cost(D, Cost).
move(state(A,*,C,D,E,F,G,H,J), state(*,A,C,D,E,F,G,H,J), left, Cost) :-
    move_cost(A, Cost).
move(state(A,*,C,D,E,F,G,H,J), state(A,C,*,D,E,F,G,H,J), right, Cost) :-
    move_cost(C, Cost).
move(state(A,*,C,D,E,F,G,H,J), state(A,E,C,D,*,F,G,H,J), down, Cost) :-
    move_cost(E, Cost).
move(state(A,B,*,D,E,F,G,H,J), state(A,*,B,D,E,F,G,H,J), left, Cost) :-
    move_cost(B, Cost).
move(state(A,B,*,D,E,F,G,H,J), state(A,B,F,D,E,*,G,H,J), down, Cost) :-
    move_cost(F, Cost).
move(state(A,B,C,*,E,F,G,H,J), state(*,B,C,A,E,F,G,H,J), up, Cost) :-
    move_cost(A, Cost).
move(state(A,B,C,*,E,F,G,H,J), state(A,B,C,E,*,F,G,H,J), right, Cost) :-
    move_cost(E, Cost).
move(state(A,B,C,*,E,F,G,H,J), state(A,B,C,G,E,F,*,H,J), down, Cost) :-
    move_cost(G, Cost).
move(state(A,B,C,D,*,F,G,H,J), state(A,*,C,D,B,F,G,H,J), up, Cost) :-
    move_cost(B, Cost).
move(state(A,B,C,D,*,F,G,H,J), state(A,B,C,D,F,*,G,H,J), right, Cost) :-
    move_cost(F, Cost).
move(state(A,B,C,D,*,F,G,H,J), state(A,B,C,D,H,F,G,*,J), down, Cost) :-
    move_cost(H, Cost).
move(state(A,B,C,D,*,F,G,H,J), state(A,B,C,*,D,F,G,H,J), left, Cost) :-
    move_cost(D, Cost).
move(state(A,B,C,D,E,*,G,H,J), state(A,B,*,D,E,C,G,H,J), up, Cost) :-
    move_cost(C, Cost).
move(state(A,B,C,D,E,*,G,H,J), state(A,B,C,D,*,E,G,H,J), left, Cost) :-
    move_cost(E, Cost).
move(state(A,B,C,D,E,*,G,H,J), state(A,B,C,D,E,J,G,H,*), down, Cost) :-
    move_cost(J, Cost).
move(state(A,B,C,D,E,F,*,H,J), state(A,B,C,D,E,F,H,*,J), right, Cost) :-
    move_cost(H, Cost).
move(state(A,B,C,D,E,F,*,H,J), state(A,B,C,*,E,F,D,H,J), up, Cost) :-
    move_cost(D, Cost).
move(state(A,B,C,D,E,F,G,*,J), state(A,B,C,D,E,F,*,G,J), left, Cost) :-
    move_cost(G, Cost).
move(state(A,B,C,D,E,F,G,*,J), state(A,B,C,D,*,F,G,E,J), up, Cost) :-
    move_cost(E, Cost).
move(state(A,B,C,D,E,F,G,*,J), state(A,B,C,D,E,F,G,J,*), right, Cost) :-
    move_cost(J, Cost).
move(state(A,B,C,D,E,F,G,H,*), state(A,B,C,D,E,*,G,H,F), up, Cost) :-
    move_cost(F, Cost).
move(state(A,B,C,D,E,F,G,H,*), state(A,B,C,D,E,F,G,*,H), left, Cost) :-
    move_cost(H, Cost).

% UCS Algorithm
ucs(Path, Moves, TotalCost) :-
    start(State),
    ucs([[State, [], 0]], [], Path, Moves, TotalCost).

% Base case: reaching the goal state
ucs([[State, Path, Cost]|_], _, FinalPath, FinalMoves, Cost) :-
    goal(State),
    reverse([State|Path], FinalPath),
    extract_moves(FinalPath, FinalMoves).

% Recursive case: expanding the next state
ucs([[State, Path, Cost]|Rest], Visited, FinalPath, FinalMoves, TotalCost) :-
    findall([NextState, [State|Path], NewCost],
            (move(State, NextState, Move, MoveCost),
             \+ member(NextState, Visited),
             NewCost is Cost + MoveCost),
            NewPaths),
    append(Rest, NewPaths, Queue),
    sort(3, @=<, Queue, SortedQueue), % Sort by cost
    ucs(SortedQueue, [State|Visited], FinalPath, FinalMoves, TotalCost).

% Extracts the sequence of moves from the sequence of states
extract_moves([_], []).
extract_moves([S1, S2 | States], [Move | Moves]) :-
    move(S1, S2, Move, _),
    extract_moves([S2 | States], Moves).

% Run the UCS algorithm
solve(Path, Moves, TotalCost) :-
    ucs(Path, Moves, TotalCost),
    show_solution(Path, Moves, TotalCost).

% Display the solution path and moves
show_solution(Path, Moves, TotalCost) :-
    show(Moves, Path),
    format('~nTotal Cost: ~w~n', [TotalCost]),
    length(Moves, N),
    format('~nMoves: ~w~n', [N]).

show([], _).
show([Move|Moves], [State|States]) :-
    State = state(A,B,C,D,E,F,G,H,I),
    format('~n~w~n~n', [Move]),
    format('~w ~w ~w~n',[A,B,C]),
    format('~w ~w ~w~n',[D,E,F]),
    format('~w ~w ~w~n',[G,H,I]),
    show(Moves, States).
