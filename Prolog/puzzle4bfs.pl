% Start the BFS search
ids :-
    start(State),
    bfs([[State]], [State], Solution), !,
    show(Solution).

% BFS: When the goal state is found
bfs([[Node|Path]|_], _, Solution):-
    goal(Node), !,
    reverse([Node|Path], Solution).

% BFS: Expand nodes and continue the search
bfs([[N|Path]|Paths], Visited, Solution) :-
    findall([M,N|Path], (move(N,M), \+ member(M,Visited)), NewPaths),
    append(Paths, NewPaths, Path1),
    bfs(Path1, [N|Visited], Solution).

% Display the solution states
show([]).
show([State|States]) :-
    State = state(A,B,C,D),
    format('~w ~w ~n',[A,B]),
    format('~w ~w ~n',[C,D]),
    format('~n'),
    show(States).

% Define the goal state
goal(state(*,0,1,2)).

% Define the start state
start(state(*,1,2,0)).

% Define possible moves
move(state(*,B,C,D), state(B,*,C,D)).
move(state(*,B,C,D), state(C,B,*,D)).
move(state(A,*,C,D), state(*,A,C,D)).
move(state(A,*,C,D), state(A,D,C,*)).
move(state(A,B,*,D), state(*,B,A,D)).
move(state(A,B,*,D), state(A,B,D,*)).
move(state(A,B,C,*), state(A,B,*,C)).
move(state(A,B,C,*), state(A,*,C,B)).
