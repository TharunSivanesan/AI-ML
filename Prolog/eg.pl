solve :-
    start(State),
    heuristic(State, N),
    bfs([[(N, State)]], [State], Moves, Path), !,
    length(Moves, Length),
    show([start|Moves], Path),
    format('~nMoves: ~w~n', [Length]).

bfs([[(N, State)|States]|_], _, [], Path) :-
    N=:=0,!,
    reverse([(N, State)|States], Path).

bfs([[(N,State)|States]|Queues], Visited, [Move|Moves], Path) :-
    findall([(M,NextState), (N,State)|States], (
        move(State, NextState, Move),
        \+ member(NextState, Visited),
        heuristic(NextState,M)
    ), NewPaths),
    insert_list(NewPaths,Queues,NewQueue),
    bfs(NewQueue, [State|Visited], Moves, Path).

insert_list([], Queue, Queue).

insert_list([[(N, State)|States]|Rest], Queue, NewQueue) :-
    insert([(N, State)|States], Queue, TempQueue),
    insert_list(Rest, TempQueue, NewQueue).

insert([(N, State)|States], [], [[(N, State)|States]]).
insert([(N, State)|States], [[(H, S)|Ss]|Rest], [[(N, State)|States], [(H, S)|Ss]|Rest]) :- 
    N =< H.
insert([(N, State)|States], [[(H, S)|Ss]|Rest], [[(H, S)|Ss]|NewRest]) :- 
    N > H,
    insert([(N, State)|States], Rest, NewRest).


show([], _).
show([Move|Moves], [(N, State)|States]) :-     
    State = state(A, B, C, D, E, F, G, H, I),
    format('~n~w~n~n', [Move]),
    format('~w ~w ~w~n', [A, B, C]),
    format('~w ~w ~w~n', [D, E, F]),
    format('~w ~w ~w~n', [G, H, I]),
    format('~nHeuristic Function value: ~w~n', [N]),
    show(Moves, States).

start(state(2,5,4,0,1,3,6,7,8)).
% start(state(1,0,2,3,4,5,6,7,8)).


heuristic(State,N):-
    State=state(A,B,C,D,E,F,G,H,I),
    (
    A =:=0 ->
    N is
    abs(B//3)+abs(mod(B,3)-1)+
    abs(C//3)+abs(mod(C,3)-2)+
    abs(D//3-1)+abs(mod(D,3))+
    abs(E//3-1)+abs(mod(E,3)-1)+
    abs(F//3-1)+abs(mod(F,3)-2)+
    abs(G//3-2)+abs(mod(G,3))+
    abs(H//3-2)+abs(mod(H,3)-1)+
    abs(I//3-2)+abs(mod(I,3)-2)
    ;
    B =:=0 ->
    N is
    abs(A//3)+abs(mod(A,3))+
    abs(C//3)+abs(mod(C,3)-2)+
    abs(D//3-1)+abs(mod(D,3))+
    abs(E//3-1)+abs(mod(E,3)-1)+
    abs(F//3-1)+abs(mod(F,3)-2)+
    abs(G//3-2)+abs(mod(G,3))+
    abs(H//3-2)+abs(mod(H,3)-1)+
    abs(I//3-2)+abs(mod(I,3)-2)
    ;
    C =:=0 ->
    N is
    abs(A//3)+abs(mod(A,3))+
    abs(B//3)+abs(mod(B,3)-1)+
    abs(D//3-1)+abs(mod(D,3))+
    abs(E//3-1)+abs(mod(E,3)-1)+
    abs(F//3-1)+abs(mod(F,3)-2)+
    abs(G//3-2)+abs(mod(G,3))+
    abs(H//3-2)+abs(mod(H,3)-1)+
    abs(I//3-2)+abs(mod(I,3)-2)
    ;
    D =:=0 ->
    N is
    abs(A//3)+abs(mod(A,3))+
    abs(B//3)+abs(mod(B,3)-1)+
    abs(C//3)+abs(mod(C,3)-2)+
    abs(E//3-1)+abs(mod(E,3)-1)+
    abs(F//3-1)+abs(mod(F,3)-2)+
    abs(G//3-2)+abs(mod(G,3))+
    abs(H//3-2)+abs(mod(H,3)-1)+
    abs(I//3-2)+abs(mod(I,3)-2)
    ;
    E =:=0 ->
    N is
    abs(A//3)+abs(mod(A,3))+
    abs(B//3)+abs(mod(B,3)-1)+
    abs(C//3)+abs(mod(C,3)-2)+
    abs(D//3-1)+abs(mod(D,3))+
    abs(F//3-1)+abs(mod(F,3)-2)+
    abs(G//3-2)+abs(mod(G,3))+
    abs(H//3-2)+abs(mod(H,3)-1)+
    abs(I//3-2)+abs(mod(I,3)-2)
    ;
    F =:=0 ->
    N is
    abs(A//3)+abs(mod(A,3))+
    abs(B//3)+abs(mod(B,3)-1)+
    abs(C//3)+abs(mod(C,3)-2)+
    abs(D//3-1)+abs(mod(D,3))+
    abs(E//3-1)+abs(mod(E,3)-1)+
    abs(G//3-2)+abs(mod(G,3))+
    abs(H//3-2)+abs(mod(H,3)-1)+
    abs(I//3-2)+abs(mod(I,3)-2)
    ;
    G =:=0 ->
    N is
    abs(A//3)+abs(mod(A,3))+
    abs(B//3)+abs(mod(B,3)-1)+
    abs(C//3)+abs(mod(C,3)-2)+
    abs(D//3-1)+abs(mod(D,3))+
    abs(E//3-1)+abs(mod(E,3)-1)+
    abs(F//3-1)+abs(mod(F,3)-2)+
    abs(H//3-2)+abs(mod(H,3)-1)+
    abs(I//3-2)+abs(mod(I,3)-2)
    ;
    H =:=0 ->
    N is
    abs(A//3)+abs(mod(A,3))+
    abs(B//3)+abs(mod(B,3)-1)+
    abs(C//3)+abs(mod(C,3)-2)+
    abs(D//3-1)+abs(mod(D,3))+
    abs(E//3-1)+abs(mod(E,3)-1)+
    abs(F//3-1)+abs(mod(F,3)-2)+
    abs(G//3-2)+abs(mod(G,3))+
    abs(I//3-2)+abs(mod(I,3)-2)
    ;
    I =:=0 ->
    N is
    abs(A//3)+abs(mod(A,3))+
    abs(B//3)+abs(mod(B,3)-1)+
    abs(C//3)+abs(mod(C,3)-2)+
    abs(D//3-1)+abs(mod(D,3))+
    abs(E//3-1)+abs(mod(E,3)-1)+
    abs(F//3-1)+abs(mod(F,3)-2)+
    abs(G//3-2)+abs(mod(G,3))+
    abs(H//3-2)+abs(mod(H,3)-1)
    ).



move(state(0, B, C, D, E, F, G, H, J), state(B, 0, C, D, E, F, G, H, J), right).
move(state(0, B, C, D, E, F, G, H, J), state(D, B, C, 0, E, F, G, H, J), down).
move(state(A, 0, C, D, E, F, G, H, J), state(0, A, C, D, E, F, G, H, J), left).
move(state(A, 0, C, D, E, F, G, H, J), state(A, C, 0, D, E, F, G, H, J), right).
move(state(A, 0, C, D, E, F, G, H, J), state(A, E, C, D, 0, F, G, H, J), down).
move(state(A, B, 0, D, E, F, G, H, J), state(A, 0, B, D, E, F, G, H, J), left).
move(state(A, B, 0, D, E, F, G, H, J), state(A, B, F, D, E, 0, G, H, J), down).
move(state(A, B, C, 0, E, F, G, H, J), state(0, B, C, A, E, F, G, H, J), up).
move(state(A, B, C, 0, E, F, G, H, J), state(A, B, C, E, 0, F, G, H, J), right).
move(state(A, B, C, 0, E, F, G, H, J), state(A, B, C, G, E, F, 0, H, J), down).
move(state(A, B, C, D, 0, F, G, H, J), state(A, 0, C, D, B, F, G, H, J), up).
move(state(A, B, C, D, 0, F, G, H, J), state(A, B, C, D, F, 0, G, H, J), right).
move(state(A, B, C, D, 0, F, G, H, J), state(A, B, C, D, H, F, G, 0, J), down).
move(state(A, B, C, D, 0, F, G, H, J), state(A, B, C, 0, D, F, G, H, J), left).
move(state(A, B, C, D, E, 0, G, H, J), state(A, B, 0, D, E, C, G, H, J), up).
move(state(A, B, C, D, E, 0, G, H, J), state(A, B, C, D, 0, E, G, H, J), left).
move(state(A, B, C, D, E, 0, G, H, J), state(A, B, C, D, E, J, G, H, 0), down).
move(state(A, B, C, D, E, F, 0, H, J), state(A, B, C, D, E, F, H, 0, J), right).
move(state(A, B, C, D, E, F, 0, H, J), state(A, B, C, D, E, 0, F, H, J), up).
move(state(A, B, C, D, E, F, G, 0, J), state(A, B, C, D, E, F, 0, G, J), left).
move(state(A, B, C, D, E, F, G, 0, J), state(A, B, C, D, E, F, G, J, 0), up).
