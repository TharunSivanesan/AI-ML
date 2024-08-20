solve:-
    start(State),
    length(Moves,N),
    bfs([[State]],[State], Moves,Path), !,
    show([start|Moves], Path),
    format('~nMoves: ~w~n',[N]).

bfs([[State|States]|_],[_],[],Path) :-
    goal(State), !,
    reverse([State|States], Path).

bfs([[State|States]|Queues], Visited,[Move|Moves],Path) :-
    findall([NextState,State|States],(
        move(State, NextState, Move),
        \+ member(NextState, Visited)
        ),
        NewPaths),
    append(Queues, NewPaths, NewQueue),
    bfs(NewQueue, [State|Visited], Moves, Path).

show([], _).
show([Move|Moves], [State|States]) :-     
    State = state(A,B,C,D,E,F,G,H,I),
    format('~n~w~n~n', [Move]),
    format('~w ~w ~w~n',[A,B,C]),
    format('~w ~w ~w~n',[D,E,F]),
    format('~w ~w ~w~n',[G,H,I]),
    show(Moves, States).

goal(state(*,0,1,2,3,4,5,6,7)).
start(state(0,*,1,2,3,4,5,6,7)).

move(state(*,B,C,D,E,F,G,H,J), state(B,*,C,D,E,F,G,H,J), right).
move(state(*,B,C,D,E,F,G,H,J), state(D,B,C,*,E,F,G,H,J), down).
move(state(A,*,C,D,E,F,G,H,J), state(*,A,C,D,E,F,G,H,J), left).
move(state(A,*,C,D,E,F,G,H,J), state(A,C,*,D,E,F,G,H,J), right).
move(state(A,*,C,D,E,F,G,H,J), state(A,E,C,D,*,F,G,H,J), down).
move(state(A,B,*,D,E,F,G,H,J), state(A,*,B,D,E,F,G,H,J), left).
move(state(A,B,*,D,E,F,G,H,J), state(A,B,F,D,E,*,G,H,J), down).
move(state(A,B,C,*,E,F,G,H,J), state(*,B,C,A,E,F,G,H,J), up).
move(state(A,B,C,*,E,F,G,H,J), state(A,B,C,E,*,F,G,H,J), right).
move(state(A,B,C,*,E,F,G,H,J), state(A,B,C,G,E,F,*,H,J), down).
move(state(A,B,C,D,*,F,G,H,J), state(A,*,C,D,B,F,G,H,J), up).
move(state(A,B,C,D,*,F,G,H,J), state(A,B,C,D,F,*,G,H,J), right).
move(state(A,B,C,D,*,F,G,H,J), state(A,B,C,D,H,F,G,*,J), down).
move(state(A,B,C,D,*,F,G,H,J), state(A,B,C,*,D,F,G,H,J), left).
move(state(A,B,C,D,E,*,G,H,J), state(A,B,*,D,E,C,G,H,J), up).
move(state(A,B,C,D,E,*,G,H,J), state(A,B,C,D,*,E,G,H,J), left).
move(state(A,B,C,D,E,*,G,H,J), state(A,B,C,D,E,J,G,H,*), down).
move(state(A,B,C,D,E,F,*,H,J), state(A,B,C,D,E,F,H,*,J), right).
move(state(A,B,C,D,E,F,*,H,J), state(A,B,C,*,E,F,D,H,J), up).
move(state(A,B,C,D,E,F,G,*,J), state(A,B,C,D,E,F,*,G,J), left).
move(state(A,B,C,D,E,F,G,*,J), state(A,B,C,D,*,F,G,E,J), up).
move(state(A,B,C,D,E,F,G,*,J), state(A,B,C,D,E,F,G,J,*), right).
move(state(A,B,C,D,E,F,G,H,*), state(A,B,C,D,E,*,G,H,F), up).
move(state(A,B,C,D,E,F,G,H,*), state(A,B,C,D,E,F,G,*,H), left).
