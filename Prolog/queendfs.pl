solve(N,[N]):-
    goal(N).

solve(N,[N|Sol]):-
    safe(N,N1),
    solve(N1,Sol).

safe(Queens,[Queen|Queens]):-
    member(Queen,[1,2,3,4]),
    noattack(Queen,Queens,1).

noattack(_,[],_).
noattack(X,[Q|Qs],N):-
    X =\=Q,
    abs(X-Q)=\=N,
    N1 is N+1,
    noattack(X,Qs,N1).

goal([_,_,_,_]).