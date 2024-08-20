solve(Start,Solution):-
    member(Start,[1,2,3,4,5,6,7,8]),
    breadthfirst([[Start]],Solution),!.

breadthfirst([[State|States]|_],[State|States]):-
    goal([State|States]),!.

breadthfirst([[N|Path]|Paths],Solution):-
    findall([M,N|Path],(member(M,[1,2,3,4,5,6,7,8]),\+ member(M,[N|Path]),noattack(M,[N|Path],1)),Newpaths),
          append(Paths,Newpaths,Path1),
          breadthfirst(Path1,Solution).

goal([_,_,_,_,_,_,_,_]).

noattack(_,[],_).
noattack(X,[X1|Others],N):-
    X=\=X1,
    abs(X-X1)=\=N,
    N1 is N+1,
    noattack(X,Others,N1).

