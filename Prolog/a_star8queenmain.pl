a_star([[Goal|Path]|_],Goal,[Goal|Path],0,_).
a_star([Path|Queue],Goal,Finalpath,N,P):-
    extend(Path,Queue1),
    % append(Queue,Newpaths,Queue1),
    sort_queue1(Queue1,Newqueue,Goal,P),
    wrq(Newqueue),
    a_star(Newqueue,Goal,Finalpath,M,P+1),
    N is M+1.

extend([Node|Path],Newpaths):-
    findall([Newnode,Node|Path],(arc(Node,Newnode),\+member(Newnode,[Node|Path])),Newpaths).
sort_queue1(L,L2,Goal,P):-
    swap1(L,L1,Goal,P),!,
    sort_queue1(L1,L2,Goal,P).
sort_queue1(L,L,Goal,P).
swap1([[A1|B1],[A2|B2]|T],[[A2|B2],[A1|B1]|T],Goal,P):-
    hh(A1,Goal,W1,P),
    hh(A2,Goal,W2,P),
    W1>W2.
swap1([X|T],[X|V],Goal,P):-
    swap1(T,V,Goal,P).
hh(State,Goal,Value,P):-
    compare1(State,Goal,Value1),
    Value is 4-Value1+P,
    number(Value),!.
hh(State,Goal,Value,P):-
    write("Invalid heauristics"),
    write(h(State,Goal,Value.P)),nl,
    abort.
wrq(Q):-
    length(Q,N),writeln(N).
