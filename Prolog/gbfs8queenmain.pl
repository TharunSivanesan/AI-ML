best_first([[Goal|Path]|_],Goal,[Goal|Path],0).
best_first([Path|Queue],Goal,Finalpath,N):-
    extend(Path,Newpaths),
    append(Queue,Newpaths,Queue1),
    sort_queue1(Queue1,Newqueue,Goal),
    wrq(Newqueue),
    best_first(Newqueue,Goal,Finalpath,M),
    N is M+1.

extend([Node|Path],Newpaths):-
    findall([Newnode,Node|Path],(arc(Node,Newnode),\+member(Newnode,[Node|Path])),Newpaths).
sort_queue1(L,L2,Goal):-
    swap1(L,L1,Goal),!,
    sort_queue1(L1,L2,Goal).
sort_queue1(L,L,Goal).
swap1([[A1|B1],[A2|B2]|T],[[A2|B2],[A1|B1]|T],Goal):-
    hh(A1,Goal,W1),
    hh(A2,Goal,W2),
    W1>W2.
swap1([X|T],[X|V],Goal):-
    swap1(T,V,Goal).
hh(State,Goal,Value):-
    compare1(State,Goal,Value1),
    Value is 4-Value1,
    number(Value),!.
hh(State,Goal,Value):-
    write("Invalid heauristics"),
    write(h(State,Goal,Value)),nl,
    abort.
wrq(Q):-
    length(Q,N),writeln(N).
