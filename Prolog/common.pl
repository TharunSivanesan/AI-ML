compare1([Node|[]],[Node|[]],1).
compare1([Node|[]],[Goal|[]],M):-
    Goal=\=Node,
    M is 0.
compare1([Node|Nodes],[Goal|Goals],N):-
    Node=:=Goal,!,
    compare1(Nodes,Goals,M),
    N is M+1.
compare1([Node|Nodes],[Goal|Goals],N):-
    Node=\=Goal,!,
    compare1(Nodes,Goals,N).
