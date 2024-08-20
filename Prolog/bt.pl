:- use_module(library(clpfd)).

list_length([],0).

list_length([_|A],B) :-
    list_length(A,C),
    B is C+1.

concatenate([], L, L).
concatenate([H|T], L, [H|R]) :-
    concatenate(T, L, R).

union([],L,L).
union([X1|Y],Z,W):-
    list_member(X1,Z),union(Y,Z,W).

union([X1|Y],Z,[X1|W]):-
    \+ list_member(X1,Z),union(Y,Z,W).

list_member(A,[A|_]).
list_member(X,[_|A]):-
    list_member(X,A).


intersection([X|A],L2,L3):-
    \+ list_member(X,L2),intersection(A,L2,L3).

intersection([X|A],L2,[X|L3]):-
    list_member(X,L2),intersection(A,L2,L3).

intersection([],L,[]).

mergesort([],L,L).
mergesort(L,[],L).

mergesort([X1|L1],[X2|L2],[X1|L3]):-
    X1 #=< X2,
    mergesort(L1,[X2|L2],L3).
mergesort([X1|L1],[X2|L2],[X2|L3]):-
    X1 #> X2,
    mergesort([X1|L1],L2,L3).


f(X,0):- X < 3,!.
f(X,2):- X<6,!.
f(X,4).

max(X,Y,X):- X>=Y,!.
max(X,Y,Y).

p(1).
p(2) :- !.
p(3).