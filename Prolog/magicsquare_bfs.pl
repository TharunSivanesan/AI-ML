solve1(Start,Solution):-
    permutation(Start,[1,2,3,4,5,6,7,8,9]),
    breadthfirst1([[Start]],Solution).

breadthfirst1([[Node|Path]|_],[Node|Path]):-
    goal1(Node).
breadthfirst1([[N|Path]|Paths],Solution):-
    bagof([M,N|Path],(s(N,M),\+ member(M,[N|Path])),Newpaths),
          append(Paths,Newpaths,Path1),!,
          breadthfirst1(Path1,Solution);
          breadthfirst1(Paths,Solution).
goal1([A1, A2, A3, B1, B2, B3, C1, C2, C3]):-
	A1 + A2 + A3 =:= 15, /*Checks the sum of Row	1*/
	B1 + B2 + B3 =:= 15, /*Checks the sum of Row	2*/
	C1 + C2 + C3 =:= 15, /*Checks the sum of Row	3*/
	A1 + B1 + C1 =:= 15, /*Checks the sum of Col	1*/
	A2 + B2 + C2 =:= 15, /*Checks the sum of Col	2*/
	A3 + B3 + C3 =:= 15, /*Checks the sum of Col	3*/
	A1 + B2 + C3 =:= 15, /*Checks the sum of Diag	1*/
	A3 + B2 + C1 =:= 15. /*Checks the sum of Diag	2*/

s(N,M):-
   permute1(N,M).

