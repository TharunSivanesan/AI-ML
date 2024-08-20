
insert(N, State, [], [(N, State)]).  % Base case: empty queue
insert(N, State, [(H, S)|Rest], [(N, State), (H, S)|Rest]) :-  
    N =< H.
insert(N, State, [(H, S)|Rest], [(H, S)|NewRest]) :-  
    N > H,
    insert(N, State, Rest, NewRest).
