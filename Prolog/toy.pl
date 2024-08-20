action(state(room1, dirty, R2), suck, state(room1, clean, R2)).
action(state(room2, R1, dirty), suck, state(room2, R1, clean)).

action(state(room2, R1, R2), left, state(room1, R1, R2)).
action(state(room1, R1, R2), right, state(room2, R1, R2)).
action(state(room1, R1, R2), left, state(room1, R1, R2)).
action(state(room2, R1, R2), right, state(room2, R1, R2)).

goal(state(_, clean, clean)).

solve(State, State, [], _).
solve(State, Goal, [Action|Actions], Visited) :-
    action(State, Action, NextState),
    \+ member(NextState, Visited),
    solve(NextState, Goal, Actions, [NextState|Visited]).

solve_vacuum(InitialState, Actions) :-
    goal(GoalState),
    solve(InitialState, GoalState, Actions, [InitialState]).