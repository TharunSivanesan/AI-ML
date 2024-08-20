% Start by defining the positions of corners and edges
corners([1, 3, 7, 9]).
edges([2, 4, 6, 8]).
center([5]).

% Helper predicates to check if a position is a corner or an edge
is_corner(Pos) :- corners(Corners), member(Pos, Corners).
is_edge(Pos) :- edges(Edges), member(Pos, Edges).
is_center(Pos) :- center(Center), member(Pos, Center).

% Main DFS predicate
dfs(Square, Square) :-
    filled(Square),
    valid(Square), !.

dfs(Square, FinalSquare) :-
    next_pos(Square, Pos),
    next_number(Square, Num),
    place_number(Square, Pos, Num, NewSquare),
    valid(NewSquare),
    dfs(NewSquare, FinalSquare).

% Initialize the square with 0s
init_square([0, 0, 0, 0, 0, 0, 0, 0, 0]).

% Check if the square is completely filled
filled(Square) :- \+ member(0, Square).

% Determine the next position based on constraints
next_pos(Square, Pos) :-
    member(Pos, [1, 2, 3, 4, 5, 6, 7, 8, 9]),
    nth1(Pos, Square, 0),
    is_valid_pos(Pos).

% Check if a position is valid based on the number to be placed
is_valid_pos(Pos) :- 
    (is_corner(Pos) ; is_edge(Pos) ; is_center(Pos)).

% Generate the next number to be placed
next_number(Square, Num) :-
    between(1, 9, Num),
    \+ member(Num, Square),
    (Num mod 2 =:= 0 -> even_number_constraint(Square, Num) ; odd_number_constraint(Square, Num)).

% Place a number at the specified position
place_number(Square, Pos, Num, NewSquare) :-
    nth1(Pos, Square, _, Rest),
    nth1(Pos, NewSquare, Num, Rest).

% Check the sum of rows, columns, and diagonals
valid(Square) :-
    row_sums(Square),
    col_sums(Square),
    diag_sums(Square).

row_sums([A,B,C,D,E,F,G,H,I]) :-
    Sum1 is A + B + C,
    Sum2 is D + E + F,
    Sum3 is G + H + I,
    Sum1 =< 15, Sum2 =< 15, Sum3 =< 15.

col_sums([A,B,C,D,E,F,G,H,I]) :-
    Sum1 is A + D + G,
    Sum2 is B + E + H,
    Sum3 is C + F + I,
    Sum1 =< 15, Sum2 =< 15, Sum3 =< 15.

diag_sums([A,B,C,D,E,F,G,H,I]) :-
    Sum1 is A + E + I,
    Sum2 is C + E + G,
    Sum1 =< 15, Sum2 =< 15.

% Constraints for placing even numbers in corners
even_number_constraint(Square, Num) :-
    corners(Corners),
    findall(Pos, (member(Pos, Corners), nth1(Pos, Square, 0)), PossiblePos),
    PossiblePos \= [].

% Constraints for placing odd numbers in other positions
odd_number_constraint(Square, Num) :-
    edges(Edges),
    findall(Pos, (member(Pos, Edges), nth1(Pos, Square, 0)), PossiblePos),
    PossiblePos \= [].

% Example usage
solve_magic_square(Solution) :-
    init_square(Square),
    dfs(Square, Solution).
