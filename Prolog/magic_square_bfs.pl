solve_magic_square_bfs(Square) :-
    % Initialize the queue with the list of numbers
    numlist(1, 9, Numbers),
    bfs([Numbers], Square).

% Breadth-First Search (BFS) implementation
bfs([], _) :- fail.  % No solution found if queue is empty
bfs([Current|_], Solution) :-
    permutation(Current, Permuted),
    is_magic_square(Permuted), !, % Check if it is a magic square
    Solution = Permuted.
bfs([Current|Rest], Solution) :-
    % Generate next permutations to explore
    findall(Next, (append(Left, [X|Right], Current), append(Left, Right, Temp), member(X, Temp), Next = [X|Temp]), NextStates),
    append(Rest, NextStates, NewQueue),
    bfs(NewQueue, Solution).

% Check if the 3x3 list represents a valid magic square
is_magic_square([A, B, C, D, E, F, G, H, I]) :-
    MagicSum = 15,
    % Check rows
    Row1 is A + B + C,
    Row2 is D + E + F,
    Row3 is G + H + I,
    % Check columns
    Col1 is A + D + G,
    Col2 is B + E + H,
    Col3 is C + F + I,
    % Check diagonals
    Diag1 is A + E + I,
    Diag2 is C + E + G,
    % All sums must be equal to the magic sum
    Row1 =:= MagicSum,
    Row2 =:= MagicSum,
    Row3 =:= MagicSum,
    Col1 =:= MagicSum,
    Col2 =:= MagicSum,
    Col3 =:= MagicSum,
    Diag1 =:= MagicSum,
    Diag2 =:= MagicSum.
