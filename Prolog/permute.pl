permute1([A1,A2,A3,B1,B2,B3,C1,C2,C3],S):-
    S = [A2,A1,A3,B1,B2,B3,C1,C2,C3];
    S = [B1,A2,A3,A1,B2,B3,C1,C2,C3];
    S = [A1,B2,A3,B1,A2,B3,C1,C2,C3];
    S = [A1,A3,A2,B1,B2,B3,C1,C2,C3];
    S = [A1,A2,B3,B1,B2,A3,C1,C2,C3];

    S = [A1,A2,A3,B2,B1,B3,C1,C2,C3];
    S = [A1,A2,A3,C1,B2,B3,B1,C2,C3];
    S = [A1,A2,A3,B1,B3,B2,C1,C2,C3];
    S = [A1,A2,A3,B1,C2,B3,C1,B2,C3];
    S = [A1,A2,A3,B1,B2,C3,C1,C2,B3];

    S = [A1,A2,A3,B1,B2,B3,C2,C1,C3];
    S = [A1,A2,A3,B1,B2,B3,C1,C3,C2].
