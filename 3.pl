intersectionTR(_, [], []).
intersectionTR([], _, []).
intersectionTR([H1|T1], L2, [H1|L]):-
    member(H1, L2),
    intersectionTR(T1, L2, L), !.
intersectionTR([_|T1], L2, L):-
    intersectionTR(T1, L2, L).

overlap([],[]).
overlap(A,B):- intersectionTR(A,B,L),check_L(L).

check_L(L):- L > 0. 
