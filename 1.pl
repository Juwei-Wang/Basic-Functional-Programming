/*30053278*/
/*juwei wang*/

/* q1 */
my_append([],Y,Y).
my_append([H|X], Y ,[H|Z]):- my_append(X,Y,Z).

pushFront(Item, List, [Item|List]).

my_reverse([],[]).
my_reverse([H|T],R):- my_reverse(T,Tmp), my_append(Tmp,[H],R).

my_flatten([],[]).
my_flatten([H|T],R):- my_flatten(H,TmpH), 
my_flatten(T,TmpT),my_append(TmpH,TmpT,R).
my_flatten([H|T],[H|R]) :- H\=[], H\=[_|_],my_flatten(T,R).

my_member(X,[X|_]).
my_member(X,[_|L]):-my_member(X,L).

my_remove(X,[X|T],T).
my_remove(X,[H|T],[H|R]):- my_remove(X,T,R).

/* q2 */
member2(X,L):- my_remove(X,L,L1), my_remove(X,L1,L2), not(my_member(X,L2)).

/* q3 */
substring(X,Y):- my_append(_,Tmp,Y),my_append(X,_,Tmp).

/* q4 */
head(X,[H|T],[[X|H]|Y]):- head(X,T,Y).
head(_,[],[]).
sublists([],[[]]).
sublists([H|T],[H|S]):- sublists(T,S1),head(H,S1,S2),appen(S2,S1,S).

/* q5 */
my_permutation([],[]).
my_permutation([H|T],R):- my_permutation(T,W), my_remove(H,R,W).

/* q6 */
mother(alice,abby).
mother(alice,cassie).
mother(abby,beyonce).
mother(abby,selina).
mother(cassie,gina).
mother(cassie,maria).
/*sister = sibling only need to change the position*/
sister(X,Y):-mother(Z,X),X \= Y,mother(Z,Y).
cousin(X,Y):-mother(Z,X),mother(M,Y),sister(Z,M).
/*granddaughter = grandMother only need to change the position*/
granddaughter(X,Y):-mother(Z,X),mother(Y,Z). 
descendent(X,Y):-mother(Y,X).
descendent(X,Y):-mother(Z,X),descendent(Z,Y).

/* q7 */
edge(1,2).
edge(1,3).
edge(2,3).
edge(2,5).
edge(3,4).
edge(3,5).
edge(4,5).
path(X,Y):-edge(X,Y);edge(Y,X).
/*find the shortest path number in [Y]*/
path1(X,Y,[Y]) :- edge(X,Y).
path1(X,Y,[Z|T]) :- edge(X,Z),path1(Z,Y,T).

shortpath(X,Y,L):- path1(X,Y,P),list_length(P,K),L is K.

list_length([], 0 ).
list_length([_|Xs] , L ) :- list_length(Xs,N) , L is N+1 .

go(X,Y,P,[Y|P]):-connect1(X,Y).
go(X,Y,P,Q):-connect1(X,A), A =\= Y,\+ my_member(A,P),go(A,Y,[A|P],Q).






/* q9 */
josephus(N,M):- form_a_list(1,N,L), find_winner(L,M,Win),write('the winner is '),write(Win).

form_a_list(A,B,[]):- A > B.
form_a_list(A,B,L):- A =< B, TmpA is A+1, form_a_list(TmpA,B,TmpL),my_append([A],TmpL,L).

find_winner([Win],_,Win).
find_winner(L,M,Win):- cyclic(L,M,[_|TmpL]),find_winner(TmpL,M,Win).

cyclic([],_,[]).
cyclic([H|T],0,[H|T]).
cyclic([H|T],N,L):- N > 0,my_append(T,[H],Tmp), N1 is N - 1, cyclic(Tmp,N1,L).
