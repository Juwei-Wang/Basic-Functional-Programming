
onBoard(N) :- member(N, [
	2, 3, 4,
	12, 13, 14,
	20,21,22,23,24,25,26,
	30,31,32,33,34,35,36,
	40,41,42,43,44,45,46,
	52,53,54,
	62,63,64
]).


pagoda_list1([
	[13, 1],
	[31, 1],
	[33, 1],
	[35, 1],
	[53, 1]
]).

pagoda_list2([
	[13, 1],
	[20, -1],
	[21, 1],
	[23, 1],
	[25, 1],
	[26, -1],
	[31, 2],
	[33 ,2],
	[35, 2],
	[40, -1],
	[41, 1],
	[43, 1],
	[45, 1],
	[46, -1],
	[53, 1]
]).

pagoda_list3([
	[2, -1],
	[4, -1],
	[12, 1],
	[14, 1],
	[31, 1],
	[32, 1],
	[34, 1],
	[36, 1],
	[52, 1],
	[54, 1],
	[62, -1],
	[64, -1]
]).

pagoda_list4([
	[3, 21],
	[13, 13],
	[20, -8],
	[21, 8],
	[23, 8],
	[25, 8],
	[26, -8],
	[30, 5],
	[31, 5],
	[33, 5],
	[35, 5],
	[36, 5],
	[40, -3],
	[41, 3],
	[43, 3],
	[45, 3],
	[46, -3],
	[53, 2],
	[54, 3],
	[63, 1]
]).


pagoda_list5([
	[2, -1],
	[4, -1],
	[31, 1],
	[32, 1],
	[34, 1],
	[36, 1],
	[52, 1],
	[54, 1],
	[62, -1],
	[64, -1]
]).

filter_pagoda(K, [], []).
filter_pagoda(K, [[K, V] | Xs], Ys) :- filterList(K, Xs, Ys).
filter_pagoda(K, [[G, V] | Xs], [[G, V] | Ys]) :- filterList(K, Xs, Ys).

member_pa(K, [], 0).
member_pa(K, [[K, V] | Xs], V).
member_pa(K, [[Y, _] | Xs], V) :- K =\= Y, member_pa(K, Xs, V).

evalPagoda([], 0).
evalPagoda([[K, V] | Xs], Out) :- evalPagoda(Xs, Sum), Out is V + Sum.
 
pagoda(_, [], 0, _).
pagoda(PList, [X | Xs], Count, Target) :-
	member_pa(X, PList, V),
	pagoda(PList, Xs, NextCount, Target),
	Count is V + NextCount,
	member_pa(Target, PList, FinalValue),
	Count >= FinalValue.


pagoda_dec(Start, Jumped, End, L, X) :-
	member_pa(Start, L, StartValue),
	member_pa(Jumped, L, JumpedValue),
	member_pa(End, L, EndValue),
	filter_pagoda(Start, L, L1),
	filter_pagoda(Jumped, L1, L2),
	filter_pagoda(End, L2, L3),
	Result is StartValue + JumpedValue,
	append(L3, [
			[End, Result]
	], X),
	evalPagoda(X, V),
	member_pa(3, L, FinalValue),
	V >= FinalValue.




jump(Start, Jumped, End) :-
	onBoard(Start),
	onBoard(Jumped),
	onBoard(End),
	Jumped is Start+1,
	End is Start+2.
jump(Start, Jumped, End) :-
	onBoard(Start),
	onBoard(Jumped),
	onBoard(End),
	Jumped is Start-1,
	End is Start-2.
jump(Start, Jumped, End) :-
	onBoard(Start),
	onBoard(Jumped),
	onBoard(End),
	Jumped is Start+10,
	End is Start+20.
jump(Start, Jumped, End) :-
	onBoard(Start),
	onBoard(Jumped),
	onBoard(End),
	Jumped is Start-10,
	End is Start-20.

crossbow_board([31, 32, 34, 35, 41, 42, 43, 44, 45, 53]).
crossbow_target(3).

longbow_board([
	20,
	26,
	30,31,33,35,36,
	41,43,45,
	52,53,54,
	63
]).
longbow_target(3).

notdead_board([
	2,3,4,
	12,14,
	20,21,22,23,24,25,26,
	30,32, 35, 36,
	40,41,42,43,44,45,46,
	52,54,
	62,64
]).
notdead_target(33).

peg(crossbow) :-
	pagoda_list5(L),
	crossbow_board(Board),
	writeln(Board),
	fullBoard(FullBoard),
	crossbow_target(Target),
	empty_board(Board, FullBoard, Empty),
	writeln(Target),
	solve(X, Board, L, Empty, Target),
	writeln(X).

peg(longbow, X) :-
	pagoda_list1(L),
	longbow_board(Board),
	writeln(Board),
	fullBoard(FullBoard),
	longbow_target(Target),
	empty_board(Board, FullBoard, Empty),
	writeln(Target),
	solve(X, Board, L, Empty, Target, []),
	writeln(X).
peg(notdead) :-
	pagoda_list1(L),
	notdead_board(Board),
	writeln(Board),
	fullBoard(FullBoard),
	notdead_target(Target),
	empty_board(Board, FullBoard, Empty),
	writeln(Target),
	solve(X, Board, L, Empty, Target, []),
	writeln(X).


are_identical(X, Y) :-
    X == Y.
filterList(A, In, Out) :-
    exclude(are_identical(A), In, Out).

notmember(X, []).
notmember(X, [T | Ts]) :- X =\= T, notmember(X, Ts).

fullBoard([
	2, 3, 4,
	12, 13, 14,
	20,21,22,23,24,25,26,
	30,31,32,33,34,35,36,
	40,41,42,43,44,45,46,
	52,53,54,
	62,63,64
]).

empty_board([], Out, Out).
empty_board([X | Xs], Full, Out) :-
	filterList(X, Full, Rest),
	empty_board(Xs, Rest, Out).


big(Start, Jumped, End, PList) :-
	member_pa(Start, PList, SV),
	member_pa(Jumped, PList, JV),
	member_pa(End, PList, EV),
	SJV is SV + JV,
	SJV >= EV.


indeCheck(Start, Jumped, End, Hist) :-
	not(member([Start, Jumped, End], Hist)).

lexorder(Start, Jumped, End, [[PS, PJ, PE] | Mvs]) :-
	V is Start + Jumped + End,
	V1 is PS + PJ + PE,
	V > V1.


solve([], [Target], _, _, Target).
solve(X, Board, PList, Empty, Target) :-
	member(Start, Board),
	member(Jumped, Board),
	member(End, Empty),
	%big(Start, Jumped, End, PList),
	jump(Start, Jumped, End),
	append([Start, Jumped, End], Next, X),
	filterList(Start, Board, Rest0),
	filterList(Jumped, Rest0, Rest1),
	append([End], Rest1, NewBoard),
	fullBoard(Full),
	%pagoda(PList, NewBoard, _, Target),
	empty_board(NewBoard, Full, NewEmpty),
	solve(Next, NewBoard, PList, NewEmpty, Target).

	/**
	*filterList(Start, Board, Rest0),
	*filterList(Jumped, Rest0, Rest1),
	*solve(X, [End, Rest1]).
	*/


	



