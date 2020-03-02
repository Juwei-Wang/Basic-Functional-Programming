/*测定棋盘范围和跳跃规则*/
/*测试OK*/
onboard(N):- 2=< N,N=<4.
onboard(N):- 12=< N,N=<14.
onboard(N):- 20=< N,N=<26.
onboard(N):- 30=< N,N=<36.
onboard(N):- 40=< N,N=<46.
onboard(N):- 52=< N,N=<54.
onboard(N):- 62=< N,N=<64.

jump(Start, Jumped, End) :-
	Jumped is Start+1,
	End is Start+2,
    onboard(Start),
	onboard(Jumped),
	onboard(End).

jump(Start, Jumped, End) :-
	Jumped is Start-1,
	End is Start-2,
    onboard(Start),
	onboard(Jumped),
	onboard(End).

jump(Start, Jumped, End) :-
	Jumped is Start+10,
	End is Start+20,
    onboard(Start),
	onboard(Jumped),
	onboard(End).

jump(Start, Jumped, End) :-
	Jumped is Start-10,
	End is Start-20,
    onboard(Start),
	onboard(Jumped),
	onboard(End).

game(crossbow,[31, 32, 34, 35, 41, 42, 43, 44, 45, 53],3).
game(longbow,[2,3,4,12,14,20,21,22,23,24,25,26,30,32, 35,36,40,41,42,43,44,45,46,52,54,62,64],[3]).
game(fullBoard,[2,3,4,12,13,14,20,21,22,23,24,25,26,30,31,32,33,34,35,36,40,41,42,43,44,45,46,52,53,54,62,63,64],[33]).



/*给予一个起始棋盘，然后删除掉开始和中间位置，获得（[End,SB2]）*/
/*问题solitaire_move([20,21,24,62],(20,22),L). yes！*/
/*测试tesk OK*/
solitaire_move(SB,(Start,End),(End|SB2)):-
delete(SB,Start,SB1),
jump(Start,Jumped,End),
delete(SB1,Jumped,SB2),
not(my_member(End,SB2)).

/*to check weather the moves are independent*/
intersectionTR(_, [], []).
intersectionTR([], _, []).
intersectionTR([H1|T1], L2, [H1|L]):-
    member(H1, L2),
    intersectionTR(T1, L2, L), !.
intersectionTR([_|T1], L2, L):-
    intersectionTR(T1, L2, L).

/*OK overlap([32,34],[43,23]).*/
/*测试tesk OK*/
overlap((S1,E1),(S2,E2)):-
    jump(S1,J1,E1),
    jump(S2,J2,E2),
    (S1==S2;S1==J2;S1==E2;J1==S2;J1==J2;J1==E2;E1=S2;E1==J2;E1==E2).

/*OK lexorder((20,22),(33,13)). = True*/
lexorder((A,_),(C,_)):- A>C.
/*OK independent([3],[]).*/
independent(_,[]).
/*OK independent([4],[[5,4]])..*/
independent(M,[H|_]):- overlap(M,H),!.

/*OK independent([32,34],[[43,23]]).*/
/*OKindependent([45,43],[[34,36]]).*/
/*independent((45,43),[(34,36)]).*/
/*错误，既没有交集，前面的也大于后面的*/
/*正确，只有有一个有交集 或者 前面的小于后面的*/
/*测试tesk OK*/
independent(M,[H|T]):-lexorder(M,H),independent(M,T).



/*findall((P,W)),(member(P,[simple])),wgt(P,NB,W),Wgts)*/
/*测试tesk OK*/
gw(crossbow,basic,1).
gw(crossbow,stronger,2).
gw(crossbow,top,0).

gw(longbow,fpart,21).

gw(fullBoard,basic,1).
gw(fullBoard,stronger,2).
gw(fullBoard,top,0).


/*计算棋盘的weight*/
/*wgt(simple,[33,23,24,25],P).*/
/*测试tesk OK*/
wgt(+,+,-).
wgt(_,[],0).
wgt(Pagoda,[Pos|Poses],Weight):-
    (pagoda(Pagoda, Pos, PWgt); PWgt = 0),
    !,
    wgt(Pagoda, Poses, Weight2),
    Weight is PWgt + Weight2.

/*计算棋盘根据pagoda的weight不能低于目标位置*/
/*测试tesk OK*/
check_weights(_, []).
check_weights(Goalboard, [(P, WgtP)|Rest]):-
    gw(Goalboard,P,GoalWeight),
    WgtP >= GoalWeight,
    check_weights(Goalboard, Rest).


/*pagoda(Type,Position,Weight)*/
pogoda(top,3,21).
pogoda(top,13,13).
pogoda(top,23,8).
pogoda(top,33,5).
pogoda(top,43,3).
pogoda(top,53,2).
pogoda(top,63,1).
pogoda(top,54,3).
pogoda(top,20,-8).
pogoda(top,21,8).
pogoda(top,30,5).
pogoda(top,31,5).
pogoda(top,40,-3).
pogoda(top,41,3).
pogoda(top,25,8).
pogoda(top,26,-8).
pogoda(top,35,5).
pogoda(top,36,5).
pogoda(top,45,3).
pogoda(top,46,-3).

pogoda(basic,13,1).
pogoda(basic,31,1).
pogoda(basic,33,1).
pogoda(basic,35,1).
pogoda(basic,53,1).

pogoda(stronger,13,1).
pogoda(stronger,20,-1).
pogoda(stronger,21,1).
pogoda(stronger,23,1).
pogoda(stronger,25,1).
pogoda(stronger,26,-1).
pogoda(stronger,31,2).
pogoda(stronger,33,2).
pogoda(stronger,35,2).
pogoda(stronger,40,-1).
pogoda(stronger,41,1).
pogoda(stronger,43,1).
pogoda(stronger,45,1).
pogoda(stronger,46,-1).
pogoda(stronger,53,0).

pagoda(tpart,2,-1).
pagoda(tpart,4,-1).
pagoda(tpart,12,1).
pagoda(tpart,14,1).
pagoda(tpart,31,1).
pagoda(tpart,32,1).
pagoda(tpart,34,1).
pagoda(tpart,36,1).
pagoda(tpart,52,1).
pagoda(tpart,54,1).
pagoda(tpart,62,-1).
pagoda(tpart,64,-1).


/*测试*/
final_board(G,B):-game(G,_,B).
solitaire_steps(G,B,_,[]):- final_board(G,B).

solitaire_steps(G,B,Hist,[MV|Moves]) :-
    make_jump(B,Start, _, End, NewBoard),
    /*solitaire_move(B,(Start,End),(End|SB2)),*/
    MV = (Start,End),
    independent(Mv,Hist),
    findall((P,W),(member(P,[basic,stronger,tpart,top]),wgt(P,NewBoard,W)),Wgts),
    check_weights(G,Wgts),
    solitaire_steps(G,NewBoard,[Mv|Hist], Moves).


/*测试tesk OK*/
make_jump(B,Start,Jumped,END,NewBoard):-
member(Start,B),
delete(B,Start,Y),
jump(Start,Jumped,END),
member(Jumped,B),
not(member(END,B)),
delete(Y,Jumped,K),
pushFront(END,K,NewBoard).
pushFront(Item, List, [Item|List]).

/*测试tesk OK*/
peg(crossbow):-
game(crossbow,K,G),
writeln(K),
writeln(G),
solitaire_steps(crossbow,k,[],Solution),
writeln(Solution).

peg(longbow):-
game(longbow,K,G),
writeln(K),
writeln(G),
solitaire_steps(longbow,k,[],Solution),
writeln(Solution).


peg(fullBoard):-
game(fullBoard,K,G),
writeln(K),
writeln(G),
solitaire_steps(fullBoard,k,[],Solution),
writeln(Solution).