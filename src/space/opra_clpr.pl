:-use_module(library(clpr)).

opra(PtA, Dir, PtB, M, I) :-
    %print(PtA),nl,print(Dir),nl,print(PtB),nl,nl,
    compute_opra(PtA,Dir, PtB, M, I).

%opra((PtA, OA), (PtB, OB), M, I, J) :-
%    compute_opra((PtA, OA), PtB, M, I),
%    compute_opra((PtB, OB), PtA, M, J).

compute_opra(PtA,OrA,PtB,M,I) :-
    angle(PtA,PtB,AB),
    angle(PtA,OrA,OA),
    (AB < OA ->
	X is AB - OA + (2 * 3.14) ;
	X is AB - OA),
    odd(I),
    {X > ((2*3.14) * ((I - 1)/(M*4))),
     X < ((2*3.14) * ((I + 1)/(M*4)))}.

compute_opra(PtA,OrA,PtB,M,I) :-
    angle(PtA,PtB,AB),
    angle(PtA,OrA,OA),
    (AB < OA ->
	X is AB - OA + (2 * 3.14) ;
	X is AB - OA),
    even(I),
    {X = ((2*3.14) * (I/(M*4)))}.

angle((Ax,Ay),(Bx,By),AB) :-
    map_twopi(Ax,Ay,Bx,By, 
    atan((By-Ay)/(Bx-Ax)),AB).

map_twopi(Ax,Ay,Bx,By,X,X2) :-
    By > Ay, Bx < Ax,
    X2 is pi + X, !.

map_twopi(Ax,Ay,Bx,By,_,X2) :-
    By = Ay, Bx < Ax,
    X2 is pi.

map_twopi(Ax,Ay,Bx,By,_,X2) :-
    By > Ay, Ax = Bx,
    X2 is pi/2.

map_twopi(Ax,Ay,Bx,By,_,X2) :-
    By = Ay, Bx > Ax,
    X2 = pi.

map_twopi(Ax,Ay,Bx,By,_,X2) :-
    By < Ay, Ax = Bx,
    X2 = (3*pi)/2.

map_twopi(Ax,Ay,Bx,By,X,X2) :-
    By < Ay, Bx < Ax,
    X2 is pi + abs(X), !.

map_twopi(Ax,Ay,Bx,By,X,X2) :-
    By < Ay, Bx > Ax,
    X2 is 2*pi + X, !.

map_twopi(Ax,Ay,Bx,By,X,X2) :-
    By > Ay, Bx > Ax,
    X2 is X, !.

odd(3).
odd(1).
odd(5).
odd(7).
odd(9).
odd(11).
odd(13).
odd(15).
odd(17).
odd(19).
odd(21).
odd(23).
odd(25).
odd(27).
odd(29).
odd(31).
odd(33).
even(0).
even(2).
even(4).
even(6).
even(8).
even(10).
even(12).
even(14).
even(16).
even(18).
even(20).
even(22).
even(24).
even(26).
even(28).
even(30).
even(32).
even(34).
even(36).


atan(X, V) :-
    V is atan(X).


