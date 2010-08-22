%:-use_module(library(chr)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% partial overlap for convex hulls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint partial_overlap/2, partial_overlap_obj/2, intersect_convexhull/2, intersect_segment_hull/3. 

partial_overlap(A, B) <=> 
        ground(A), 
        ground(B) | 
        partial_overlap_obj(A, B).

partial_overlap_obj(region(A), region(B)) <=>
        convex_hull(A, ConvexA),
        convex_hull(B, ConvexB),        
        intersect_convexhull(ConvexA, ConvexB).

partial_overlap_obj(line(A), region(B)) <=>
        convex_hull(B, ConvexB),        
        intersect_convexhull(A, ConvexB).

partial_overlap_obj((_,_), polygon(_)) <=>
        fail.

intersect_convexhull([P1,P2|[]], Z) <=> 
        (intersect_segment_hull(P1,P2,Z) -> true; fail).

intersect_convexhull([P1,P2|PR], Z) <=>
        (intersect_segment_hull(P1,P2,Z) -> 
         true ;
         intersect_convexhull([P2|PR], Z)).

intersect_segment_hull(P1,P2,[P3,P4|[]]) <=> 
        (intersection_segment_segment([P1,P2],[P3,P4],[]) -> 
         fail; true).

intersect_segment_hull(P1,P2,[P3,P4|PR]) <=>
        (intersection_segment_segment([P1,P2],[P3,P4], []) -> 
          intersect_segment_hull(P1,P2,[P4|PR]) ; 
          true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% disconnect for convex hulls
% two hulls are disconnected if there are no segment intersections
% and are not ntpp or ntppi
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint disconnect/2, disconnect_obj/2, determinant/4, no_intersecting_segments/2, outside_point_hull/2.

disconnect(A, B) <=>
        ground(A),
        ground(B) |
        disconnect_obj(A, B).

disconnect_obj(region(A), region(B)) <=>
        convex_hull(A, ConvexA),
        convex_hull(B, ConvexB)| 
        no_intersecting_segments(ConvexA, ConvexB),
        outside_point_hull(ConvexA, ConvexB),
        outside_point_hull(ConvexB, ConvexA).

disconnect_obj(line([(X,Y),(U,V)]), region(A)) <=>
	convex_hull(A, ConvexA),
	no_intersecting_segments([(X,Y),(U,V)],ConvexA).

outside_point_hull([PT|_], Z) <=>
        (inside_point_hull(PT, Z) ->fail;true).

no_intersecting_segments([P1,P2|[]], Z) <=> 
    (intersect_segment_hull(P1,P2,Z) -> fail;true).

no_intersecting_segments([P1,P2|PR], Z) <=>
    (intersect_segment_hull(P1,P2,Z) -> fail ;
    no_intersecting_segments([P2|PR], Z)).


print_pt((X,Y)) :-
    print('('),print(X),print(','),print(Y),print(') ').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% topology: inside convex hulls
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- chr_constraint ntpp/2, inside_convexhull/2, inside_point_hull/2, inside_point_hull/3,  normalize_sign/2.

ntpp(A, B) <=>
        ground(A),
        ground(B) |
        inside_convexhull(A, B).

inside_convexhull(region(A), region(B)) <=>
        convex_hull(A, ConvexA),
        convex_hull(B, ConvexB),
        inside_convexhull(ConvexA, ConvexB).

inside_convexhull([],_) <=> true.

inside_convexhull([PT|PR], Convexhull) <=>
        inside_point_hull(PT, Convexhull),
        inside_convexhull(PR, Convexhull).

% assumes the convex hull is ordered clockwise. A point is inside the hull
% if each point has a negative determinant with every segment in hull
inside_point_hull(PT1,[PT2,PT3|[]]) <=> 
        determinant(PT1, PT2, PT3, D), 
        D < 0.

inside_point_hull(PT1, [PT2,PT3|PR]) <=>
        determinant(PT1, PT2, PT3, D),
        D < 0,
        inside_point_hull(PT1, [PT3|PR]). 

determinant((X1, Y1), (X2, Y2), (X3, Y3), D) <=>
        D is ((Y1-Y2)*(X3-X2)) - ((X1-X2)*(Y3-Y2)).

normalize_sign(S_old, S_normal) <=> 
        (S_old < 0 -> S_normal = -1 ; S_normal = 1).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% outside convex hull
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint ntppi/2.

ntppi(A, B) <=>
        ground(A),
        ground(B) |
        ntpp(B, A).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERSECTION SEGMENT SEGMENT (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint intersection_segment_segment/3.

intersection_segment_segment1 @ 
			intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
			inside_point_segment((A,B),(E,F),(G,H)),
			inside_point_segment((C,D),(E,F),(G,H))
			|  
			(eq2((A,B),(C,D))->POINT=[(A,B)];POINT=[line([(A,B),(C,D)])]).

%----- (E,F)---(A,B)--(C,D)---(G,H)----

intersection_segment_segment2 @ 
			intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
			inside_point_segment((E,F),(A,B),(C,D)),
			inside_point_segment((G,H),(A,B),(C,D))
			|
			(eq2((E,F),(G,H))->POINT=[(E,F)];POINT=[line([(E,F),(G,H)])]). 

%----- (A,B)---(E,F)--(G,H)---(C,D)----

intersection_segment_segment3 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		inside_point_segment((A,B),(E,F),(G,H)),
		inside_point_segment((E,F),(C,D),(A,B)) 
		|
		(eq2((E,F),(A,B))->POINT=[(E,F)];POINT=[line([(E,F),(A,B)])]).


%-----(C,D)--- (E,F)---(A,B)----(G,H)----

intersection_segment_segment4 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		inside_point_segment((C,D),(E,F),(G,H)),
		inside_point_segment((E,F),(A,B),(C,D))
		|
		(eq2((E,F),(C,D))->POINT=[(E,F)];POINT=[line([(E,F),(C,D)])]).

%-----(A,B)---(E,F)---(C,D)---(G,H)----

intersection_segment_segment5 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		inside_point_segment((A,B),(E,F),(G,H)),
		inside_point_segment((G,H),(A,B),(C,D)) 
		|
		(eq2((A,B),(G,H))->POINT=[(A,B)];POINT=[line([(A,B),(G,H)])]).


%-----(E,F)--- (A,B)---(G,H)----(C,D)----


intersection_segment_segment6 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		inside_point_segment((C,D),(E,F),(G,H)),
		inside_point_segment((G,H),(C,D),(A,B)) 
		|
		(eq2((C,D),(G,H))->POINT=[(C,D)];POINT=[line([(C,D),(G,H)])]).


%-----(E,F)--- (C,D)---(G,H)----(A,B)----

 				 
intersection_segment_segment7 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		neqn(D,B),neqn(H,F),neqn(A,C),neqn(E,G),
		SLOPE_1 is (C-A)/(D-B), SLOPE_2 is (G-E)/(H-F), 	 		
		neqn(SLOPE_1,SLOPE_2),
      		Y is ((-SLOPE_1*D)+(SLOPE_2*H)-G+C)/(SLOPE_2-SLOPE_1),
		X is (SLOPE_1*(Y-D))+C,    
		in_segment((X,Y),(E,F),(G,H)),in_segment((X,Y),(A,B),(C,D))  
		|
		POINT=[(X,Y)].

%--A=/C, B=/D, E=/G, F=/H

intersection_segment_segment8 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		eqn(D,B),neqn(H,F),neqn(G,E),neqn(C,A), 
		SLOPE_2 is (G-E)/(H-F), 
		X is ((D-F)*SLOPE_2)+E,  
		Y is D,
		in_segment((X,Y),(E,F),(G,H)),in_segment((X,Y),(A,B),(C,D))  
		|
		POINT=[(X,Y)].
				
%--D=B,A=/C,E=/G,F=/H

intersection_segment9 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		eqn(D,B),neqn(H,F),eqn(G,E),neqn(A,C), 
		X is G,
		Y is D,
		in_segment((X,Y),(E,F),(G,H)),in_segment((X,Y),(A,B),(C,D))  
		|
		POINT=[(X,Y)].
				
%--D=B,G=E,H=/F,A=/C

intersection_segment_segment10 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		neqn(D,B),neqn(H,F),eqn(G,E),neqn(A,C), 
		X is G,
		SLOPE_1 is (C-A)/(D-B), 
		Y is ((X-C)/SLOPE_1)+D,
		in_segment((X,Y),(E,F),(G,H)),in_segment((X,Y),(A,B),(C,D))   
		|
		POINT=[(X,Y)].
				
%--G=E,D/=B,H=/F,A=/C

intersection_segment_segment11 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		eqn(H,F),neqn(B,D),neqn(C,A),neqn(E,G),  
		SLOPE_1 is (C-A)/(D-B), 
		X is ((H-B)*SLOPE_1)+A, 
		Y is H,
		in_segment((X,Y),(E,F),(G,H)),in_segment((X,Y),(A,B),(C,D))  
		|
		POINT=[(X,Y)].

%--H=F,B=/D,C=/A,E=/G

intersection_segment_segment12 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		eqn(H,F),neqn(B,D),eqn(C,A),neqn(E,G), 
		X is C,
		Y is H,
		in_segment((X,Y),(E,F),(G,H)),in_segment((X,Y),(A,B),(C,D))  
		|
		POINT=[(X,Y)].
			
%--H=F,C=A,D/=B,E=/G

intersection_segment_segment13 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		neqn(H,F),neqn(B,D),eqn(C,A),neqn(E,G), 
		X is C,
		SLOPE_1 is (H-F)/(G-E), 
		Y is ((X-G)/SLOPE_1)+H,
		in_segment((X,Y),(E,F),(G,H)),in_segment((X,Y),(A,B),(C,D))   			 			
		|
		POINT=[(X,Y)].
			
%--C=A,H/=F,D/=B,E=/G


intersection_segment_segment14 @ 
		intersection_segment_segment([(_,_),(_,_)],[(_,_),(_,_)],POINT) <=>
		POINT=[].	


%%%%%%%%%%%%%%%%%%%%%%%%%
% IN SEGMENT (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint in_segment/3, in_segment2/3.

in_segment((X,Y),(A,B),(C,D)) <=>  in_segment2((X,Y),(A,B),(C,D)) | true.

in_segment((X,Y),(A,B),(C,D)) <=>  in_segment2((X,Y),(C,D),(A,B)) | true.

in_segment((X,Y),(A,B),(C,D)) <=>  in_segment2((X,Y),(A,D),(C,B)) | true.

in_segment((X,Y),(A,B),(C,D)) <=>  in_segment2((X,Y),(C,B),(A,D)) | true.

in_segment((_,_),(_,_),(_,_)) <=>  fail.

%%%%%%%%%%%%%%%%%%%%%%%%%
% IN SEGMENT (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%

in_segment2((X,Y),(A,B),(C,D)) <=> leq(A,X),leq(X,C),leq(B,Y),leq(Y,D) | true.

in_segment2((_,_),(_,_),(_,_)) <=> fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSIDE POINT SEGMENT (SET)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint inside_point_segment/3.

inside_point_segment1 @	inside_point_segment((X,Y),(Z,T),(U,V)) <=> 
					VD1 is Z-U, VD2 is T-V, neqn(VD1,0), neqn(VD2,0), 
					D1 is X-Z, D2 is Y-T, 
					leq(Z,X), leq(X,U), leq(T,Y), leq(Y,V)|
					VD1*D2 =:= VD2*D1.

inside_point_segment2 @ inside_point_segment((X,Y),(Z,T),(U,V)) <=> 
					VD1 is Z-U, VD2 is T-V, neqn(VD1,0), neqn(VD2,0), 
					D1 is X-Z, D2 is Y-T, 
					leq(U,X), leq(X,Z), leq(T,Y), leq(Y,V) |
					VD1*D2 =:= VD2*D1.
inside_point_segment3 @ inside_point_segment((X,Y),(Z,T),(U,V)) <=> 
					VD1 is Z-U, VD2 is T-V, neqn(VD1,0), neqn(VD2,0), 
					D1 is X-Z, D2 is Y-T, 
					leq(Z,X), leq(X,U), leq(V,Y), leq(Y,T)|
					D1*D2 =:= VD2*D1.
inside_point_segment4 @ inside_point_segment((X,Y),(Z,T),(U,V)) <=> 
					VD1 is Z-U, VD2 is T-V, neqn(VD1,0), neqn(VD2,0), 
					D1 is X-Z, D2 is Y-T,					
					leq(U,X), leq(X,Z), leq(V,Y), leq(Y,T) |
					VD1*D2 =:= VD2*D1.
inside_point_segment5 @ inside_point_segment((X,Y),(Z,T),(U,V)) <=> VD1 is Z-U,  
			eqn(VD1,0), leq(T,Y), leq(Y,V) |	
			X=:=Z.

inside_point_segment6 @ inside_point_segment((X,Y),(Z,T),(U,V)) <=> VD1 is Z-U, 
			eqn(VD1,0), leq(V,Y), leq(Y,T)  | 
			X=:=Z.

inside_point_segment7 @ inside_point_segment((X,Y),(Z,T),(U,V)) <=> VD2 is T-V,   
			eqn(VD2,0), leq(Z,X), leq(X,U) |
			Y=:=T.

inside_point_segment8 @ inside_point_segment((X,Y),(Z,T),(U,V)) <=> VD2 is T-V,  
			eqn(VD2,0), leq(U,X), leq(X,Z) |
			Y=:=T.

inside_point_segment9 @ inside_point_segment((_,_),(_,_),(_,_)) <=> fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LEQ (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint leq/2, lt/2, geq/2, gt/2.

leq1 @ leq(X,Y) <=>  number(X), number(Y),X=<Y | true.

leq2 @ leq(X,Y) <=>  number(X), number(Y),Y<X | fail.

leq3 @ leq(X,Y) <=> eqn(X,Y) | true.

leq4 @ leq(X,Y), leq(Y,X) <=>  eqn(X,Y).

leq5 @ leq(X,Y), leq(Y,Z) ==> leq(X,Z).

leq6 @ leq(X,Y), leq(X,Z) <=> number(Y),number(Z),Y=<Z | leq(X,Y).

leq7 @ leq(Y,X), leq(Z,X) <=> number(Y),number(Z),Y=<Z | leq(Z,X).

leq8 @ lt(Z,X), leq(X,Y) <=> number(Y),number(Z),Y=<Z | fail.

leq9 @ leq(Z,X), lt(X,Y) <=> number(Y),number(Z),Y=<Z | fail.

leq10 @ lt(Z,X), lt(X,Y) <=> number(Y),number(Z),Y=<Z | fail.

leq11 @ leq(Z,X), leq(X,Y) <=> number(Y),number(Z),Y<Z | fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LT (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lt1 @ lt(X,Y) <=>  number(X), number(Y), X<Y | true.

lt2 @ lt(X,Y) <=>  number(X), number(Y), Y=<X | fail.

lt3 @ lt(X,Y) <=>  eqn(X,Y) | fail.

lt4 @ lt(X,Y), lt(X,Z) <=> number(Y),number(Z),Y=<Z | lt(X,Y).

lt5 @ lt(Y,X), lt(Z,X) <=> number(Y),number(Z),Y=<Z | lt(Z,X).
lt6 @ lt(X,Y), lt(Y,X) <=> fail.

lt7 @ lt(X,Y), lt(Y,Z) ==> lt(X,Z).

lt8 @ lt(X,Y), leq(Y,Z) ==> lt(X,Z).

lt9 @ lt(X,Y) ==>  neqn(X,Y).

lt10 @ leq(X,Y), lt(X,Z) <=> number(Y),number(Z),Y=<Z | leq(X,Y).

lt11 @ leq(Y,X), lt(Z,X) <=> number(Y),number(Z),Y=<Z | lt(Z,X).

lt12 @ lt(X,Y), leq(X,Z) <=> number(Y),number(Z),Y=<Z | lt(X,Y).

lt13 @ lt(Y,X), leq(Z,X) <=> number(Y),number(Z),Y=<Z | leq(Z,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GEQ (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
geq @ geq(X,Y) <=> leq(Y,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GT (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gt @ gt(X,Y) <=> lt(Y,X).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EQUALITY CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint eq/2,eqp/2,eqobj/2,neq/2,neqp/2,neqobj/2,eqlist/2,eqobjlist/2.
:- chr_constraint eqn/2,neqn/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EQ (EQUALITY)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eq1 @ eq(O1,O2), neq(O1,O2) <=> fail.
eq2 @ eq(O1,O2) <=> eqobj(O1,O2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NEQ (EQUALITY)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
neq @ neq(O1,O2) <=> ground(O1),ground(O2) | neqobj(O1,O2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EQ2, NEQ2 LOGIC (EQUALITY)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eq2(O1,O2):-eqobj(O1,O2),!.
neq2(O1,O2):-neqobj(O1,O2),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EQP (EQUALITY)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eqp1 @ eqp(P1,P2),neqp(P1,P2) <=> fail.
eqp2 @ eqp((O1,O2),(O3,O4)) <=> 
				eq(O1,O3), 
				eq(O2,O4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NEQP (EQUALITY)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


neqp @ neqp((O1,O2),(O3,O4)) <=> 
				neq(O1,O3); 
				neq(O2,O4).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EQOBJ (EQUALITY) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eqobj1 @ eqobj(O1,O2), neqobj(O1,O2) <=> fail.
eqobj2 @ eqobj(X,O)  <=> var(X) | X=O.
eqobj3 @ eqobj(O,X)  <=> var(X) | X=O.
eqobj4 @ eqobj((X,Y),(Z,T))<=> eqn(X,Z),eqn(Y,T).
eqobj5 @ eqobj(line(L1),line(L2)) <=> L1=L2.
eqobj6 @ eqobj(polygon(L1),polygon(L2)) <=> L1=L2.
eqobj7 @ eqobj(region(L1),region(L2)) <=> L1=L2.
eqobj8 @ eqobj(diffo(O1,O2),diffo(O3,O4)) <=> O1=O3,O2=O4.
eqobj9 @ eqobj(_,_) <=> fail.

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NEQOBJ (EQUALITY)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

neqobj @ neqobj(O1,O2)<=> (eqobj(O1,O2)->fail;true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EQN (EQUALITY)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eqn1 @ eqn(N1,N2), neqn(N1,N2) <=> fail.
eqn2 @ eqn(N1,N2) <=> ground(N1),ground(N2), float(N1)=:=float(N2) | true.
eqn3 @ eqn(N1,N2) <=> N1=N2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NEQN (EQUALITY)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
neqn @ neqn(N1,N2) <=> (eqn(N1,N2)->fail;true).


