
/* divide_and_conquer(Points, ConvexHullVertices) is true if Points is a     */
/*   list of points in the form (X,Y), and ConvexHullVertices are the       */
/*   vertices in the form (X,Y) of the convex hull of the Points, in        */
/*   clockwise order, starting and ending at the smallest point (as          */
/*   determined by X-values, and by Y-values to resolve ties). No three      */
/*   vertices of the convex hull will be collinear.                          */
/* e.g.divide_and_conquer([(0,6),(3,7),(4,6),(4,5),(3,4),(2,4),(5,0)],*/
/*                        [(0,6),(3,7),(4,6),(5,0),(0,6)]).             */
divide_and_conquer(Ps, Qs):-
  sort(Ps, Ps1), % Duplicated points are discarded
  length(Ps1, N),
  divide_and_conquer_1(N, Ps1, Qs, []).

divide_and_conquer_1(1, [X|Ps], [X,X], Ps):-!.
divide_and_conquer_1(2, [X,Y|Ps], [X,Y,X], Ps):-!.
divide_and_conquer_1(3, [X,Y,Z|Ps], [X,Y,Z,X], Ps):-
  strictly_to_right(Z, X, Y), !.
divide_and_conquer_1(3, [X,Y,Z|Ps], [X,Z,Y,X], Ps):-
  strictly_to_left(Z, X, Y), !.
divide_and_conquer_1(3, [X,_,Z|Ps], [X,Z,X], Ps):-!.
divide_and_conquer_1(N, Ps, Qs, Rs):-
  N1 is N // 2,
  N2 is N - N1,
  divide_and_conquer_1(N1, Ps, Left, Temp),
  divide_and_conquer_1(N2, Temp, Right, Rs),
  union(Left, Right, Qs).

/* length(Xs, L) is true if L is the number of elements in the list Xs.      */
%length(Xs, L):-length_1(Xs, 0, L).

/* length_1(Xs, L0, L) is true if L is equal to L0 plus the number of        */
/*   elements in the list Xs.                                                */
%length_1([_|Xs], L0, L):-L1 is L0 + 1, length_1(Xs, L1, L).
%length_1([], L, L).

/* strictly_to_left(Pa, Pb, Pc) is true if Pa is strictly to the left of the */
/*   directed line from Pb to Pc.                                            */
strictly_to_left((Xa,Ya), (Xb,Yb), (Xc,Yc)):-
  (Xb-Xa) * (Yc-Ya) - (Xc-Xa) * (Yb-Ya) > 0.0.

/* strictly_to_right(Pa, Pb, Pc) is true if Pa is strictly to the right of   */
/*   the directed line from Pb to Pc.                                        */
strictly_to_right((Xa,Ya), (Xb,Yb), (Xc,Yc)):-
  (Xb-Xa) * (Yc-Ya) - (Xc-Xa) * (Yb-Ya) < 0.0.

/* are_collinear(Pa, Pb, Pc) is true if Pa, Pb and Pc are collinear.         */
are_collinear((Xa,Ya), (Xb,Yb), (Xc,Yc)):-
  (Xb-Xa) * (Yc-Ya) - (Xc-Xa) * (Yb-Ya) =:= 0.0.

/* is_nearer(Pa, Pb, Pc) is true if the distance from Pa to Pc is strictly   */
/*   less than the distance from Pb to Pc.                                   */
is_nearer((Xa,Ya), (Xb,Yb), (Xc,Yc)):-
  Xa_Xc is Xa - Xc,
  Ya_Yc is Ya - Yc,
  Xb_Xc is Xb - Xc,
  Yb_Yc is Yb - Yc,
  (Xa_Xc)*(Xa_Xc) + (Ya_Yc)*(Ya_Yc) < (Xb_Xc)*(Xb_Xc) + (Yb_Yc)*(Yb_Yc).

/*
 * Union of disjoint convex hulls
 */

/* Given P1, find Q2, the first point in Qs s.t. P1-Q2 is a tangent for Qs.  */
/*   If P2 is strictly to the right of P1-Q2, return P1-Q2.                  */
/* Given Q2, find P2, the first point in Ps s.t. P2-Q2 is a tangent for Ps.  */
/*   If Q3 is strictly to the right of P2-Q2, return P2-Q2.                  */

/* union(Ps, Qs, Rs) is true if Ps and Qs are disjoint convex hulls, all     */
/*   vertices of Ps being to the left of all the vertices of Qs, and Rs is   */
/*   the convex hull of the union of the vertices of Ps and Qs.  For each    */
/*   convex hull, the vertices are in the form (X,Y), in clockwise order,   */
/*   starting and ending at the smallest point (as determined by X-values,   */
/*   and by Y-values to resolve ties), and no three vertices are collinear.  */
/* This is a O(n) time algorithm.                                            */
/* This may not work for some degenerate convex hulls, but should work in    */
/*   all cases when called by divide_and_conquer_1/4.                        */
/* e.g. union([(0,1),(1,3),(2,3),(3,0),(0,1)],                          */
/*            [(4,0),(4,1),(5,2),(7,2),(4,0)],                          */
/*            [(0,1),(1,3),(2,3),(7,2),(4,0),(3,0),(0,1)]).           */
union(Ps, Qs, Rs):-
  split_left_hull(Ps, [], Ps1, Ps2),
  split_right_hull(Qs, Qs2),
  bridge(Ps1, Qs, A, B, Qs3),
  bridge(Qs2, Ps2, C, D, Ps3),
  concatenate([B|Qs3], C, [D|Ps3], Rs0),
  concatenate(Ps, A, Rs0, Rs).

/* split_left_hull(Ps, [], Xs, Ys) is true if Xs is the reverse of the       */
/*   points in Ps up to and including the rightmost point, and Ys are the    */
/*   remaining points in Ps starting at the rightmost point.                 */
/* e.g. split_left_hull([(0,1),(1,3),(2,3),(3,0),(0,1)], [],            */
/*                      [(3,0),(2,3),(1,3),(0,1)], [(3,0),(0,1)]).     */
split_left_hull(Ps, Xs, [P1|Xs], Ps):- % For a one-point hull
  Ps=[P1,P2|_],
  P1=P2, !.
split_left_hull(Ps, Xs, [P1|Xs], Ps):-
  Ps=[P1,P2|_],
  lt(P2, P1), !.
split_left_hull([P1|Ps], Xs0, Xs, Ys):-
  split_left_hull(Ps, [P1|Xs0], Xs, Ys).

/* split_right_hull(Qs, Ys) is true if Ys is the reverse of the points in Qs */
/*   starting at the rightmost point.                                        */
/* e.g. split_right_hull([(4,0),(4,1),(5,2),(7,2),(4,0)],               */
/*                       [(4,0),(7,2)]).                                   */
split_right_hull(Qs, Ys):- % This clause is probably redundant
  Qs=[Q1,Q2|_],
  Q2=Q1, !,
  reverse(Qs, Ys).
split_right_hull(Qs, Ys):-
  Qs=[Q1,Q2|_],
  lt(Q2, Q1), !,
  reverse(Qs, Ys).
split_right_hull([_|Qs], Ys):-
  split_right_hull(Qs, Ys).

lt((X,_), (X1,_)):-X < X1, !.
lt((X,Y), (X,Y1)):-Y < Y1.

/* bridge(Ps, Qs, P, Q, Qs1) is true if Ps are the vertices of the left      */
/*   convex hull starting at the rightmost point going anti-clockwise to the */
/*   leftmost point, Qs are the vertices of the right convex hull starting   */
/*   and ending at the leftmost point going clockwise, the line from P (a    */
/*   point in Ps) to Q (a point in Qs) is a left tangent for the convex      */
/*   hulls, and Qs1 are the points following Q in Qs.                        */
/* e.g. bridge([(3,0),(2,3),(1,3),(0,1)],                                */
/*             [(4,0),(4,1),(5,2),(7,2),(4,0)],                         */
/*             (2,3), (7,2), [(4,0)]).                                    */
bridge(Ps, Qs, P, Q, Qs1):-
  Ps=[P1|_],
  tangent_to_right_hull(Qs, P1, Qs0),
  bridge_1(Ps, Qs0, P, Q, Qs1).

bridge_1([P], [Q|Qs], P, Q, Qs):-!.
bridge_1([P,P1|_], [Q|Qs], P, Q, Qs):-
  strictly_to_right(P1, P, Q), !.
bridge_1([_|Ps], Qs, P, Q, Qs1):-
  Qs=[Q1|_],
  tangent_to_left_hull(Ps, Q1, Ps1),
  bridge_2(Qs, Ps1, P, Q, Qs1).

bridge_2([Q], [P|_], P, Q, []):-!.
bridge_2([Q,Q1|Qs], [P|_], P, Q, [Q1|Qs]):-
  strictly_to_right(Q1, P, Q), !.
bridge_2([Q,Q1|Qs], [P|_], P, Q, [Q1|Qs]):- % For some 2-vertex Qs
  are_collinear(P, Q, Q1),
  is_nearer(Q1, Q, P), !.
bridge_2([_|Qs], Ps, P, Q, Qs1):-
  Ps=[P1|_],
  tangent_to_right_hull(Qs, P1, Qs2),
  bridge_1(Ps, Qs2, P, Q, Qs1).

/* tangent_to_right_hull(Qs, P, Qs1) is true if the head of Qs1 is the last  */
/*   element (and the tail of Qs1 are the remaining elements) of Qs (a       */
/*   convex chain going clockwise) such that the line from P to Q is a       */
/*   tangent for Qs.                                                         */
/* e.g. tangent_to_right_hull([(4,0),(4,1),(5,2),(7,2),(4,0)], (3,0),  */
/*                            [(5,2),(7,2),(4,0)]).                       */
tangent_to_right_hull([Q], _, [Q]):-!.
tangent_to_right_hull(Qs, P, Qs):-
  Qs=[Q,Q1|_],
  strictly_to_right(Q1, P, Q), !.
tangent_to_right_hull(Qs, P, Qs):- % For some 2-vertex Qs
  Qs=[Q,Q1|_],
  are_collinear(P, Q, Q1),
  is_nearer(Q1, Q, P), !.
tangent_to_right_hull([_|Qs], P, Qs1):-
  tangent_to_right_hull(Qs, P, Qs1).

/* tangent_to_left_hull(Ps, Q, Ps1) is true if the head of Ps1 is the first  */
/*   element (and the tail of Ps1 are the remaining elements) of Ps (a       */
/*   convex chain going anti-clockwise) such that the line from Q to P is a  */
/*   tangent for Ps.                                                         */
/* e.g. tangent_to_left_hull([(3,0),(2,3),(1,3),(0,1)], (5,2),          */
/*                           [(2,3),(1,3),(0,1)]).                        */
tangent_to_left_hull([P], _, [P]):-!.
tangent_to_left_hull(Ps, Q, Ps):-
  Ps=[P,P1|_],
  strictly_to_right(P1, P, Q), !.
tangent_to_left_hull([_|Ps], Q, Ps1):-
  tangent_to_left_hull(Ps, Q, Ps1).

/* concatenate(Ps, P, Qs0, Qs) is true if the list Qs consists of the        */
/*   elements of Ps up to and including the element P, followed by the       */
/*   elements of Qs0.                                                        */
/* e.g. The goal                                                             */
/*   concatenate([9,10,11,12,13], 11, [], Y),                                */
/*   concatenate([5,6,7,8,9], 7, [-8|Y], Z),                                 */
/*   concatenate([1,2,3,4,5], 3, [-4|Z], A).                                 */
/* gives Y=[9,10,11] Z=[5,6,7,-8,9,10,11] A=[1,2,3,-4,5,6,7,-8,9,10,11]      */
concatenate([P|_], P, Qs, [P|Qs]):-!.
concatenate([Q|Ps], P, Qs0, [Q|Qs]):-
  concatenate(Ps, P, Qs0, Qs).

/* reverse(Xs, Ys) is true if Ys is the result of reversing the order of     */
/*   the elements in the list Xs.                                            */
%reverse(Xs, Ys):-reverse_1(Xs, [], Ys).

%reverse_1([], As, As).
%reverse_1([X|Xs], As, Ys):-reverse_1(Xs, [X|As], Ys).

