
:-use_module(library(chr)).
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
        convex_hull(B, ConvexB) |
        no_intersecting_segments(ConvexA, ConvexB),
        outside_point_hull(ConvexA, ConvexB),
        outside_point_hull(ConvexB, ConvexA).

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


%%%%%%%%%%%%%%%%%%%%%%%
% GRAVEYARD
%%%%%%%%%%%%%%%%%%%%%%%

%inside_points_hull([P|[]], H, SIGNS) <=> 
%        inside_point_hull(P,H,SIGNS).

%inside_points_hull([P1|PR], H, SIGNS) <=>
%        inside_point_hull(P1, H, SIGNS2),
%        inside_points_hull(PR, H, SIGNS1),
%        append(SIGNS1, SIGNS2, SIGNS). 

%inside_point_hull(P,[P1,P2|[]], SIGN) <=>
%        point_line_direction(P, P1, P2, S),
%        SIGN = [S].

%inside_point_hull(P, [P1,P2|PR], SIGN) <=>
%        determinant(P, P1, P2, Det),
%        normalize_sign(Det, S),
%        point_line_direction(P, P1, P2, SIGN1),
%        inside_point_hull(P, [P2|PR], SIGN2),
%        append([SIGN1], SIGN2, SIGN).
%intersect_hull([P1,P2|[]], Z, POINTS) <=>
%        intersect_segment_hull(P1, P2, Z, POINTS).

%intersect_hull([P1,P2|PR], Z, POINTS) <=>
%        intersect_hull([P2|PR], Z, POINTS2),
%        intersect_segment_hull(P1, P2, Z, POINTS1),
%        append_clean(POINTS1, POINTS2, POINTS).

%intersect_segment_hull(P1, P2, [P3,P4|[]], POINTS) <=>
%        intersection_segment_segment([P1,P2],[P3,P4],POINTS). 

%intersect_segment_hull(P1, P2, [P3,P4|PR], POINTS) <=>
%        intersection_segment_segment([P1, P2], [P3, P4], POINTS1),
%        intersect_segment_hull(P1, P2 ,[P4|PR],POINTS2),
%        append_clean(POINTS1,POINTS2,POINTS).

