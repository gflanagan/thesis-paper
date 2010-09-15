
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% architectual entities are represented in a multi
% perspective manner. eg. a door can be represented 
% directed point. The perspective/3 predicate transforms
% the physical geometry to the various spatial 
% abstraction based on the architectual type.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

point_transformation(Id,  Pt) :-
    ifc_geometry(Id, Geom), 
    abstract_to_point(Geom, Pt).

convex_hull_transformation(Id, Convex) :-
    ifc_geometry(Id, Geom),
    abstract_to_convex_hull(Geom, Convex).

% currently works for all types of arch entities
% direction must be hard coded
directed_point_transformation(Id, Pt, Dir) :-
    %( arch_entity(Id, dsDoor) ;
    %  arch_entity(Id, dsWindow) ;
    %  arch_entity(Id, dsWall) ),
    % ifc_geometry(Id, Geom),
    % abstract_to_point(Geom, Pt),
    direction_hard_code(Id, dir_pt(Pt, Dir)).

perspective(ID, fs_point, RmID, PT) :-
    functional_space(ID, RmID, FS),
    abstract_to_point(FS, PT).

abstract_to_point(region(R), Centroid) :-
    centroid(R, Centroid).

abstract_to_convex_hull(Geom, Convex) :-
    convex_hull(Geom, Convex).

% geometry utility functions

% calculates the centroid of a polygon. 
% NOTE: doesn't work if all coordinates are negative
centroid(region(R), C) :- !,
    abstract_to_convex_hull(R, ConvexR),
    polygon_area(ConvexR, A),
    centroid_xy_sum(ConvexR, SumX, SumY),
    my_abs(SumX, SumX_pos),
    my_abs(SumY, SumY_pos),
    mult_area(A, SumX_pos, SumY_pos, C).

centroid(R, C) :- !,
    abstract_to_convex_hull(R, ConvexR),
    polygon_area(ConvexR, A),
    centroid_xy_sum(ConvexR, SumX, SumY),
    my_abs(SumX, SumX_pos),
    my_abs(SumY, SumY_pos),
    mult_area(A, SumX_pos, SumY_pos, C).

my_abs(Neg, Pos) :-
    (Neg < 0 -> Pos is -Neg; Pos is Neg).

polygon_area(R, A) :-
    polygon_xy_sum(R, SumXY, SumYX),
    Sum is SumYX - SumXY,
    A is Sum / 2.

polygon_xy_sum([_|[]], 0, 0).

polygon_xy_sum([(X1,Y1),(X2,Y2)|PR], SumXY, SumYX) :-
    polygon_xy_sum([(X2,Y2)|PR], SumXY_prev, SumYX_prev),
    XY is X1 * Y2,
    YX is Y1 * X2,
    SumXY is XY + SumXY_prev,
    SumYX is YX + SumYX_prev. 

mult_area(A, SumX, SumY, (X,Y)) :-
    X is ((1/(6 * A)) * SumX),
    Y is ((1/(6 * A)) * SumY).

centroid_xy_sum([_|[]], 0, 0).

centroid_xy_sum([(X1,Y1),(X2,Y2)|PR], SumX, SumY) :-
    centroid_xy_sum([(X2,Y2)|PR], SumX_prev, SumY_prev),
    Dx is ((X1 * Y2) - (X2 * Y1)),
    Ux is (X1 + X2) * Dx,
    Uy is (Y1 + Y2) * Dx,
    SumX is Ux + SumX_prev,
    SumY is Uy + SumY_prev.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% geometry 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

spatial_primitive(Geometry, Pt1, Pt2) :-
    is_point(Pt1), is_point(Pt2),
    Geometry = line([Pt1, Pt2]).

spatial_primitive(Geometry, L) :-
    is_list(L),
    Geometry = region(L). 

spatial_primitive(Geometry, Pt) :-
    is_point(Pt),
    Geometry = Pt.

is_point((X,Y)) :- 
    number(X),number(Y).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% topology function wrapper 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
topology(Geom1, Geom2, Rel) :-
    F =..[Rel, Geom1, Geom2],call(F). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% distance for architectual entities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
min_dist(D1, D2, Min) :-
    mindist(D1, D2, Dist),
    Dist >= Min.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% utility 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_object(Type, Ret) :-
    findall(Object, type(Object, Type), Ret).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% architectual types
% these predicates are currently not being use.
% stuctural_geometry calls the geometry predicate
% directly. These predicates will become useful in the
% future when arhcitectual entites become more complex 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stair(Stair, Region, Landing) :-
    Stair = [region(Region), region(Landing)].

sensor(Sensor, Pt, RangeL) :-
    Sensor = [Pt, region(RangeL)].

window(Win, Pt1, Pt2, Fs) :-
    Win = [line([Pt1,Pt2]), region(Fs)].

door(Door, Pt1, Pt2, Fs) :-
    Door = [line([Pt1, Pt2]),region(Fs)].

wall(Wall, Pt1, Pt2) :-
    Wall = [line([Pt1, Pt2])].

room(Room, Area) :-
    Room = [region(Area)].

slab(Slab, Area) :-
    Slab = [region(Area)].

column(Column, Area) :-
    Column = [region(Area)].


