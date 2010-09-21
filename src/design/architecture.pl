
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% architectual entities are represented in a multi
% perspective manner. eg. a door can be represented 
% directed point. The perspective/3 predicate transforms
% the physical geometry to the various spatial 
% abstraction based on the architectual type.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

point_transformation_list([], []).

point_transformation_list([Id|IdRest], [Pt|PtList]) :-
    point_transformation(Id, Pt),
    point_transformation_list(IdRest, PtList).

point_transformation(Id,  Pt) :-
    ifc_geometry(Id, Geom), 
    abstract_to_point(Geom, Pt).

convex_hull_transformation(Id, Convex) :-
    ifc_geometry(Id, region(Geom)),
    abstract_to_convex_hull(Geom, Convex).

% Direction is generated automatically for doors and windows. Two direction
% vectors will be extracted and returned in a list.
% All other entity types must have direction hard coded.
directed_point_transformation_dw(Id, PtRel, Pt, Dir) :-
    convex_hull_transformation(Id, G),
    calc_direction(G, Pt, DirList),
    closest_point(PtRel, DirList, Dir).

directed_point_transformation(Id, Pt, Dir) :-
    direction_hard_code(Id, Pt, Dir).

calc_direction(G, Pt, DirList) :-
    centroid(G, Pt),
    calc_direction_hlpr(G, DirList).

calc_direction_hlpr([Pt1,Pt2,Pt3,Pt4|_], [Dir1,Dir2]) :-
    longest_length((Pt1, Pt2), (Pt2, Pt3), Seg1),
    longest_length((Pt3, Pt4), (Pt4, Pt1), Seg2),
    midpoint(Seg1, Dir1),
    midpoint(Seg2, Dir2).

%calc_direction_hlpr2((X,Y), ((X1, Y1),(X2,Y2)), (Xd, Yd)) :-
%    Slope is ((Y2-Y1)/(X2-Y1)),
%    Perp_slope is (-(1/Slope)),
%    Y_intercept is (Y-(Perp_slope*X)),
 
midpoint(((X1,Y1),(X2,Y2)), (Xd,Yd)) :-
    Xd is ((X1 + X2) / 2),
    Yd is ((Y1 + Y2) / 2).    

closest_point(PtRel, [Dir1,Dir2], Dir) :-
    mindist(PtRel, Dir1, D1),
    mindist(PtRel, Dir2, D2),
    (D1 >= D2 -> Dir = Dir2 ; Dir = Dir1).

longest_length((Pt1, Pt2), (Pt3, Pt4), ((P1, P2))) :-
    mindist(Pt1, Pt2, D1),        % call to distance contraint solver
    mindist(Pt3, Pt4, D2),
    (D1 >= D2 -> P1 = Pt1, P2 = Pt2 ;
                 P1 = Pt3, P2 = Pt4).

perspective(ID, fs_point, RmID, PT) :-
    functional_space(ID, RmID, FS),
    abstract_to_point(FS, PT).

abstract_to_point(region(R), Centroid) :-
    centroid(R, Centroid).

abstract_to_convex_hull(Geom, Convex) :-
    convex_hull(Geom, Convex).

% geometry utility functions
centroid(R, (X,Y)) :-
    convex_hull(R, Convex),
    x_y_average(Convex, X, Y).

x_y_average(Convex, X, Y) :-
    sum_x_y(Convex, Sum_x, Sum_y),
    length(Convex, L),
    X is Sum_x / (L-1),  %sub 1 because convex has extra pt at end
    Y is Sum_y / (L-1).

% don't process last element because it's a dup because of convex
% hull representation.
sum_x_y([_|[]], 0, 0).

sum_x_y([(X,Y)|Xs], Sum_x, Sum_y) :-
    sum_x_y(Xs, New_x_sum, New_y_sum),
    Sum_x is X + New_x_sum,
    Sum_y is Y + New_y_sum.



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


