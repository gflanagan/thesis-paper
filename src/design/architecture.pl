
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
    point_transformation(Id, Pt),
    direction_hard_code(Id, Dir).

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


