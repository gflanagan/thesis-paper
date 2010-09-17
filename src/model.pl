

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% building instantiation 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

arch_entity(room1, dsRoom).
arch_entity(wall1, ifcWall).
arch_entity(wall2, ifcWall).
arch_entity(wall3, ifcWall).
arch_entity(wall4, ifcWall).
arch_entity(door1, dsDoor).
arch_entity(door2, dsDoor).
arch_entity(door3, dsDoor).
arch_entity(door4, dsDoor).
arch_entity(sensor1, ifcSensor).
arch_entity(stair1, ifcStair).




ifc_geometry(room1, G) :-
    geometry(G, [(0,0),(0,18),(18,0),(18,18)]).

ifc_geometry(wall1, G) :-
    geometry(G, (0,0), (0,18)).

ifc_geometry(wall2, G) :-
    geometry(G, (0,18), (18,18)).

ifc_geometry(wall3, G) :-
    geometry(G, (18,18), (18,0)).

ifc_geometry(wall4, G) :-
    geometry(G, (18,0), (0,0)).

ifc_geometry(door1, G) :-
    geometry(G, [(4,10),(10,14),(15,14),(15,10)]).

ifc_geometry(door2, G) :-
    geometry(G, [(0,0),(0,2), (2,2),(2,0)]).

ifc_geometry(door4, G) :-
    geometry(G, [(-4,-4),(-5,-5),(-3,-4)]).

ifc_geometry(door3, G) :-
    geometry(G, [(3,3),(3,7), (8,7),(8,3)]).

ifc_geometry(sensor1, G) :-
    geometry(G, (18,14)).

ifc_geometry(stair1, G) :-
    geometry(G, [(0,0),(0,8),(4,8),(4,0)]).

functional_space(door1, Fs) :-
    geometry(Fs, [(0,13),(0,15),(2,15),(2,13)]).

functional_space(door2, Fs) :-
    geometry(Fs, [(0,0),(0,2),(2,0),(2,2)]).

functional_space(stair1, Fs) :-
    geometry(Fs, [(0,8),(0,9),(4,9),(4,8)]).

operational_space(door1, Os) :-
    geometry(Os, (20,20)).

range_space(sensor1, Fs) :-
    geometry(Fs, [(18,14),(0,18),(0,12)]).

