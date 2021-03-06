
arch_entity(a06_h0R80tmJw7TMroEl0va, dsBuilding).
arch_entity(a1fCiqbw8OpGRaSO3tbyXU, dsBuildingStorey).
arch_entity(a0ZbxEVJPUfIfsik4T2SNWV, dsWallStandardCase).
arch_entity(a3MiiSDGq2vIe7B_EDXK4fW, dsWallStandardCase).
arch_entity(a2Y0ZSQLWNEIxQCel49guaX, dsWallStandardCase).
arch_entity(a3ynJ1m1naWHRn15HKz45l3, dsWallStandardCase).
arch_entity(a2fwFdhKyeYJAcbA3UdcG_C, dsWallStandardCase).
arch_entity(a2Tkz1XiH2_Jx468ptEDA6r, dsWallStandardCase).
arch_entity(a2FyqNujg9nHOxyJVdKl487, dsDoor).
arch_entity(a3NjtDZTasVG8zXOwwGrKsm, dsDoor).
arch_entity(door1, dsDoor).

ifc_geometry(a0ZbxEVJPUfIfsik4T2SNWV, G) :-
	spatial_primitive(G, [(0,0),(13.6,0),(13.3,0.3),(0.3,0.3),(0,0)]).

ifc_geometry(a3MiiSDGq2vIe7B_EDXK4fW, G) :-
	spatial_primitive(G, [(13.6,0),(13.6,6.6),(13.3,6.3),(13.3,0.3),(13.6,0)]).


%ifc_geometry(a2Y0ZSQLWNEIxQCel49guaX, G) :-
%	spatial_primitive(G, [(13.6,6.6),(6.6,6.6),(6.3,6.3),(13.3,6.3),(13.6,6.6)]).

ifc_geometry(a2Y0ZSQLWNEIxQCel49guaX, G) :-
	spatial_primitive(G, [(13.6,6.6),(8,8),(7.8,7.8),(13.3,6.3),(13.6,6.6)]).

%ifc_geometry(a3ynJ1m1naWHRn15HKz45l3, G) :-
%	spatial_primitive(G, [(6.6,6.6),(6.6,13.6),(6.3,13.3),(6.3,6.3),(6.6,6.6)]).

ifc_geometry(a3ynJ1m1naWHRn15HKz45l3, G) :-
	spatial_primitive(G, [(8,8),(6.6,13.6),(6.3,13.3),(7.8,7.8),(8,8)]).

ifc_geometry(a2fwFdhKyeYJAcbA3UdcG_C, G) :-
	spatial_primitive(G, [(6.6,13.6),(0,13.6),(0.3,13.3),(6.3,13.3),(6.6,13.6)]).

ifc_geometry(a2Tkz1XiH2_Jx468ptEDA6r, G) :-
	spatial_primitive(G, [(0,13.6),(0,0),(0.3,0.3),(0.3,13.3),(0,13.6)]).

ifc_geometry(a2FyqNujg9nHOxyJVdKl487, G) :-
	spatial_primitive(G, [(10.3463,-0.019),(10.3463,0.319),(11.9203,0.319),(11.9203,-0.019),(10.3463,-0.019)]).

ifc_geometry(a3NjtDZTasVG8zXOwwGrKsm, G) :-
	spatial_primitive(G, [(4.087,13.619),(4.087,13.281),(2.513,13.281),(2.513,13.619),(4.087,13.619)]).

direction(a3NjtDZTasVG8zXOwwGrKsm, (3.3,15)).
direction(a2FyqNujg9nHOxyJVdKl487, (11.13,1)).

functional_space(a2FyqNujg9nHOxyJVdKl487, rm1, G):-
	spatial_primitive(G, [(9,1),(9,2),(11,2),(11,1)]).

functional_space(a3NjtDZTasVG8zXOwwGrKsm, rm1, G) :-
	spatial_primitive(G, [(4,12),(2.5,12),(4,13),(2.5,13)]).

containment(a0ZbxEVJPUfIfsik4T2SNWV, rm1).
containment(a3MiiSDGq2vIe7B_EDXK4fW, rm1).
containment(a2Y0ZSQLWNEIxQCel49guaX, rm1).
containment(a3ynJ1m1naWHRn15HKz45l3, rm1).
containment(a2fwFdhKyeYJAcbA3UdcG_C, rm1).
containment(a2Tkz1XiH2_Jx468ptEDA6r, rm1).
containment(a2FyqNujg9nHOxyJVdKl487, rm1).
containment(a3NjtDZTasVG8zXOwwGrKsm, rm1).

