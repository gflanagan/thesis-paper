arch_entity(a06_h0R80tmJw7TMroEl0va, dsBuilding).
arch_entity(a1fCiqbw8OpGRaSO3tbyXU, dsBuildingStorey).
arch_entity(w1, dsWallStandardCase).
arch_entity(w2, dsWallStandardCase).
arch_entity(w3, dsWallStandardCase).
arch_entity(w4, dsWallStandardCase).
arch_entity(w5, dsWallStandardCase).
arch_entity(w6, dsWallStandardCase).
arch_entity(doo1, dsdoor).
arch_entity(door2, dsdoor).

ifc_geometry(w1, G) :-
	spatial_primitive(G, [(0,0),(13.6,0),(13.3,0.3),(0.3,0.3),(0,0)]).

ifc_geometry(w2, G) :-
	spatial_primitive(G, [(13.6,0),(13.6,6.6),(13.3,6.3),(13.3,0.3),(13.6,0)]).

ifc_geometry(w3, G) :-
	spatial_primitive(G, [(13.6,6.6),(6.6,6.6),(6.3,6.3),(13.3,6.3),(13.6,6.6)]).

ifc_geometry(w4, G) :-
	spatial_primitive(G, [(6.6,6.6),(6.6,13.6),(6.3,13.3),(6.3,6.3),(6.6,6.6)]).

ifc_geometry(w5, G) :-
	spatial_primitive(G, [(6.6,13.6),(0,13.6),(0.3,13.3),(6.3,13.3),(6.6,13.6)]).

ifc_geometry(w6, G) :-
	spatial_primitive(G, [(1.55112,13.6),(1.0,0),(0.3,0.3),(0.3,13.3),(1.55112,13.6)]).

ifc_geometry(door1, G) :-
	spatial_primitive(G, [(10.3463,0.019),(10.3463,0.319),(11.9203,0.319),(11.9203,0.019),(10.3463,-0.019)]).

ifc_geometry(door2, G) :-
	spatial_primitive(G, [(4.087,13.619),(4.087,13.281),(2.513,13.281),(2.513,13.619),(4.087,13.619)]).

