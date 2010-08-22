

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% design constraints 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dspace_ctr(InSet, Domain, OutSet, Relation, forall) :-
   findall(X, pred_test(X, Domain, Y, InSet, Relation), OutSet),print(OutSet),nl.

pred_test(X, Domain, Y, InSet, Ctr) :-
    member(X,Domain), member(Y,InSet),
    F=..[Ctr,X,Y], call(F).  


secure_doors :-
    secure_doors_sensors.

secure_doors_sensors :-
    get_object(ifcDoor,Doors), 
    get_object(ifcSensor,Sensors),
    secure_doors_sensors(Doors, Sensors, forall).

secure_doors_sensors(Doors, Sensors, Quant) :-
    dspace_ctr(Sensors, Doors, OutSet, secured_by, Quant).

secured_by(Door,Sensor) :-
    structural_geometry(Door,G1), 
    range_space(Sensor,G2),
    topology(G1,G2,inside).

clear_landing :-
    clear_landing_doors.

clear_landing_doors :-
    get_object(ifcStair, Stairs), 
    get_object(ifcDoor, Doors),
    clear_landing_doors(Doors, Stairs, forall).

clear_landing_doors(Doors, Stairs, Quant) :-    
    dspace_ctr(Doors, Stairs, OutSet, is_clear, Quant).

is_clear(Stair, Door) :-
    functional_space(Door,G1), 
    functional_space(Stair,G2),
    min_dist(G1,G2,5).


jurrybox_visibility :-
    jurrybox_judge_visibility,
    jurrybox_witness_visibility.

jurrybox_judge_visibility :-
    get_object(ifcJurryBox, JurryBox), 
    get_object(ifcJudgeBench, JudgeBench),
    visibility(JurryBox, JudgeBench, forall).
       
jurrybox_witness_visibility :-
    get_object(ifcJurryBox, JurryBox),    
    get_object(ifcWitnessBox, WitnessBox),
    visibility(JurryBox, WitnessBox, forall).

visibility(A, B, Quant) :-
    dspace_ctr(B, A, OutSet, is_visible, Quant).

is_visible(ID1,ID2) :-
    orientation(ID1,O1), orientation(ID2,O2), 
    opra(O1, O2, 4, 1, 15),!.

is_visible(ID1,ID2) :- 
    orientation(ID1,O1), orientation(ID2,O2), 
    opra(O1, O2, 4, 15, 1),!.

is_visible(ID1,ID2) :- 
    orientation(ID1,O1), orientation(ID2,O2), 
    opra(O1, O2, 4, 15, 15),!.

is_visible(ID1,ID2) :- 
    orientation(ID1,O1), orientation(ID2,O2), 
    opra(O1, O2, 4, 1, 1), !.


attorney_privacy :-
    get_object(dsAttorneyDesk, AttorneyDesk),
    get_object(dsJurryBox, JurryBox),
    dspace_ctr(AttorneyDesk, JurryBox, OutSet, attorney_privacy_ctr, forall).

attornet_privacy_ctr(Attorney, JurryBox) :-
    structural_geometry(Attorney, G1),
    structural_geometry(JurryBox, G2),
    mid_dist(G1,G2,40).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% geometry 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

geometry(Geometry, Pt1, Pt2) :-
    is_point(Pt1), is_point(Pt2),
    Geometry = line([Pt1, Pt2]).

geometry(Geometry, L) :-
    is_list(L),
    Geometry = region(L). 

geometry(Geometry, Pt) :-
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% old constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%
secure_doors_old :-
    unsecure_doors([]).

unsecure_doors_old(Ret) :-
    findall(Door, secure_door_old(Door, _), SecureDoor), 
    get_object(ifcDoor, AllDoors), 
    subtract(AllDoors, SecureDoor, Ret).

secure_door_old(Door, Sensor) :-
    type(Door, ifcDoor), type(Sensor, ifcSensor),
    functional_space(Door, Fs),
    range_space(Sensor, Rs),
    topology(Fs, Rs, inside).

clear_landing_old :-
    unclear_landing_old([]).

unclear_landing_old(Ret) :-
    findall(Stair, min_landing_dist_old(Stair, 5), ClearLanding), 
    get_object(ifcStair, AllStairs),
    subtract(AllStairs, ClearLanding, Ret).

min_landing_dist_old(Stair, Min) :-
    min_landing_dist_old(Stair, Min, functional_space),
    min_landing_dist_old(Stair, Min, operational_space).

min_landing_dist_old(Stair, Min, Func) :-
    type(Id, _), type(Stair, ifcStair),
    functional_space(Stair, FsS),
    F=..[Func, Id, FsD], call(F),
    min_dist(FsD, FsS, Min).


