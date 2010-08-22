

is_visible(ID1, ID2) :-
    line_of_site(ID1,ID2),
    not_obstructed_room(ID1, ID2).

line_of_site(ID1,ID2) :-
    perspective(ID1, directed_point, PT1),
    perspective(ID2, directed_point, PT2),
    print(PT1),nl,print(PT2),nl,
    (opra(PT1, PT2, 3, 7, _) ;
     opra(PT1, PT2, 3, 0, _) ;
     opra(PT1, PT2, 3, 1, _)).    

get_objects_in_room(RM, Ret):-
    findall(ID, room_contain(ID, RM), Ret).

not_obstructed_room(ID1, ID2) :-
    %get_room_id(ID1, RM),
    get_objects_in_room(rm1, OBJL),
    not_obstructed_set(ID1, ID2, OBJL).

not_obstructed_set(_,_,[]).

not_obstructed_set(ID1, ID2, [ID3|IDR]) :-
    not_obstructed(ID1, ID2, ID3),
    not_obstructed_set(ID1, ID2, IDR).

not_obstructed(ID1, ID2, ID3) :-
    perspective(ID1, point, PT),
    perspective(ID2, extended_region, R2),
    perspective(ID3, extended_region, R3),
    obstruction_region(PT, R2, OR),
    disconnect(R3,OR).
    
obstruction_region(PT, region(R), region(PTS)) :-
    convex_hull(R, ConvexR),
    visible_points(PT, ConvexR, Visible_PTS), 
    append([PT], Visible_PTS, PTS). 
 
determinant2((X1, Y1),(X2, Y2), (X3, Y3), D) :-
    %D is ((Y1-Y2)*(X3-X2)) - ((X1-X2)*(Y3-Y2)).
    Matrix_0_0 is X1 - X3,
    Matrix_0_1 is X2 - X3,
    Matrix_1_0 is Y1 - Y3,
    Matrix_1_1 is Y2 - Y3,
    Cross1 is Matrix_0_0 * Matrix_1_1,
    Cross2 is Matrix_0_1 * Matrix_1_0,
    D is Cross1 - Cross2.

normalize_sign(S_old, S_normal) :-
    (S_old < 0 -> S_normal = -1 ; S_normal = 1).

visible_points(PT1, [PT2,PT3|PR], PTS) :- 
    determinant2(PT1, PT2, PT3, Det),
    normalize_sign(Det, S),
    append([PT3|PR], [PT3], L),
    visible_points_h(PT1, L, S, PTS).

visible_points_h(PT1,[PT2,PT3|[]], S1, PTS) :- 
    determinant2(PT1, PT2, PT3, Det),
    normalize_sign(Det, S2),
    %print(PT2),print(' '),print(PT3),print(' '),print(Det),nl,
    change_point(S1, S2, PT2, PTS).

visible_points_h(PT1, [PT2,PT3|PR], S1, PTS) :-
    determinant2(PT1, PT2, PT3, Det),
    normalize_sign(Det, S2),
    change_point(S1, S2, PT2, PTS1),
    visible_points_h(PT1, [PT3|PR], S2, PTS2),
    %print(PT2),print(' '),print(PT3),print(' '),print(Det),nl,
    append(PTS1, PTS2, PTS).

change_point(S1, S2, C, [C]) :-
    S1 \= S2.

change_point(S1, S2, _, []) :-
    S1 =:= S2. 



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

is_visible2(ID1,ID2) :-
    orientation(ID1,O1), orientation(ID2,O2), 
    opra(O1, O2, 4, 1, 15),!.

is_visible2(ID1,ID2) :- 
    orientation(ID1,O1), orientation(ID2,O2), 
    opra(O1, O2, 4, 15, 1),!.

is_visible2(ID1,ID2) :- 
    orientation(ID1,O1), orientation(ID2,O2), 
    opra(O1, O2, 4, 15, 15),!.

is_visible2(ID1,ID2) :- 
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


