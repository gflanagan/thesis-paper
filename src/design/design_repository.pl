

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  qualitative spatial attributes found in architecture (QSA) 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% possitioning rules
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
opposing_side(Id1, Id2, Id_Space) :- 
    perspective(Id_space, extended_region, GeoSpace),
    perspective(Id1, point, Pt1),
    perspective(Id2, point, Pt2),
    centroid(GeoSpace, Centroid),
    scc1(Pt1, Pt2, Centroid). 
    % scc0(Pt1, Pt2, Centroid) ;
    % scc1(Pt1, Pt2, Centroid) ;
    % scc7(Pt1, Pt2, Centroid)).

same_side(Id1, Id2, Id_Space) :- 
    perspective(Id_space, extended_region, GeoSpace),
    perspective(Id1, point, Pt1),
    perspective(Id2, point, Pt2),
    centroid(GeoSpace, Centroid),
    print('1 '), print(Pt1), nl, print('2 '), print(Pt2), nl,
    print('rm '), print(Centroid), nl,
    scc3(Pt1, Pt2, Centroid).
    %(scc3(Pt1, Pt2, Centroid) ;
    % scc4(Pt1, Pt2, Centroid) ;
    % scc5(Pt1, Pt2, Centroid)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% facing rules
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
facing_towards(Id1, Id2) :-
    perspective(Id1, directed_point, Dr1),
    perspective(Id2, directed_point, Dr2),
    (opra(Dr1, Dr2, 8, 0, _);
     opra(Dr1, Dr2, 8, 1, _);
     opra(Dr1, Dr2, 8, 32, _)).

facing_away(Id1, Id2) :-
    (facing_towards(Id1, Id2) -> fail, true). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% proximity rules
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% visibility rules
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_visible(Id1, Id2) :-
    viewspace_not_obstructed(Id1, Id2).

% check that no object ubstruct the visibility between Id1, Id2. collects
% all objects that are in that same room as Id1 and Id2 to check for 
% obstruction.
viewspace_not_obstructed(Id1, Id2) :-
    containment(Id1, RM1),
    containment(Id2, RM2),
    match(RM1, RM2),
    get_objects_in_room(RM1, OBJL), 
    remove(Id1, OBJL, OBJL2), remove(Id2, OBJL2, OBJL3),
    viewspace_not_obstructed_set(Id1, Id2, OBJL3, RM1).

viewspace_not_obstructed_set(_,_,[],_).

viewspace_not_obstructed_set(Id1, Id2, [Id3|IdR], RM) :-
    viewspace_not_obstructed(Id1, Id2, Id3, RM),
    viewspace_not_obstructed_set(Id1, Id2, IdR, RM).

viewspace_not_obstructed(Id1, Id2, Id3, RM) :-
   (arch_entity(Id1, dsDoor) ; arch_entity(Id1, dsWindow)),
   (arch_entity(Id2, dsDoor) ; arch_entity(Id2, dswindow)),
   perspective(Id1, fs_point, RM, PT1),
   perspective(Id2, fs_point, RM, PT2),
   perspective(Id3, extended_region, R),
   disconnect(line([PT1,PT2]), R).

viewspace_not_obstructed(Id1, Id2, Id3) :-
    perspective(Id1, point, PT),
    perspective(Id2, extended_region, R2),
    perspective(Id3, extended_region, R3),
    visibility_obstruction_region(PT, R2, OR),
    disconnect(R3,OR).
    
visibility_obstruction_region(PT, region(R), region(PTS)) :-
    convex_hull(R, ConvexR),
    visible_points(PT, ConvexR, Visible_PTS), 
    append([PT], Visible_PTS, PTS). 
 
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
    change_point(S1, S2, PT2, PTS).

visible_points_h(PT1, [PT2,PT3|PR], S1, PTS) :-
    determinant2(PT1, PT2, PT3, Det),
    normalize_sign(Det, S2),
    change_point(S1, S2, PT2, PTS1),
    visible_points_h(PT1, [PT3|PR], S2, PTS2),
    append(PTS1, PTS2, PTS).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  architectural concepts
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% privacy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% continuity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% enclosure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% building codes
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

surveillance(Door, Sensor, RM) :-
    functional_space(Door, RM, FS), 
    range_space(Sensor, RS),
    ntpp(FS, RS).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%
% utility functions
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
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

change_point(S1, S2, C, [C]) :-
    S1 \= S2.

change_point(S1, S2, _, []) :-
    S1 =:= S2. 

get_objects_in_room(RM, RetL):-
    findall(Id, containment(Id, RM), RetL).    

match(A,A).

remove(_,[],[]).

remove(A, [A|AX], PREV) :-
    remove(A, AX, PREV). 

remove(A, [B|BX], [B|PREV]) :-
    A \= B,
    remove(A, BX, PREV).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% graveyard 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%dspace_ctr(InSet, Domain, OutSet, Relation, forall) :-
%   findall(X, pred_test(X, Domain, Y, InSet, Relation), OutSet),print(OutSet),nl.

%pred_test(X, Domain, Y, InSet, Ctr) :-
%    member(X,Domain), member(Y,InSet),
%    F=..[Ctr,X,Y], call(F).  


%secure_doors :-
%    secure_doors_sensors.

%secure_doors_sensors :-
%    get_object(ifcDoor,Doors), 
%    get_object(ifcSensor,Sensors),
%    secure_doors_sensors(Doors, Sensors, forall).

%secure_doors_sensors(Doors, Sensors, Quant) :-
%    dspace_ctr(Sensors, Doors, OutSet, secured_by, Quant).

%secured_by(Door,Sensor) :-
%    structural_geometry(Door,G1), 
%    range_space(Sensor,G2),
%    topology(G1,G2,inside).

%clear_landing :-
%    clear_landing_doors.

%clear_landing_doors :-
%    get_object(ifcStair, Stairs), 
%    get_object(ifcDoor, Doors),
%    clear_landing_doors(Doors, Stairs, forall).

%clear_landing_doors(Doors, Stairs, Quant) :-    
%    dspace_ctr(Doors, Stairs, OutSet, is_clear, Quant).

%is_clear(Stair, Door) :-
%    functional_space(Door,G1), 
%    functional_space(Stair,G2),
%    min_dist(G1,G2,5).


%jurrybox_visibility :-
%    jurrybox_judge_visibility,
%    jurrybox_witness_visibility.

%jurrybox_judge_visibility :-
%    get_object(ifcJurryBox, JurryBox), 
%    get_object(ifcJudgeBench, JudgeBench),
%    visibility(JurryBox, JudgeBench, forall).
       
%jurrybox_witness_visibility :-
%    get_object(ifcJurryBox, JurryBox),    
%    get_object(ifcWitnessBox, WitnessBox),
%    visibility(JurryBox, WitnessBox, forall).

%visibility(A, B, Quant) :-
%    dspace_ctr(B, A, OutSet, is_visible, Quant).

%is_visible2(Id1,Id2) :-
%    orientation(Id1,O1), orientation(Id2,O2), 
%    opra(O1, O2, 4, 1, 15),!.

%is_visible2(Id1,Id2) :- 
%    orientation(Id1,O1), orientation(Id2,O2), 
%    opra(O1, O2, 4, 15, 1),!.

%is_visible2(Id1,Id2) :- 
%    orientation(Id1,O1), orientation(Id2,O2), 
%    opra(O1, O2, 4, 15, 15),!.

%is_visible2(Id1,Id2) :- 
%    orientation(Id1,O1), orientation(Id2,O2), 
%    opra(O1, O2, 4, 1, 1), !.


%attorney_privacy :-
%    get_object(dsAttorneyDesk, AttorneyDesk),
%    get_object(dsJurryBox, JurryBox),
%    dspace_ctr(AttorneyDesk, JurryBox, OutSet, attorney_privacy_ctr, forall).

%attornet_privacy_ctr(Attorney, JurryBox) :-
%    structural_geometry(Attorney, G1),
%    structural_geometry(JurryBox, G2),
%    mid_dist(G1,G2,40).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% old constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%
%secure_doors_old :-
%    unsecure_doors([]).

%unsecure_doors_old(Ret) :-
%    findall(Door, secure_door_old(Door, _), SecureDoor), 
%    get_object(ifcDoor, AllDoors), 
%    subtract(AllDoors, SecureDoor, Ret).

%secure_door_old(Door, Sensor) :-
%    type(Door, ifcDoor), type(Sensor, ifcSensor),
%    functional_space(Door, Fs),
%    range_space(Sensor, Rs),
%    topology(Fs, Rs, inside).

%clear_landing_old :-
%    unclear_landing_old([]).

%unclear_landing_old(Ret) :-
%    findall(Stair, min_landing_dist_old(Stair, 5), ClearLanding), 
%    get_object(ifcStair, AllStairs),
%    subtract(AllStairs, ClearLanding, Ret).

%min_landing_dist_old(Stair, Min) :-
%    min_landing_dist_old(Stair, Min, functional_space),
%    min_landing_dist_old(Stair, Min, operational_space).

%min_landing_dist_old(Stair, Min, Func) :-
%    type(Id, _), type(Stair, ifcStair),
%    functional_space(Stair, FsS),
%    F=..[Func, Id, FsD], call(F),
%    min_dist(FsD, FsS, Min).


