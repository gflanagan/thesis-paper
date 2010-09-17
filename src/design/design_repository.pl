
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  qualitative spatial attributes found in architecture (QSA) 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% positioning rules
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

opposing_side(Id1, Id2, ContextId) :- 
    point_transformation(Id1, Pt1),
    point_transformation(Id2, Pt2),
    point_transformation(ContextId, Pt3),
    ( scc0(Pt1, Pt3, Pt2) ;
      scc1(Pt1, Pt3, Pt2) ;
      scc7(Pt1, Pt3, Pt2) ).

same_side(Id1, Id2, ContextId) :-
    point_transformation(Id1, Pt1),
    point_transformation(Id2, Pt2),
    point_transformation(ContextId, Pt3),
    ( scc3(Pt1, Pt3, Pt2) ;
      scc4(Pt1, Pt3, Pt2) ;
      scc5(Pt1, Pt3, Pt2) ).

left_side(Id1, Id2, ContextId) :-
    point_transformation(Id1, Pt1),
    point_transformation(Id2, Pt2),
    point_transformation(ContextId, Pt3),
    ( scc5(Pt1, Pt3, Pt2) ;
      scc6(Pt1, Pt3, Pt2) ;
      scc7(Pt1, Pt3, Pt2) ).

right_side(Id1, Id2, ContextId) :-
    point_transformation(Id1, Pt1),
    point_transformation(Id2, Pt2),
    point_transformation(ContextId, Pt3),
    ( scc1(Pt1, Pt3, Pt2) ;
      scc2(Pt1, Pt3, Pt2) ;
      scc3(Pt1, Pt3, Pt2) ).

same_left(Id1, Id2, ContextId) :-
    same_side(Id1, Id2, ContextId), 
    left_side(Id1, Id2, ContextId).

same_right(Id1, Id2, ContextId) :-
    same_side(Id1, Id2, ContextId),
    right_side(Id1, Id2, ContextId).

opposing_left(Id1, Id2, ContextId) :-
    opposing_side(Id1, Id2, ContextId),
    left_side(Id1, Id2, ContextId).

opposing_right(Id1, Id2, ContextId) :-
    opposing_side(Id1, Id2, ContextId),
    right_side(Id1, Id2, ContextId).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% positioning for sequence rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

horizontally_perceived(Pt, Context, List) :-
    point_transformation(Context, PtContext),
    point_transformation_list(List, PtList),
    horizontally_perceived_hlpr(Pt, PtContext, PtList, SCC),
    check_scc_567(SCC),
    check_scc_107(SCC).

horizontally_perceived_hlpr(_, _, [], []).

horizontally_perveived_hlpr(Pt, PtC, [Obj|Os], SCC) :-
    print('todo').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% facing rules
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

facing_towards(Id1, Id2) :-
    directed_point_transformation(Id1, Pt, Dir),
    point_transformation(Id2, Pt2),
    ( opra(Pt, Dir, Pt2, 8, 0) ;
      opra(Pt, Dir, Pt2, 8, 1) ;
      opra(Pt, Dir, Pt2, 8, 31) ).

facing_away(Id1, Id2) :-
    (facing_towards(Id1, Id2) -> fail; true). 

facing_towards_each_other(Id1, Id2) :-
    facing_towards(Id1, Id2),
    facing_towards(Id2, Id1).

facing_away_from_each_other(Id1, Id2) :-
    facing_away(Id1, Id2),
    facing_away(Id2, Id1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% proximity rules
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

near(Id1, Id2) :-
    ifc_geometry(Id1, G1),
    ifc_geometry(Id2, G2),
    mindist(G1, G2, Dist),
    Dist < 3.

near_plus(Id1, Id2) :-
    ifc_geometry(Id1, G1),
    ifc_geometry(Id2, G2),
    mindist(G1, G2, Dist),
    Dist < 6,
    Dist > 3.

far(Id1, Id2) :-
    ifc_geometry(Id1, G1),
    ifc_geometry(Id2, G2),
    mindist(G1, G2, Dist),
    Dist > 6.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% visibility rules
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% checks for obstruction between Id1 / Id2, given list of objects 
is_visible(Id1, Id2, ObstructList) :-
    point_transformation(Id1, Pt),
    convex_hull_transformation(Id2, Convex),
    generate_viewspace(Pt, Convex, Viewspace),
    is_visible_hlpr(Viewspace, ObstructList).

is_visible_hlpr(_,[]).

is_visible_hlpr(Viewspace, [Id|Rest]) :-
    convex_hull_transformation(Id, Convex),
    disconnect(Viewspace, Convex),
    is_visible_hlpr(Viewspace, Rest).

generate_viewspace(Pt, Convex, Viewspace) :-
    visible_points(Pt, Convex, VisiblePts),
    append([Pt], VisiblePts, Pts),
    convex_hull(Pts, Viewspace). 


% - checks for visibility between two objects located in the same room
% - if objects are not in the same room then predicate will fail
% - walls that contain doors / windows being checked for visibility are
%   removed from obstruction list.

is_visible(Id1, Id2) :-
    shared_room(Id1, Id2, Rm),
    get_objects_in_room(Rm, ObjList),
    remove_list([Id1,Id2], ObjList, ObjList2),
    ((arch_entity(Id1, dsDoor) ; arch_entity(Id1, dsWindow)) -> 
        wall_containment(Id1, W1), remove(W1, ObjList2, ObjList3) ;
        ObjList2 = ObjList3),
    ((arch_entity(Id2, dsDoor) ; arch_entity(Id2, dsWindow)) -> 
        wall_containment(Id2, W2), remove(W2, ObjList3, ObjList4) ;
        ObjList3 = ObjList4),
    print(ObjList4),nl,
    is_visible(Id1, Id2, ObjList4), !. 

shared_room(Id1, Id2, Rm) :-
    room_containment(Id1, Rm),
    room_containment(Id2, Rm).

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
 
normalize_sign(S_old, S_normal) :-
    (S_old < 0 -> S_normal = -1 ; S_normal = 1).

determinant2((X1, Y1),(X2, Y2), (X3, Y3), D) :-
    %D is ((Y1-Y2)*(X3-X2)) - ((X1-X2)*(Y3-Y2)).
    Matrix_0_0 is X1 - X3,
    Matrix_0_1 is X2 - X3,
    Matrix_1_0 is Y1 - Y3,
    Matrix_1_1 is Y2 - Y3,
    Cross1 is Matrix_0_0 * Matrix_1_1,
    Cross2 is Matrix_0_1 * Matrix_1_0,
    D is Cross1 - Cross2.


change_point(S1, S2, C, [C]) :-
    S1 \= S2.

change_point(S1, S2, _, []) :-
    S1 =:= S2. 


get_objects_in_room(Rm, RetList):-
    findall(Id, room_containment(Id, Rm), RetList).    

remove_list([], A, A).

remove_list([A|AX], Old, New) :-
    remove(A, Old, Old2),
    remove_list(AX, Old2, New).  

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


