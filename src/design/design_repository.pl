
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% qualitative spatial attributes found in architecture (QSA) 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% positioning
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
% positioning for sequence
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

horizontally_perceived(Id, Context, List) :-
    point_transformation(Id, Pt),
    horizontally_perceived(Pt, Context, List).

horizontally_perceived((X,Y), Context, List) :-
    point_transformation(Context, PtContext),
    point_transformation_list(List, PtList),
    check_scc_567((X,Y), PtContext, PtList),
    check_scc_123((X,Y), PtContext, PtList).

check_scc_567(_,_,[]) :- fail, !.

check_scc_567(Origin, Relatum, [Referent|Rest]) :-
    ( (scc5(Origin, Relatum, Referent) ;
       scc6(Origin, Relatum, Referent) ;
       scc7(Origin, Relatum, Referent)) ->
       true, ! ;
       check_scc_567(Origin, Relatum, Rest)).

check_scc_123(_,_,[]) :- fail, !.

check_scc_123(Origin, Relatum, [Referent|Rest]) :-
    ( (scc1(Origin, Relatum, Referent) ;
       scc2(Origin, Relatum, Referent) ;
       scc3(Origin, Relatum, Referent)) ->
       true, ! ;
       check_scc_123(Origin, Relatum, Rest)).


vertically_perceived(Id, Context, List) :-
    point_transformation(Id, Pt),
    vertically_perceived(Pt, Context, List).

vertically_perceived((X,Y), Context, List) :-
    point_transformation(Context, PtContext),
    point_transformation_list(List, PtList),
    check_scc_35((X,Y), PtContext, PtList),
    check_scc_017((X,Y), PtContext, PtList).

check_scc_35(_,_,[]) :- fail, !.

check_scc_35(Origin, Relatum, [Referent|Rest]) :-
    ( (scc3(Origin, Relatum, Referent) ;
       scc5(Origin, Relatum, Referent) ) ->
       true, ! ;
       check_scc_35(Origin, Relatum, Rest)).

check_scc_017(_,_,[]) :- fail, !.

check_scc_017(Origin, Relatum, [Referent|Rest]) :-
    ( (scc0(Origin, Relatum, Referent) ;
       scc1(Origin, Relatum, Referent) ;
       scc7(Origin, Relatum, Referent)) ->
       true, ! ;
       check_scc_017(Origin, Relatum, Rest)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% facing 
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

facing_towards(Id1, Id2) :-
    % direction for doors/windows are automatically calculated,
    % all other objects must have a hard coded direction.
    ( (arch_entity(Id1, dsDoor) ; arch_entity(Id1, dsWindow)) -> 
       directed_point_transformation_dw(Id1, Pt2, Pt, Dir) ;
       directed_point_transformation(Id1, Pt, Dir) ),
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
% proximity
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

near((X,Y), Id2) :-
    ifc_geometry(Id2, G2),
    mindist((X,Y), G2, Dist),
    Dist < 3.

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
% visibility
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% check visibility between Id1 / Id2, given list of objects 
is_visible(Id1, Id2, ObstructList) :-
    point_transformation(Id1, Pt),
    convex_hull_transformation(Id2, Convex),
    generate_viewspace(Pt, Convex, Viewspace),
    is_visible_hlpr(Viewspace, ObstructList).

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
    is_visible(Id1, Id2, ObjList4), !. 

is_visible_hlpr(_,[]).

is_visible_hlpr(Viewspace, [Id|Rest]) :-
    convex_hull_transformation(Id, Convex),
    disconnect(Viewspace, Convex),
    is_visible_hlpr(Viewspace, Rest).

generate_viewspace(Pt, Convex, Viewspace) :-
    visible_points(Pt, Convex, VisiblePts),
    append([Pt], VisiblePts, Pts),
    convex_hull(Pts, Viewspace). 

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

%privacy(RmId, BldgId, EntranceId, LivingId) :-
    %get_doors_in_room(RmId, Doors),
    % get_doors_in_adjacent_room(RmId, Doors2),
    %opposing_side(EntranceId, BldgId, RmId),
    %not_visible(LivingId, Doors).
    % facing_away(LivingId, Doors).
     


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% continuity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
continuity([]).

continuity([RmId|Rest]) :-
    get_doors_in_room(RmId, Doors),
    mutually_visible(Doors),
    continuity(Rest).

mutually_visible([Dr1, Dr2|[]]) :-
    mutually_visible_hlpr(Dr1, [Dr2]).

mutually_visible([Dr1, Dr2|Rest]) :-
    mutually_visible_hlpr(Dr1, Rest),
    mutually_visible([Dr2|Rest]).

mutually_visible_hlpr(_,[]).

mutually_visible_hlpr(Dr1, [Dr2|Rest]) :-
    is_visible(Dr1, Dr2),
    is_visible(Dr2, Dr2),
    mutually_visible_hlrp(Dr1, Rest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% enclosure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
confined_space(Id) :-
    point_transformation(Id, Pt),
    room_containment(Id, Rm),
    confined_space(Pt, Rm).

confined_space(Pt, RmId) :-
    get_objects_in_room(RmId, Objs),
    num_near_objs(Pt, Objs, N), print(N),nl,
    N >= 3.
 
open_space(Id) :-
    point_transformation(Id, Pt),
    room_containment(Id, Rm),
    open_space(Pt, Rm).

open_space(Pt, RmId) :-
    get_objects_in_room(RmId, Objs),
    num_near_objs(Pt, Objs, N),
    N < 3.

num_near_objs(_,[],0).

num_near_objs(Pt, [O|Os], N) :-
    num_near_objs(Pt, Os, NRest),
    ( near(Pt, O) -> N is NRest + 1 ; N is NRest + 0 ).
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% building codes
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

surveillance(Door, Sensor, RM) :-
    functional_space(Door, RM, FS), 
    range_space(Sensor, RS),
    ntpp(FS, RS).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% utility functions
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

doors_in_room(Id, Rm) :-
    arch_entity(Id, dsDoor),
    room_containment(Id, Rm).

get_doors_in_room(Rm, Drs) :-
    findall(Id, doors_in_room(Id, Rm), Drs).

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


