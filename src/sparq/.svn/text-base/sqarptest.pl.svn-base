
test :-
    cardir(sensor1, sensor2, BR),
    BR = n.


cardir(Id1, Id2, BR) :-
    structural_geometry(Id1, (X1,Y1)),
    structural_geometry(Id2, (X2,Y2)),
    qualify(cardir, [obj(Id1, X1, Y1),obj(Id2, X2,Y2)], R, Rlist),
    print(R),
    get_br(R, BR).

get_br([[_,BR,_]], BR).



