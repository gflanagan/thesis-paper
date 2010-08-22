:-use_module(library(clpr)).

slope((X1,Y1),(X2,Y2),((Y2-Y1)/(X2-X1))).

y_intercept((X,Y),M,(Y-(M*X))).

reciprocal(M,-(1/M)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Single-Cross
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
scc0((X1, Y1),(X2,Y2),(X3,Y3)) :-
    slope((X1,Y1),(X2,Y2), M1), reciprocal(M1,M2),
    y_intercept((X1,Y1),M1,B1), y_intercept((X2,Y2),M2,B2), 
    {Y3 = M1 * X3 + B1},
    (Y1 < Y2 ->
     {Y3 #> M2 * X3 + B2} ;
     {Y3 #< M2 * X3 + B2}).

scc1((X1,Y1),(X2,Y2),(X3,Y3)) :-
    slope((X1,Y1),(X2,Y2), M1), reciprocal(M1,M2),
    y_intercept((X1,Y1),M1,B1), y_intercept((X2,Y2),M2,B2), 
    (X1 < X2 ->
     {Y3 #> M1 * X3 + B1} ;
     {Y3 #< M1 * X3 + B1}),
    (Y1 < Y2 ->
     {Y3 #> M2 * X3 + B2} ;
     {Y3 #< M2 * X3 + B2}). 

scc2((X1,Y1),(X2,Y2),(X3,Y3)) :-
    slope((X1,Y1),(X2,Y2), M1), reciprocal(M1,M2),
    y_intercept((X1,Y1),M1,B1), y_intercept((X2,Y2),M2,B2), 
    {Y3 #= M2 * X3 + B2},
    (X1 < X2 ->
     {Y3 #> M1 * X3 + B1} ;
     {Y3 #< M1 * X3 + B1}).

scc3((X1,Y1),(X2,Y2),(X3,Y3)) :-
    slope((X1,Y1),(X2,Y2), M1), reciprocal(M1,M2),
    y_intercept((X1,Y1),M1,B1), y_intercept((X2,Y2),M2,B2), 
    (X1 < X2 ->
     {Y3 #> M1 * X3 + B1} ;
     {Y3 #< M1 * X3 + B1}),
    (Y1 < Y2 ->
     {Y3 #< M2 * X3 + B2} ;
     {Y3 #> M2 * X3 + B2}).

scc4((X1,Y1),(X2,Y2),(X3,Y3)) :-
    slope((X1,Y1),(X2,Y2), M1), reciprocal(M1,M2),
    y_intercept((X1,Y1),M1,B1), y_intercept((X2,Y2),M2,B2), 
    {Y3 #= M1 * X3 + B1},
    (Y1 < Y2 ->
     {Y3 #< M2 * X3 + B2} ;
     {Y3 #> M2 * X3 + B2}).

scc5((X1,Y1),(X2,Y2),(X3,Y3)) :-
    slope((X1,Y1),(X2,Y2), M1), reciprocal(M1,M2),
    y_intercept((X1,Y1),M1,B1), y_intercept((X2,Y2),M2,B2), 
    (X1 < X2 ->
     {Y3 #< M1 * X3 + B1} ;
     Y3 #> M1 * X3 + B1),
    (Y1 < Y2 ->
     {Y3 #> M2 * X3 + B2} ;
     {Y3 #< M2 * X3 + B2}).

scc6((X1,Y1),(X2,Y2),(X3,Y3)) :-
    slope((X1,Y1),(X2,Y2), M1), reciprocal(M1,M2),
    y_intercept((X1,Y1),M1,B1), y_intercept((X2,Y2),M2,B2), 
    {Y3 #= M2 * X3 + B2},
    (X1 < X2 ->
     {Y3 #< M1 * X3 + B1} ;
     {Y3 #> M1 * X3 + B1}).

scc7((X1,Y1),(X2,Y2),(X3,Y3)) :-
    slope((X1,Y1),(X2,Y2), M1), reciprocal(M1,M2),
    y_intercept((X1,Y1),M1,B1), y_intercept((X2,Y2),M2,B2), 
    (X1 < X2 ->
     {Y3 #< M1 * X3 + B1} ;
     {Y3 #> M1 * X3 + B1}),
    (Y1 < Y2 ->
     {Y3 #< M2 * X3 + B2} ;
     {Y3 #> M2 * X3 + B2}).




