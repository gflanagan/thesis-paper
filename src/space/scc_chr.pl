%:-use_module(library(chr)).

slope((X1,Y1),(X2,Y2),((Y2-Y1)/(X2-X1))).

y_intercept((X,Y),M,(Y-(M*X))).

reciprocal(M,-(1/M)).


:- chr_constraint less_than_inequality_cstr/3, greater_than_inequality_cstr/3, equality_cstr/3.

less_than_inequality_cstr((X,Y), S, B) <=>
        Y < S * X + B.

greater_than_inequality_cstr((X,Y), S, B) <=>
        Y > S * X + B.

equality_cstr((X,Y), S, B) <=>
        Y =:= S * X + B.

:- chr_constraint scc0/3, scc1/3, scc2/3, scc3/3, scc4/3, scc5/3, scc6/3, scc7/3.

% scc0
scc0_1 @ scc0((X1, Y1), (X2, Y2), (X3, Y3)) <=> 
        Y1 < Y2,
        X1 =< X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        equality_cstr((X3,Y3), S1, B1),
        greater_than_inequality_cstr((X3,Y3), S2, B2).

scc0_2 @ scc0((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 >= Y2,
        X1 < X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        equality_cstr((X3,Y3), S1, B1),
        less_than_inequality_cstr((X3,Y3), S2, B2).

scc0_3 @ scc0((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 < Y2,
        X1 >= X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        equality_cstr((X3,Y3), S1, B1),
        greater_than_inequality_cstr((X3,Y3), S2, B2).

scc0_4 @ scc0((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 >= Y2,
        X1 > X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        equality_cstr((X3,Y3), S1, B1),
        less_than_inequality_cstr((X3,Y3), S2, B2).



% scc1
scc1_1 @ scc1((X1, Y1), (X2, Y2), (X3, Y3)) <=> 
        Y1 < Y2,
        X1 =< X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        less_than_inequality_cstr((X3,Y3), S1, B1),
        greater_than_inequality_cstr((X3,Y3), S2, B2).

scc1_2 @ scc1((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 >= Y2,
        X1 < X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        less_than_inequality_cstr((X3,Y3), S1, B1),
        less_than_inequality_cstr((X3,Y3), S2, B2).

scc1_3 @ scc1((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 < Y2,
        X1 >= X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        greater_than_inequality_cstr((X3,Y3), S1, B1),
        greater_than_inequality_cstr((X3,Y3), S2, B2).

scc1_4 @ scc1((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 >= Y2,
        X1 > X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        greater_than_inequality_cstr((X3,Y3), S1, B1),
        less_than_inequality_cstr((X3,Y3), S2, B2).

% scc2
scc2_1 @ scc2((X1, Y1), (X2, Y2), (X3, Y3)) <=> 
        Y1 < Y2,
        X1 =< X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        less_than_inequality_cstr((X3,Y3), S1, B1),
        equality_cstr((X3,Y3), S2, B2).

scc2_2 @ scc2((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 >= Y2,
        X1 < X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        less_than_inequality_cstr((X3,Y3), S1, B1),
        equality_cstr((X3,Y3), S2, B2).

scc2_3 @ scc2((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 < Y2,
        X1 >= X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        greater_than_inequality_cstr((X3,Y3), S1, B1),
        equality_cstr((X3,Y3), S2, B2).

scc2_4 @ scc2((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 >= Y2,
        X1 > X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        greater_than_inequality_cstr((X3,Y3), S1, B1),
        equality_cstr((X3,Y3), S2, B2).


% scc3
scc3_1 @ scc3((X1, Y1), (X2, Y2), (X3, Y3)) <=> 
        Y1 < Y2,
        X1 =< X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        less_than_inequality_cstr((X3,Y3), S1, B1),
        less_than_inequality_cstr((X3,Y3), S2, B2).

scc3_2 @ scc3((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 >= Y2,
        X1 < X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        less_than_inequality_cstr((X3,Y3), S1, B1),
        greater_than_inequality_cstr((X3,Y3), S2, B2).

scc3_3 @ scc3((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 =< Y2,
        X1 > X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        greater_than_inequality_cstr((X3,Y3), S1, B1),
        less_than_inequality_cstr((X3,Y3), S2, B2).

scc3_4 @ scc3((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 > Y2,
        X1 >= X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        greater_than_inequality_cstr((X3,Y3), S1, B1),
        greater_than_inequality_cstr((X3,Y3), S2, B2).

% scc4
scc4_1 @ scc4((X1, Y1), (X2, Y2), (X3, Y3)) <=> 
        Y1 < Y2,
        X1 =< X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        equality_cstr((X3,Y3), S1, B1),
        less_than_inequality_cstr((X3,Y3), S2, B2).

scc4_2 @ scc4((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 >= Y2,
        X1 < X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        equality_cstr((X3,Y3), S1, B1),
        greater_than_inequality_cstr((X3,Y3), S2, B2).

scc4_3 @ scc4((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 < Y2,
        X1 >= X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        equality_cstr((X3,Y3), S1, B1),
        less_than_inequality_cstr((X3,Y3), S2, B2).

scc4_4 @ scc4((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 >= Y2,
        X1 > X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        equality_cstr((X3,Y3), S1, B1),
        greater_than_inequality_cstr((X3,Y3), S2, B2).


% scc5
scc5_1 @ scc5((X1, Y1), (X2, Y2), (X3, Y3)) <=> 
        Y1 < Y2,
        X1 =< X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        greater_than_inequality_cstr((X3,Y3), S1, B1),
        less_than_inequality_cstr((X3,Y3), S2, B2).

scc5_2 @ scc5((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 >= Y2,
        X1 < X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        greater_than_inequality_cstr((X3,Y3), S1, B1),
        greater_than_inequality_cstr((X3,Y3), S2, B2).

scc5_3 @ scc5((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 =< Y2,
        X1 > X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        less_than_inequality_cstr((X3,Y3), S1, B1),
        less_than_inequality_cstr((X3,Y3), S2, B2).

scc5_4 @ scc5((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 > Y2,
        X1 >= X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        less_than_inequality_cstr((X3,Y3), S1, B1),
        greater_than_inequality_cstr((X3,Y3), S2, B2).

% scc6
scc6_1 @ scc6((X1, Y1), (X2, Y2), (X3, Y3)) <=> 
        Y1 < Y2,
        X1 =< X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        less_than_inequality_cstr((X3,Y3), S1, B1),
        equality_cstr((X3,Y3), S2, B2).

scc6_2 @ scc6((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 >= Y2,
        X1 < X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        less_than_inequality_cstr((X3,Y3), S1, B1),
        equality_cstr((X3,Y3), S2, B2).

scc6_3 @ scc6((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 < Y2,
        X1 >= X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        greater_than_inequality_cstr((X3,Y3), S1, B1),
        equality_cstr((X3,Y3), S2, B2).

scc6_4 @ scc6((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 >= Y2,
        X1 > X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        greater_than_inequality_cstr((X3,Y3), S1, B1),
        equality_cstr((X3,Y3), S2, B2).

% scc7
scc7_1 @ scc7((X1, Y1), (X2, Y2), (X3, Y3)) <=> 
        Y1 < Y2,
        X1 =< X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        greater_than_inequality_cstr((X3,Y3), S1, B1),
        greater_than_inequality_cstr((X3,Y3), S2, B2).

scc7_2 @ scc7((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 >= Y2,
        X1 < X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        greater_than_inequality_cstr((X3,Y3), S1, B1),
        less_than_inequality_cstr((X3,Y3), S2, B2).

scc7_3 @ scc7((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 =< Y2,
        X1 > X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        less_than_inequality_cstr((X3,Y3), S1, B1),
        greater_than_inequality_cstr((X3,Y3), S2, B2).

scc7_4 @ scc7((X1, Y1), (X2, Y2), (X3, Y3)) <=>
        Y1 > Y2,
        X1 >= X2 |
        slope((X1, Y1), (X2, Y2), S1),
        reciprocal(S1, S2),
        y_intercept((X1,Y1), S1, B1),
        y_intercept((X2,Y2), S2, B2),
        less_than_inequality_cstr((X3,Y3), S1, B1),
        less_than_inequality_cstr((X3,Y3), S2, B2).


