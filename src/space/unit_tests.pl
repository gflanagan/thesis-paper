

unit_tests :-
    test_partial_overlap,
    test_disconnect,
    test_not_obstructed.


test_not_obstructed :-
    print('Not Obstructed Unit Tests'),nl,
    test_pass(not_obstructed(door2, door1, door4), 'Test1'),
    test_fail(not_obstructed(door2, door1, door3), 'Test2'),
    test_fail(not_obstructed(door1, door4, door2), 'Test3').

test_partial_overlap :-
    print('Partial Overlap Unit Tests'),nl,
    R1 = region([(0,0),(5,5),(9,0)]),
    R2 = region([(4,2),(9,4),(10,2)]),
    R3 = region([(15,2),(9,4),(10,2)]),
    R4 = region([(4,6),(8,12),(13,8),(11,6),(6,13)]),
    R5 = region([(4,12),(12,12),(9,2)]),
    R6 = region([(8,7),(8,9),(10,9),(10,7)]),
    R7 = region([(4,10),(5,5),(9,4)]),
    R8 = region([(4,10),(5,5),(9,4),(0,0)]),
    R9 = region([(4,10),(5,5),(9,4),(0,0),(9,0)]),
    test_pass(partial_overlap(R1, R2), 'Test1'),
    test_pass(partial_overlap(R2, R1), 'Test2'),
    test_pass(partial_overlap(R4, R5), 'Test3'),
    test_fail(partial_overlap(R1, R3), 'Test4'),
    test_fail(partial_overlap(R4, R6), 'Test5'),
    test_fail(partial_overlap(R5, R6), 'Test6'),
    test_fail(partial_overlap(R6, R5), 'Test7'),
    test_pass(partial_overlap(R1, R7), 'Test8'),
    test_pass(partial_overlap(R1, R8), 'Test9'),
    test_pass(partial_overlap(R1, R9), 'Test10').


test_disconnect :-
    print('Disconnect Unit Tests'),nl,
    R1 = region([(4,6),(8,12),(13,8),(11,6),(6,3)]),
    R2 = region([(4,12),(12,12),(9,2)]),
    R3 = region([(7,8),(8,10),(9,8)]),
    R4 = region([(-3,-3),(-7,-4),(-3,-8)]),
    R5 = region([(0,5),(2,9),(6,6)]),
    
    test_fail(disconnect(R1, R2), 'Test1'),
    test_fail(disconnect(R2, R1), 'Test2'),
    test_fail(disconnect(R1, R3), 'Test3'),
    test_fail(disconnect(R3, R1), 'Test4'),
    test_fail(disconnect(R2, R3), 'Test5'),
    test_fail(disconnect(R3, R2), 'Test6'),
    test_pass(disconnect(R1, R4), 'Test7'),
    test_pass(disconnect(R4, R1), 'Test8'),
    test_pass(disconnect(R2, R4), 'Test9'),
    test_pass(disconnect(R4, R2), 'Test10'),
    test_fail(disconnect(R1, R5), 'Test11'),
    test_fail(disconnect(R5, R1), 'Test12').



test_pass(Test, Title) :-
    test_print(Title),
    (call(Test) -> print('PASS') ; print('FAIL')),nl.    

test_fail(Test, Title) :-
    test_print(Title),
    (call(Test) -> print('FAIL') ; print('PASS')), nl.

test_print(Title) :-
    print('   '),print(Title), print(': ').

