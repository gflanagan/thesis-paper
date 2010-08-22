%
% Author: Gregory Flanagan
% email: gregmflanagan @ gmail.com
%
% Description: Creates a tcp interface with the sparq toolkit
%              and provides services to qualify 2D point, dipole,
%	       and oriented point objects. 
%
% NOTE: Processing the input stream is very specific to the current
%       format provided by sparq. If this format changes it is
%	likely the read_from_sparq will no longer work. 
% 
% Usage: 1) create a sparq client with create_sparq_client/0
%	 2) to qualify a list of object use qualify/4              
%	 3) qualify(Calculus, ObjetList, ReturnList, ReturnPredList)
% 


:- use_module(library(socket)).

sparq_port(4443).
sparq_host('localhost').


qualify(Calculus, OList, R, RList) :-
    build_cmd(OList, Calculus, CMD), 
    write_to_sparq(CMD),
    read_from_sparq(Calculus, R, RList).
    %my_write(R), my_write(RList). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% constructs a tcp client that interacts with SparQ
% input stream is stored in sparq_in_stream/1
% output stream is store in sparq_out_stream/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_sparq_client :-
    sparq_port(P), sparq_host(H),
    tcp_socket(Socket),
    tcp_connect(Socket, H:P), 
    tcp_open_socket(Socket, Read, Write),
    assert(sparq_in_stream(Read)),
    assert(sparq_out_stream(Write)).

close_sparq_client :-
    sparq_in_stream(In),
    sparq_out_stream(Out),
    close(In),
    close(Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% i/o predicates 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_to_sparq(Command) :-
    sparq_out_stream(Out),
    write(Out, Command),
    flush_output(Out).

read_from_sparq(Calculus, R, RList) :-
    % processing input depends on '>' char 
    sparq_in_stream(In),
    seek('>', In), seek('(',In),
    % reads stream per character, groups into words,
    % and adds workds into list until \n
    % () characters encompas a relation
    % NOTE: if sparq's output format changes this code will not work
    read_stream(In, Line),
    % transforms list of characters into list of relations
    % this is specific to the output format of sparq
    % [(,a, dc, b,),(,d, tpp, b,)] -> [[a, dc, b],[d, tpp, b]]
    transform(Line, R),
    map_qsr(R, RList, Calculus).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% input utility predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seek(Symbol, In) :-
    get_char(In, C),
    same(Symbol, C, In).

same(S, S, _).

same(S, _, In) :-
    seek(S, In).


trans([')'|XS], [], XS) :- !.

trans([X|XS], [X|RS], R) :- trans(XS, RS, R).

transform([], []) :- !.

transform([')'|[]], []) :- !.

transform(['('|RS], [L|List]) :- 
    trans(RS, L, R), 
    transform(R, List).
    

read_stream(In, Line) :-
    get_char(In, C),
    get_relation(C, Line, In).

get_relation('\n', [], _) :- !.

get_relation(' ', L, In) :- !, read_stream(In, L).

get_relation('(', ['('|L], In) :- !, read_stream(In, L). 

get_relation(')', [')'|L], In) :- !, read_stream(In, L).

get_relation(Char, [Word|Wordlist], In) :-
    get_chars(Char, Chars, Nextchar, In),
    name(Word, Chars),
    get_relation(Nextchar, Wordlist, In).

get_chars('\n', [], '\n', _) :- !.

get_chars(' ', [], ' ', _) :- !.

get_chars(')', [], ')', _) :- !.

get_chars(C, [C|CS], Nextchar, In) :-
    get_char(In, Char),
    get_chars(Char, CS, Nextchar, In).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% builds sparq qualification command given:
%   - calculus
%   - list of list of objects in form 
%        2d pt -> [[id, x, y],[id,x,y]] or 
%        dipole -> [[id, x, y, dx, dy],[id,x,y,dx,dy]] 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
extract_objs([], '').

extract_objs([obj(ID, X, Y)|OS], CMD) :-
    extract_pt(ID, X, Y, L),
    extract_objs(OS, CMD1),
    concat(L,CMD1,CMD).

extract_objs([obj(ID, X, Y, DX, DY)|OS], CMD) :-
    extract_dipole(ID,X,Y,DX,DY,L),
    extract_objs(OS, CMD1),
    concat(L, CMD1, CMD).

extract_dipole(ID, X, Y, DX, DY, L) :-
    concat('(',ID, CMD1),
    concat(CMD1,' ',CMD2),
    concat(CMD2,X,CMD3),
    concat(CMD3,' ',CMD4),
    concat(CMD4,Y,CMD5),
    concat(CMD5,' ',CMD6),
    concat(CMD6,DX,CMD7),
    concat(CMD7,' ',CMD8),
    concat(CMD8,DY,CMD9),
    concat(CMD9,')',L).

extract_pt(ID, X, Y, L) :-
    concat('(',ID, CMD1),
    concat(CMD1,' ',CMD2),
    concat(CMD2,X,CMD3),
    concat(CMD3,' ',CMD4),
    concat(CMD4,Y,CMD5),
    concat(CMD5,')',L).

build_cmd(OList, opra, CMD) :- 
    concat('qualify opra-2',' all (', CMD1),
    extract_objs(OList, C),
    concat(CMD1, C, CMD2),
    concat(CMD2, ')\n', CMD).

build_cmd(OList, dra, CMD) :- 
    concat('qualify dra-24',' all (', CMD1),
    extract_objs(OList, C),
    concat(CMD1, C, CMD2),
    concat(CMD2, ')\n', CMD).

build_cmd(OList, Calculus, CMD) :- 
    concat('qualify ', Calculus, CMD1),
    concat(CMD1, ' all (', CMD2),
    extract_objs(OList, C),
    concat(CMD2, C, CMD3),
    concat(CMD3, ')\n', CMD).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% transorms list of relations to predicates based on Calculus
% [rcc, a, dc, b] -> rcc(a, dc, b)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
map_qsr([X|[]], [R], Calculus) :- R =..[Calculus|X], !.
map_qsr([X|XS], [R|Rel], Calculus) :- 
    R =..[Calculus|X],  
    map_qsr(XS, Rel, Calculus).


my_write(Line) :-
    write(Line),nl.


