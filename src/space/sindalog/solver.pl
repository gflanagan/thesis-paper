%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SINDALOG: A Constraint Solver for Spatial Objects
% Developed for Jesús M. Almendros-Jimenez
% under SWI-Prolog and CHR
% University of Almeria. December, 2007.
% contact jalmen(at)ual.es
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:-use_module(library(chr)).




%%%%%%%%%%%%%%%%%%%%%%%%%
% LOADING AND WRITING SPATIAL DATA
%%%%%%%%%%%%%%%%%%%%%%%%%%

% load_points,write_points,write_file_points,
% load_segments, write_segments, write_file_segments
% read_number,read_list,read_file

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD POINTS (LOADING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_file(File,Set):-load_points(File,Set).



load_points(File,List_Data):-	open(File,read,Stream),                         	
				read_file(Stream,FileList),  
                        	load_points_aux(FileList,List_Data).
                        	 

load_points_aux(File,[(X,Y)|List_Data]):- File\=[],
						read_number(File,_,File2),
						read_number(File2,X,File3), 
						read_number(File3,Y,File4),
						load_points_aux(File4,List_Data).

load_points_aux([],[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WRITE POINTS (LOADING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_points(FileList,File):-
					open(File,write,Stream),
                 			write_file_points(Stream,FileList,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WRITE FILE POINTS (LOADING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


write_file_points(Stream,[(X,Y)|L],M):-
					write(Stream,M),
					write(Stream,' '),
					write(Stream,X),
					write(Stream,' '),
 					write(Stream,Y),
					write(Stream,'\n'),
					N is M+1,
					write_file_points(Stream,L,N).

write_file_points(Stream,[],_):-
					close(Stream).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD SEGMENTS (LOADING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_file_seg(File,Set):-load_segments(File,Set).

load_segments(File,List_Data):-	open(File,read,Stream),                         	
					read_file(Stream,FileList),  
                        		load_segments_aux(FileList,List_Data).
                        		 

load_segments_aux(File,[line([(X,Y),(Z,T)])|List_Data]):- File\=[],
					read_number(File,_,File2),
					read_number(File2,X,File3), 
					read_number(File3,Y,File4),
					read_number(File4,Z,File5), 
					read_number(File5,T,File6),
					load_segments_aux(File6,List_Data).

load_segments_aux([],[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WRITE SEGMENTS (LOADING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_segments(FileList,File):-
					open(File,write,Stream),
                 		        write_file_segments(Stream,FileList,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WRITE FILE POINTS (LOADING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


write_file_segments(Stream,[line([(X,Y),(Z,T)])|L],M):-
					write(Stream,M),
					write(Stream,' '),
					write(Stream,X),
					write(Stream,' '),
 					write(Stream,Y),					
					write(Stream,' '),
					write(Stream,Z),
					write(Stream,' '),
 					write(Stream,T),
                 		        write(Stream,'\n'),
					N is M+1,
					write_file_segments(Stream,L,N).

write_file_segments(Stream,[],_):-
					close(Stream).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% READ NUMBER (LOADING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_number(File,N,File2):-
			read_list(File,S,File2), 
			number_chars(N,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% READ LIST (LOADING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_list([X|File],[X|N],File2):-
			'0'@=<X,X@=<'9',!,
			read_list(File,N,File2).

read_list(['.'|File],['.'|N],File2):-!,
			read_list(File,N,File2).

read_list([' '|File],[],File):-!.

read_list([C,D|File],[],File):-char_code(C,13),char_code(D,10),!. %for mac/linux

%read_list([C|File],[],File):-!.  

read_list([],[],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% READ FILE (LOADING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_file(Stream,[]):-
		at_end_of_stream(Stream),!,
		close(Stream).

read_file(Stream,[X|L]):-
		get_char(Stream,X),
		read_file(Stream,L).


%---------------------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STORING DATA IN A RTREE 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  RTREE (STORING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rtree([],_,null).

rtree([X|L],K,Tree):-
			rtree(L,K,TreeL),
			insert_root(X,K,TreeL,Tree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSERT ROOT (STORING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_root(X,K,TreeL,node([(Mbr1,node(Tree1,P)),(Mbr2,node(Tree2,Q))],2)):-
			insert(X,K,TreeL,node(Tree,M)),
                    	M>K,!,
                    	P is M // 2, 
		        Q is M-P, 
		        split_index(Tree,P,Tree1,Tree2),                                        
                    	enclose_tree_mbr(Tree1,Mbr1),
                    	enclose_tree_mbr(Tree2,Mbr2).
                     

insert_root(X,K,TreeL,Tree):-
			insert(X,K,TreeL,Tree). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSERT (STORING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert(X,_,null,node([(MBr,objects([X],1)) ],1) ):-
		       mbr_obj(X,MBr).


insert(X,K,node(NodeList,M),Tree):-
			search(X,NodeList,(Mbr,Child)),!,
			insert(X,K,Child,NewChild),                   
			splitting((Mbr,NewChild),K,A,B),                  
                	substitute(NodeList,M,(Mbr,Child),A,B,Tree).

insert(X,K,node(NodeList,M),Tree):-
			new_mbr(X,NodeList,NodeList2),!,  
			insert(X,K,node(NodeList2,M),Tree).  
                                    

insert(X,_,objects([Y|L],M),objects([X,Y|L],S)):-
			S is M+1.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SEARCH (STORING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

search(X,[(Mbr,Child)|_],(Mbr,Child)):-			 
			inside_obj_mbr2(X,Mbr),!.

search(X,[_|Children],MbrChild):-
			search(X,Children,MbrChild).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NEW MBR (STORING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_mbr(X,Nodes1,Nodes2):-
		best_mbr(X,Nodes1,MbrOld,BestMbr),
          	substitute_mbr(Nodes1,MbrOld,BestMbr,Nodes2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BEST MBR (STORING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


best_mbr(X,[(Mbr,_)|Children],MbrOld,BestMbr):-
				enclose_obj_mbr(X,Mbr,MbrInit), 
				best_mbr(X,Children,Mbr,MbrInit,MbrOld,BestMbr).

best_mbr(X,[(Mbr,_)|Children],_,BestMbr,M,B):-
					enclose_obj_mbr(X,Mbr,MbrNew),
					dimension(MbrNew,KNew),
					dimension(BestMbr,KBest),
					lt(KNew,KBest),!,
					best_mbr(X,Children,Mbr,MbrNew,M,B).

best_mbr(X,[(_,_)|Children],MbrOld,BestMbr,M,B):-
					best_mbr(X,Children,MbrOld,BestMbr,M,B).

best_mbr(_,[],MbrOld,BestMbr,MbrOld,BestMbr).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSIDE OBJ MBR (STORING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


inside_obj_mbr2( O, mbr((A,B),(C,D)) ):-
			mbr_obj(O,mbr( (X,Y),(Z,T) )),
			leq(A,X),
			leq(X,C),
			leq(B,Y),
			leq(Y,D),
		    	leq(A,Z),
			leq(Z,C),
			leq(B,T),
			leq(T,D).
		

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MBR_OBJ (STORING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mbr_obj((X,Y),U) :- \+compound(X),\+compound(Y),mbr_point((X,Y),Z), Z=U.
mbr_obj(line(L),U):- mbr_line(line(L),Z), Z=U.
mbr_obj(polygon(L),U):-mbr_line(line(L),Z),Z=U.
mbr_obj(region(L),U):-mbr_line(line(L),Z),Z=U.
mbr_obj(diff(O,_),U):-mbr_obj(O,U).
mbr_obj((O1,O2),U):-mbr_obj(O1,M1),mbr_obj(O2,M2),union_mbr(M1,M2,U).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ENCLOSE_OBJ_MBR (STORING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enclose_obj_mbr(X,mbr(A,B),U):-
			mbr_obj(X,mbr(Z,T)),
			minimum_points2(Z,A,N),
			maximum_points2(T,B,M),
			U=mbr(N,M),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MINIMUM_POINTS2 (STORING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

minimum_points2((X,Y),(Z,T),U):- leq(X,Z), leq(Y,T),U=(X,Y),!.

  
minimum_points2((X,Y),(Z,T),U):- leq(X,Z), lt(T,Y), U=(X,T),!.

 
minimum_points2((X,Y),(Z,T),U):- lt(Z,X), leq(Y,T), U=(Z,Y),!.

 
minimum_points2((X,Y),(Z,T),U):- lt(Z,X), lt(T,Y), U=(Z,T),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAXIMUM_POINTS2 (STORING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maximum_points2((X,Y),(Z,T),U):- leq(X,Z), leq(Y,T), U=(Z,T),!.

  
maximum_points2((X,Y),(Z,T),U):- leq(X,Z),lt(T,Y),U=(Z,Y),!.

  
maximum_points2((X,Y),(Z,T),U):- lt(Z,X),leq(Y,T),U=(X,T),!.


maximum_points2((X,Y),(Z,T),U):- lt(Z,X),lt(T,Y),U=(X,Y),!.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DIMENSION (STORING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dimension(mbr(A,B),K):-
			A=(A1,A2),B=(B1,B2), 
			A1\=B1, A2\=B2, 
			K is (B1-A1)*(B2-A2),!.

dimension(mbr(A,B),K):-
			A=(A1,_),B=(B1,_), 
			A1\=B1, K is B1-A1,!.

dimension(mbr(A,B),K):-
			A=(_,A2),B=(_,B2), 
			A2\=B2, K is B2-A2,!.

dimension(mbr(_,_),0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUBSTITUTE MBR (STORING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

substitute_mbr([(X,C)|L],X,Y,[(Y,C)|L]):-!.

substitute_mbr([X|L],Z,Y,[X|L2]):-
			substitute_mbr(L,Z,Y,L2).

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SPLITTING (STORING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

splitting((Mbr,node(NodeList,M)),K,(Mbr,node(NodeList,M)),null):-
					geq(K,M),!.

splitting((_,node(NodeList,M)),_,(Mbr1,node(Node1,P)),(Mbr2,node(Node2,Q))):-!,
				P is M // 2, 
				Q is M-P, 
				split_index(NodeList,P,Node1,Node2),
            			enclose_tree_mbr(Node1,Mbr1),
            			enclose_tree_mbr(Node2,Mbr2).

splitting((Mbr,objects(LP,M)),K,(Mbr,objects(LP,M)),null):- 
					geq(K,M),!.

splitting((_,objects(LP,M)),_,(Mbr1,objects(LP1,P)),(Mbr2,objects(LP2,Q))):-!,
            			P is M // 2,
            			Q is  M-P,
            			split_index(LP,P,LP1,LP2),
            			enclose_tree_mbr(LP1,Mbr1),
            			enclose_tree_mbr(LP2,Mbr2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUBSTITUTE (STORING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


substitute(NodeList,M,N,N1,null,node(NodeList2,M)):-!,
			            substitute_list(NodeList,N,N1,null,NodeList2).

substitute(NodeList,M,N,N1,N2,node(NodeList2,K)):- 
				K is M+1, 
				substitute_list(NodeList,N,N1,N2,NodeList2).

substitute_list([N|L],N,N1,null,[N1|L]):-!.

substitute_list([N|L],N,N1,N2,[N1,N2|L]):-!.

substitute_list([N|L],M,M1,M2,[N|L2]):-
					substitute_list(L,M,M1,M2,L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MBR (STORING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enclose_tree_mbr([(mbr(A,B),_)|RMbr],Mbr):-!,
				enclose_tree_mbr(RMbr,A,B,Mbr).

enclose_tree_mbr([X|RMbr],Mbr):-mbr_obj(X,mbr(U,V)),
				enclose_tree_mbr(RMbr,U,V,Mbr).

enclose_tree_mbr([],A,B,mbr(A,B)):-!.

enclose_tree_mbr([(mbr(A,B),_)|RMbr],C,D,Mbr):-!,
				minimum_points2(A,C,X),
				maximum_points2(B,D,Y),
				enclose_tree_mbr(RMbr,X,Y,Mbr).

enclose_tree_mbr([X|RMbr],C,D,Mbr):- 
				mbr_obj(X,mbr(U,V)),
				minimum_points2(U,C,AA),
				maximum_points2(V,D,BB),
				enclose_tree_mbr(RMbr,AA,BB,Mbr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SPLIT INDEX (STORING)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split_index(L,0,[],L):-!.

split_index([X|L],P,[X|L2],L3):-
					P>0,
					Q is P-1,
					split_index(L,Q,L2,L3).






%---------------------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOCAL CONSISTENCE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



mbr_bound1 @ mbr_bound(S,MBr1),mbr_bound(S,MBr2) <=> 
			MBr1\==MBr2 | 
			union_mbr(MBr1,MBr2,MBr3), 
			mbr_bound(S,MBr3).

mbr_bound2 @ in_set(O,S), mbr_bound(S,MBr) ==> inside_obj_mbr(O,MBr).


mbr_bound_first1 @ mbr_bound_first(S,MBr1),mbr_bound_first(S,MBr2) <=> 
				MBr1\==MBr2 | 
				union_mbr(MBr1,MBr2,MBr3), 
				mbr_bound_first(S,MBr3).

mbr_bound_second1 @ mbr_bound_second(S,MBr1),mbr_bound_second(S,MBr2) <=> 
				MBr1\==MBr2 | 
				union_mbr(MBr1,MBr2,MBr3), 
				mbr_bound_second(S,MBr3).

mbr_bound_first2 @ in_pairs((O1,_),PS), mbr_bound_first(PS,Mbr) ==> inside_obj_mbr(O1,Mbr).

mbr_bound_second2 @ in_pairs((_,O2),PS), mbr_bound_second(PS,Mbr) ==> inside_obj_mbr(O2,Mbr).
 


lower_bound1 @ lower_bound(D,A),lower_bound(D,E) <=> 
						ground(A),
						ground(E),
						leq(A,E) 
						 
					| 
						lower_bound(D,A).

upper_bound1 @ upper_bound(D,B),upper_bound(D,F) <=> 
						ground(B),
						ground(F),
					     	leq(B,F) 
					| 
						upper_bound(D,F).

lower_bound2 @ lower_bound(D,Lower) ==> leq(D,Lower).

upper_bound2 @ upper_bound(D,Upper) ==> geq(D,Upper).


dist_bound_min1 @ dist_bound_min(PS,PR1,A),dist_bound_min(PS,PR2,E) <=> 
					ground(A),
					ground(E),
					ground(PR1),
					ground(PR2), 
					leq(A,E), 
					filtermin_pairs(PR2,A,PR3), 
					append(PR1,PR3,PR)
				| 
					dist_bound_min(PS,PR,A).

dist_bound_max1 @ dist_bound_max(PS,PR1,B),dist_bound_max(PS,PR2,F) <=> 
					ground(B),
					ground(F), 
					ground(PR1),
					ground(PR2), 
					leq(B,F),  
            				filtermax_pairs(PR1,F,PR3), 
					append(PR2,PR3,PR)
				| 
					dist_bound_max(PS,PR,F).

dist_bound_min2 @ dist_bound_min(PS,_,DIST), in_pairs((O1,O2),PS) ==> 
					mindist(O1,O2,D), leq(D,DIST).
dist_bound_max1 @ dist_bound_max(PS,_,DIST), in_pairs((O1,O2),PS) ==> 
					maxdist(O1,O2,D), geq(D,DIST).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EQUALITY CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- chr_constraint eq/2,eqp/2,eqobj/2,neq/2,neqp/2,neqobj/2,eqlist/2,eqobjlist/2.
:- chr_constraint eqn/2,neqn/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EQ (EQUALITY)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eq1 @ eq(O1,O2), neq(O1,O2) <=> fail.
eq2 @ eq(O1,O2) <=> eqobj(O1,O2).

 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NEQ (EQUALITY)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


neq @ neq(O1,O2) <=> ground(O1),ground(O2) | neqobj(O1,O2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EQ2, NEQ2 LOGIC (EQUALITY)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eq2(O1,O2):-eqobj(O1,O2),!.
neq2(O1,O2):-neqobj(O1,O2),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EQP (EQUALITY)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eqp1 @ eqp(P1,P2),neqp(P1,P2) <=> fail.
eqp2 @ eqp((O1,O2),(O3,O4)) <=> 
				eq(O1,O3), 
				eq(O2,O4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NEQP (EQUALITY)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


neqp @ neqp((O1,O2),(O3,O4)) <=> 
				neq(O1,O3); 
				neq(O2,O4).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EQOBJ (EQUALITY) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eqobj1 @ eqobj(O1,O2), neqobj(O1,O2) <=> fail.

eqobj2 @ eqobj(X,O)  <=> var(X) | X=O.
eqobj3 @ eqobj(O,X)  <=> var(X) | X=O.
eqobj4 @ eqobj((X,Y),(Z,T))<=> eqn(X,Z),eqn(Y,T).
eqobj5 @ eqobj(line(L1),line(L2)) <=> L1=L2.
eqobj6 @ eqobj(polygon(L1),polygon(L2)) <=> L1=L2.
eqobj7 @ eqobj(region(L1),region(L2)) <=> L1=L2.
eqobj8 @ eqobj(diffo(O1,O2),diffo(O3,O4)) <=> O1=O3,O2=O4.
eqobj9 @ eqobj(_,_) <=> fail.

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NEQOBJ (EQUALITY)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

neqobj @ neqobj(O1,O2)<=> (eqobj(O1,O2)->fail;true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EQN (EQUALITY)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eqn1 @ eqn(N1,N2), neqn(N1,N2) <=> fail.
eqn2 @ eqn(N1,N2) <=> ground(N1),ground(N2), float(N1)=:=float(N2) | true.
eqn3 @ eqn(N1,N2) <=> N1=N2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NEQN (EQUALITY)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

neqn @ neqn(N1,N2) <=> (eqn(N1,N2)->fail;true).



%---------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MEMBERSHIP CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 
:- chr_constraint in_o/2, nin_o/2, in_set/2, nin_set/2, in_pairs/2, nin_pairs/2,  
	look_obj/2, in_node/2,
	inside_obj_mbr/2,inside_mbr_mbr/2,inside_point_mbr/2.
	
:- chr_constraint in_int/2,nin_int/2,leq/2,lt/2,geq/2,gt/2.

:- chr_constraint print/0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IN_O (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

in_o1 @ in_o(P,O), nin_o(P,O) <=> fail.
in_o2 @ in_o((X,Y),O) <=> 	ground((X,Y)),ground(O),
					mbr(O,MBr),
					inside_point_mbr((X,Y),MBr) 
					| 
					inside_point_obj((X,Y),O).

in_o3 @ in_o((X,Y),O) <=> ground((X,Y)),ground(O) 
					| 
					fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NIN_O (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nin_o @ nin_o((X,Y),O) <=> ground((X,Y)),ground(O) 
					| 
					(in_o((X,Y),O)->fail;true).
				 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IN_SET (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

in_set1 @ in_set(O,S), nin_set(O,S) <=> fail.


in_set2 @ in_set(O,node(L,K)) <=> 
				root_nodes(node(L,K),N), 			 
				in_node(O,N).

in_set3 @ in_set(O,objects(L,_)) <=> 	 
				look_obj(O,L).

in_set4 @ in_set(_,[]) <=> fail.

in_set5 @ in_set(O,[X|L]) <=>   ground([X|L]), 
				rtree([X|L],5,R) 
				|
				in_set(O,R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NIN_SET (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nin_set @ nin_set(O,S) <=> ground(S) 
			| 
			(in_set(O,S)->fail;true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IN_PAIRS (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

in_pairs1 @ in_pairs(O,P), nin_pairs(O,P) <=> fail.

in_pairs2 @ in_pairs(_,[]) <=> fail.

in_pairs3 @ in_pairs(O,[X|L]) <=> ground([X|L]), 	
					rtree([X|L],5,R) 
					|
					in_set(O,R).

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NIN_PAIRS (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



nin_pairs @ nin_pairs(O,P) <=> 	ground(P) 
				| 
				(in_pairs(O,P)->fail;true).



%%%%%%%%%%%%%%%%%%%%%%%%%
% LOOK_OBJ (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%

look_obj1 @ look_obj(_,[]) <=> fail.

look_obj2 @ look_obj(X,[Y|L]) <=> X=Y; look_obj(X,L).

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INNODE (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

in_node @ in_node(O,(M,Child)) <=> ground(O),
				inside_obj_mbr(O,M) |
				in_set(O,Child).

in_node @ in_node(O,(_,Child)) <=> \+ground(O) | 
				in_set(O,Child).

in_node @ in_node(_,_) <=> fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSIDE_OBJ_MBR (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inside_obj_mbr @ inside_obj_mbr(O,mbr(A,B)) <=> 
					      mbr(O,MBr) |			
                			inside_mbr_mbr(MBr,mbr(A,B)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSIDE MBR MBR (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inside_mbr_mbr @ inside_mbr_mbr(mbr(C,D),mbr(A,B)) <=>
					cox_point(C,COXC),coy_point(C,COYC),			
					cox_point(D,COXD),coy_point(D,COYD) |
					inside_point_mbr((COXC,COYC),mbr(A,B)),		
					inside_point_mbr((COXD,COYD),mbr(A,B)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSIDE POINT MBR (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inside_point_mbr @ inside_point_mbr((X,Y),mbr((A,B),(C,D))) <=>
							leq(A,X),
							leq(X,C),
							leq(B,Y),
							leq(Y,D).		

 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IN_INT (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

in_int @ in_int(N,[N1,N2]) <=> leq(N1,N),leq(N,N2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NIN_INT (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nin_int @ nin_int(N,[N1,N2]) <=> gt(N1,N);gt(N,N2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LEQ (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

leq1 @ leq(X,Y) <=>  number(X), number(Y),X=<Y | true.

leq2 @ leq(X,Y) <=>  number(X), number(Y),Y<X | fail.

leq3 @ leq(X,Y) <=> eqn(X,Y) | true.

leq4 @ leq(X,Y), leq(Y,X) <=>  eqn(X,Y).

leq5 @ leq(X,Y), leq(Y,Z) ==> leq(X,Z).

leq6 @ leq(X,Y), leq(X,Z) <=> number(Y),number(Z),Y=<Z | leq(X,Y).

leq7 @ leq(Y,X), leq(Z,X) <=> number(Y),number(Z),Y=<Z | leq(Z,X).

leq8 @ lt(Z,X), leq(X,Y) <=> number(Y),number(Z),Y=<Z | fail.

leq9 @ leq(Z,X), lt(X,Y) <=> number(Y),number(Z),Y=<Z | fail.

leq10 @ lt(Z,X), lt(X,Y) <=> number(Y),number(Z),Y=<Z | fail.

leq11 @ leq(Z,X), leq(X,Y) <=> number(Y),number(Z),Y<Z | fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LT (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lt1 @ lt(X,Y) <=>  number(X), number(Y), X<Y | true.

lt2 @ lt(X,Y) <=>  number(X), number(Y), Y=<X | fail.

lt3 @ lt(X,Y) <=>  eqn(X,Y) | fail.

lt4 @ lt(X,Y), lt(X,Z) <=> number(Y),number(Z),Y=<Z | lt(X,Y).

lt5 @ lt(Y,X), lt(Z,X) <=> number(Y),number(Z),Y=<Z | lt(Z,X).
lt6 @ lt(X,Y), lt(Y,X) <=> fail.

lt7 @ lt(X,Y), lt(Y,Z) ==> lt(X,Z).

lt8 @ lt(X,Y), leq(Y,Z) ==> lt(X,Z).

lt9 @ lt(X,Y) ==>  neqn(X,Y).

lt10 @ leq(X,Y), lt(X,Z) <=> number(Y),number(Z),Y=<Z | leq(X,Y).

lt11 @ leq(Y,X), lt(Z,X) <=> number(Y),number(Z),Y=<Z | lt(Z,X).

lt12 @ lt(X,Y), leq(X,Z) <=> number(Y),number(Z),Y=<Z | lt(X,Y).

lt13 @ lt(Y,X), leq(Z,X) <=> number(Y),number(Z),Y=<Z | leq(Z,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GEQ (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

geq @ geq(X,Y) <=> leq(Y,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GT (MEMBERSHIP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gt @ gt(X,Y) <=> lt(Y,X).


%----------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRINT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print, leq(X,Y) <=> print, write(X),write('<='),write(Y),write(' ').
print, lt(X,Y) <=> print, write(X),write('<'),write(Y),write(' ').
print <=> true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SET CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 
:-chr_constraint union_obj/3, inter_obj/3, diff_obj/3,  diff_obj/4.
:-chr_constraint inside_point_obj/2,inside_point_line/2, inside_point_segment/3. 			
:- chr_constraint inside_line_obj/2,inside_line_line/2,inter_line_region/3,
	check_intersection_region/4.
:- chr_constraint inter_lines/3, inside_point_region/2.
		 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UNION OBJ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		

union_obj @ in_set(O,S), union_obj(S1,S2,S) <=> var(S) | in_set(O,[S1]);in_set(O,[S2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERSECTION OBJ (SET)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 
inter_obj1 @ inter_obj((X,Y),(Z,T),S) <=> eqn(X,Z),eqn(Y,T) | S=[(X,Y)].

inter_obj2 @ inter_obj((_,_),(_,_),S) <=> S=[].

inter_obj3 @ inter_obj((X,Y),line(L),S) <=> 
			(overlap((X,Y),line(L)),inside_point_obj((X,Y),line(L)) -> 
			S=[(X,Y)]; S=[]).

inter_obj4 @ inter_obj(line(L),(X,Y),S) <=> inter_obj((X,Y),line(L),S).

inter_obj5 @ inter_obj(line(L1),line(L2),S) <=> 
			overlap(line(L1),line(L2)) -> inter_lines(L1,L2,S); S=[].

inter_obj6 @ inter_obj((X,Y),polygon([P1|L]),S) <=> 
			append([P1|L],[P1],L2),
			(overlap((X,Y),line(L2)),inside_point_obj((X,Y),line(L2)) -> 
			S=[(X,Y)]; S=[]).

inter_obj7 @ inter_obj(polygon(L),(X,Y),S) <=> inter_obj((X,Y),polygon(L),S).

inter_obj8 @ inter_obj(line(L1),polygon([P1|L2]),S) <=> 
			append([P1|L2],[P1],L3),
			overlap(line(L1),line(L3)) -> 
			inter_lines(L1,L3,S); S=[].

inter_obj9 @ inter_obj(polygon(L1),line(L2),S) <=> inter_obj(line(L2),polygon(L1),S).

inter_obj10 @ inter_obj(polygon([P1|L1]),polygon([P2|L2]),S) <=>
			append([P1|L1],[P1],LP1),
			append([P2|L2],[P2],LP2),
			overlap(line(LP1),line(LP2))->
			inter_lines(LP1,LP2,S);S=[].

inter_obj11 @ inter_obj((X,Y),region(L),S) <=> inside_point_obj((X,Y),region(L)) ->
				S=[(X,Y)]; S=[].

inter_obj12 @ inter_obj(region(L),(X,Y),S) <=> inter_obj((X,Y),region(L),S).

inter_obj13 @ inter_obj(line(L1),region(L2),S) <=> overlap(line(L1),region(L2)) -> 					
				inter_line_region(L1,region(L2),S);S=[].

inter_obj14 @ inter_obj(region(L2),line(L1),S) <=> inter_obj(line(L1),region(L2),S).

inter_obj15 @ inter_obj(polygon([P|L1]),region(L2),S) <=> 
			(overlap(polygon([P|L1]),region(L2)) -> 	
			append([P|L1],[P],L3),
			inter_line_region(L3,region(L2),S);S=[]).

inter_obj16 @ inter_obj(polygon(L1),region(L2),S) <=> inter_obj(region(L2),polygon(L1),S).

inter_obj17 @ inter_obj(region([P1|L1]),region(L2),S) <=> 
			(overlap(region([P1|L1]),region(L2))->
			append([P1|L1],[P1],L3),
			inter_line_region(L3,region(L2),S1),
			add_new_nodes(S1,L2,NP),
                        remove_points(NP,region([P1|L1]),RES),
			S=[region(RES)];
			S=[]).

 			
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DIFFERENCE OBJ (SET)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 

diff_obj1 @ diff_obj(O1,O2,O3) <=> diff_obj(O1,O2,F,H),O3=diffo(F,H).


diff_obj2 @ diff_obj((X,Y),(Z,T),OBJ,HOLES) <=> eqn(X,Z),eqn(Y,T) | OBJ=(X,Y),HOLES=[(X,Y)].

diff_obj3 @ diff_obj((X,Y),(_,_),OBJ,HOLES) <=> OBJ=(X,Y),HOLES=[].

diff_obj4 @ diff_obj((X,Y),line(L),OBJ,HOLES) <=> 
					(overlap((X,Y),line(L)),inside_point_obj((X,Y),line(L)) -> 
					OBJ=(X,Y),HOLES=[(X,Y)]); OBJ=(X,Y),HOLES=[].

diff_obj5 @ diff_obj((X,Y),polygon([P1|L]),OBJ,HOLES) <=>
 				append([P1|L],[P1],L2),
				(overlap((X,Y),line(L2)),inside_point_obj((X,Y),line(L2)) -> 
				OBJ=(X,Y),HOLES=[(X,Y)]); OBJ=(X,Y),HOLES=[].

diff_obj6 @ diff_obj((X,Y),region([P1|L]),OBJ,HOLES) <=>
 				 
				(overlap((X,Y),region([P1|L])),
				inside_point_obj((X,Y),region([P1|L])) -> 
				OBJ=(X,Y),HOLES=[(X,Y)]); OBJ=(X,Y),HOLES=[].


diff_obj7 @ diff_obj(line(L),(X,Y),OBJ,HOLES) <=>  
				(overlap((X,Y),line(L)),inside_point_obj((X,Y),line(L)) -> 
				OBJ=line(L),HOLES=[(X,Y)]); OBJ=line(L),HOLES=[].

diff_obj8 @ diff_obj(polygon([P1|L]),(X,Y),OBJ,HOLES) <=>
				append([P1|L],[P1],L2),  
				(overlap((X,Y),line(L2)),inside_point_obj((X,Y),line(L2)) -> 
				OBJ=polygon([P1|L]),HOLES=[(X,Y)]); 
				OBJ=polygon([P1|L]),HOLES=[].

diff_obj9 @ diff_obj(region([P1|L]),(X,Y),OBJ,HOLES) <=>
				   
				(overlap((X,Y),region([P1|L])),inside_point_obj((X,Y),
				region([P1|L])) -> 
				OBJ=region([P1|L]),HOLES=[(X,Y)]); 
				OBJ=region([P1|L]),HOLES=[].


diff_obj10 @ diff_obj(line(L1),line(L2),OBJ,HOLES) <=> 
				overlap(line(L1),line(L2)) -> inter_lines(L1,L2,S),
				OBJ=line(L1),HOLES=S;
				OBJ=line(L1),HOLES=[].

diff_obj11 @ diff_obj(line(L1),polygon([P1|L2]),OBJ,HOLES) <=>
				append([P1|L2],[P1],L3), 
				overlap(line(L1),line(L3)) -> inter_lines(L1,L3,S),
				OBJ=line(L1),HOLES=S;
				OBJ=line(L1),HOLES=[].

diff_obj12 @ diff_obj(line(L1),region([P1|L2]),OBJ,HOLES) <=>
				  
				overlap(line(L1),region([P1|L2])) -> 
				inter_line_region(L1,region([P1|L2]),S),
				OBJ=line(L1),HOLES=S;
				OBJ=line(L1),HOLES=[].

diff_obj13 @ diff_obj(polygon([P1|L1]),line(L2),OBJ,HOLES) <=>
				append([P1|L1],[P1],L3), 
				overlap(line(L3),line(L2)) -> inter_lines(L3,L2,S),
				OBJ=polygon([P1|L1]),HOLES=S;
				OBJ=polygon([P1|L1]),HOLES=[].

diff_obj14 @ diff_obj(polygon([P1|L1]),polygon([P2|L2]),OBJ,HOLES) <=>
				append([P1|L1],[P1],L3),
				append([P2|L2],[P2],L4), 
				overlap(line(L3),line(L4)) -> inter_lines(L3,L4,S),
				OBJ=polygon([P1|L1]),HOLES=S;
				OBJ=polygon([P1|L1]),HOLES=[].

diff_obj15 @ diff_obj(polygon([P1|L1]),region([P2|L2]),OBJ,HOLES) <=>
				append([P1|L1],[P1],L3),
				append([P2|L2],[P2],L4), 
				overlap(line(L3),region([P2|L2])) -> inter_lines(L3,L4,S),
				OBJ=polygon([P1|L1]),HOLES=S;
				OBJ=polygon([P1|L1]),HOLES=[].

diff_obj16 @ diff_obj(region([P1|L2]),line(L1),OBJ,HOLES) <=>
				 
				overlap(line(L1),region([P1|L2])) -> 
				inter_line_region(L1,region([P1|L2]),S),
				OBJ=region([P1|L2]),HOLES=S;
				OBJ=region([P1|L2]),HOLES=[].

diff_obj17 @ diff_obj(region([P1|L1]),polygon([P2|L2]),OBJ,HOLES) <=>
				 
				append([P2|L2],[P2],L4), 
				overlap(line(L4),region([P1|L1])) -> 
				inter_line_region(L4,region([P1|L1]),S),
				OBJ=region([P1|L1]),HOLES=S;
				OBJ=region([P1|L1]),HOLES=[].

diff_obj18 @ diff_obj(region([P1|L1]),region(L2),OBJ,HOLES) <=>
				 append([P1|L1],[P1],L3),
			         overlap(region([P1|L1]),region(L2)) ->
				 inter_line_region(L3,region(L2),S1),
			         add_new_nodes(S1,L2,NP), 
                                 remove_points(NP,region([P1|L1]),RES),
				 OBJ=region([P1|L1]),HOLES=region(RES);
				 OBJ=region([P1|L1]),HOLES=[].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTER LINES (SET)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


inter_lines1 @ inter_lines([P1,P2|RP],L,S) <=> 
			intersection_segment_line([P1,P2],L,POINTS),
			inter_lines([P2|RP],L,S2),
			append_clean(POINTS,S2,S).
			 
inter_lines2 @ inter_lines([_],_,S) <=> S=[].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% APPEND POINTS (SET)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
append_clean(POINTS,[X|L],RP):-
		member(Y,POINTS),
		inside_obj(X,Y),!,
		append_clean(POINTS,L,RP).

append_clean(POINTS,[X|L],[X|RP]):-
		member(Y,POINTS),
		inside_obj(Y,X),!,
		delete(POINTS,Y,POINTS2),
		append_clean(POINTS2,L,RP).

append_clean(POINTS,[X|L],[X|RP]):-
		append_clean(POINTS,L,RP).

append_clean(POINTS,[],POINTS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DELETE (SET)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delete([],_,[]).
delete([X|L],Y,L2):-eq2(X,Y),!,delete(L,Y,L2).
delete([X|L],Y,[X|L2]):-delete(L,Y,L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSIDE POINT OBJ (SET)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inside_point_obj1 @ inside_point_obj((X,Y),(Z,T)) <=> eqn(X,Z), eqn(Y,T).

inside_point_obj2 @ inside_point_obj((X,Y),line(L)) <=> inside_point_line((X,Y),L).

inside_point_obj3 @ inside_point_obj((X,Y),polygon([P1|L])) <=>
					append([P1|L],[P1],L2), 
					inside_point_line((X,Y),L2).

inside_point_obj4 @ inside_point_obj((X,Y),region(L)) <=>  
					inside_point_region((X,Y),region(L)).


inside_point_obj5 @ inside_point_obj((X,Y),diff(O1,O2)) <=>
					inside_point_obj((X,Y),O1),
					\+inside_point_obj((X,Y),O2).
					


 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSIDE POINT LINE (SET)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


inside_point_line1 @ inside_point_line((X,Y),[P1,P2]) <=> inside_point_segment((X,Y),P1,P2).

inside_point_line2 @ inside_point_line((X,Y),[P1,P2,P3|RP]) <=> 
				inside_point_segment((X,Y),P1,P2);
				inside_point_line((X,Y),[P2,P3|RP]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSIDE POINT SEGMENT (SET)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


inside_point_segment1 @	inside_point_segment((X,Y),(Z,T),(U,V)) <=> 
					VD1 is Z-U, VD2 is T-V, neqn(VD1,0), neqn(VD2,0), 
					D1 is X-Z, D2 is Y-T, 
					leq(Z,X), leq(X,U), leq(T,Y), leq(Y,V)|
					VD1*D2 =:= VD2*D1.

inside_point_segment2 @ inside_point_segment((X,Y),(Z,T),(U,V)) <=> 
					VD1 is Z-U, VD2 is T-V, neqn(VD1,0), neqn(VD2,0), 
					D1 is X-Z, D2 is Y-T, 
					leq(U,X), leq(X,Z), leq(T,Y), leq(Y,V) |
					VD1*D2 =:= VD2*D1.
inside_point_segment3 @ inside_point_segment((X,Y),(Z,T),(U,V)) <=> 
					VD1 is Z-U, VD2 is T-V, neqn(VD1,0), neqn(VD2,0), 
					D1 is X-Z, D2 is Y-T, 
					leq(Z,X), leq(X,U), leq(V,Y), leq(Y,T)|
					D1*D2 =:= VD2*D1.
inside_point_segment4 @ inside_point_segment((X,Y),(Z,T),(U,V)) <=> 
					VD1 is Z-U, VD2 is T-V, neqn(VD1,0), neqn(VD2,0), 
					D1 is X-Z, D2 is Y-T,					
					leq(U,X), leq(X,Z), leq(V,Y), leq(Y,T) |
					VD1*D2 =:= VD2*D1.
inside_point_segment5 @ inside_point_segment((X,Y),(Z,T),(U,V)) <=> VD1 is Z-U,  
			eqn(VD1,0), leq(T,Y), leq(Y,V) |	
			X=:=Z.

inside_point_segment6 @ inside_point_segment((X,Y),(Z,T),(U,V)) <=> VD1 is Z-U, 
			eqn(VD1,0), leq(V,Y), leq(Y,T)  | 
			X=:=Z.

inside_point_segment7 @ inside_point_segment((X,Y),(Z,T),(U,V)) <=> VD2 is T-V,   
			eqn(VD2,0), leq(Z,X), leq(X,U) |
			Y=:=T.

inside_point_segment8 @ inside_point_segment((X,Y),(Z,T),(U,V)) <=> VD2 is T-V,  
			eqn(VD2,0), leq(U,X), leq(X,Z) |
			Y=:=T.

inside_point_segment9 @ inside_point_segment((_,_),(_,_),(_,_)) <=> fail.
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSIDE LINE OBJ (SET)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inside_line_obj1 @ inside_line_obj(line(_),(_,_)) <=> fail.

inside_line_obj2 @ inside_line_obj(line(L1),line(L2)) <=> 
				inside_line_line(L1,L2).

inside_line_obj3 @ inside_line_obj(line(L1),polygon([P|L2])) <=> 
				append([P|L2],[P],L3),
				inside_line_line(L1,L3).

inside_line_obj4 @ inside_line_obj(line(L1),region(L2)) <=> 
				inside_line_region(L1,region(L2)).

inside_line_obj5 @ inside_line_obj(line(L1),diff(O1,O2)) <=>
				inside_line_obj(line(L1),O1),
				\+overlap_obj(line(L1),O2).
			

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSIDE LINE LINE (SET)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


inside_line_line1 @ inside_line_line([P|RP],L) <=> 
					inside_point_line(P,L),
					inside_line_line(RP,L).

inside_line_line2 @ inside_line_line([],_) <=> true.
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSIDE POINT REGION (SET)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inside_point_region1 @ inside_point_region((X,Y),region([P|L])) <=> 
			inside_point_obj((X,Y),polygon([P|L])) | true.
 
inside_point_region2 @ inside_point_region((X,Y),region([P|L])) <=> 
        		mbr((X,Y),MBrP),
			mbr(region([P|L]),mbr((A,B),(C,D))),
			overlap_mbr(MBrP,mbr((A,B),(C,D))),
			append([P|L],[P],L2),
			intersection_segment_line([(X,Y),(C,D)],L2,POINTS1),
			intersection_segment_line([(X,Y),(A,B)],L2,POINTS2),
			check_odd(POINTS1,(C,D),false,0),
			check_odd(POINTS2,(A,B),false,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CHECK ODD (SET)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


check_odd([],(_,_),_,N):-L is N mod 2, 
			L=1.

check_odd([(X,Y)|RP],(P1,P2),false,N):-
			eqn(X,P1),
			eqn(Y,P2),!,
			M is N+1, 
			check_odd(RP,(P1,P2),true,M).

check_odd([(X,Y)|RP],(P1,P2),true,N):-
			eqn(X,P1),
			eqn(Y,P2),!,
			check_odd(RP,(P1,P2),true,N).

check_odd([_|RP],(P1,P2),Bool,N):-
			M is N+1, 
			check_odd(RP,(P1,P2),Bool,M).
										  

%%%%%%%%%%%%%%%%%%%%%%%% 
% INTER LINE REGION (SET)
%%%%%%%%%%%%%%%%%%%%%%%%

inter_line_region1 @ inter_line_region([],region(_),S) <=> S=[].
inter_line_region2 @ inter_line_region([P1,P2|RP],region(L),S) <=> 
			inside_line_region([P1,P2],region(L)) 
			|
			inter_line_region([P2|RP],region(L),S2),
			S=[line([P1,P2])|S2].

inter_line_region3 @ inter_line_region([P1,P2|RP],region([P|L]),S) <=> 
			inside_point_obj(P1,region([P|L])) 
			|
	  		append([P|L],[P],L2),
			inter_lines([P1,P2],L2,S3), 
			check_intersection_region(P1,S3,region([P|L]),S4),
			inter_line_region([P2|RP],region([P|L]),S2),
			append_clean(S4,S2,S).

inter_line_region4 @ inter_line_region([P1,P2|RP],region([P|L]),S) <=> 
			inside_point_obj(P2,region([P|L])) 
			|
	  		append([P|L],[P],L2),
			inter_lines([P2,P1],L2,S3), 
			check_intersection_region(P2,S3,region([P|L]),S4),
			inter_line_region([P2|RP],region([P|L]),S2),
			append_clean(S4,S2,S).

inter_line_region5 @ inter_line_region([P1,P2|RP],region([P|L]),S) <=> 
	  		append([P|L],[P],L2),
			inter_lines([P1,P2],L2,S3),
			inter_line_region([P2|RP],region([P|L]),S2),
			append_clean(S3,S2,S).

inter_line_region6 @ inter_line_region([_|RP],region(L),S) <=>
			inter_line_region(RP,region(L),S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CHECK INTERSECTION REGION  (SET)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 

check_intersection_region1 @ check_intersection_region((X,Y),[(Z,T)|RP],region(L),S) <=> 
				neq2((X,Y),(Z,Y)), 
				inside_line_region([(X,Y),(Z,T)],region(L))
				|							
				check_intersection_region((X,Y),RP,region(L),S1), 
				S=[line([(X,Y),(Z,T)])|S1].

check_intersection_region2 @ check_intersection_region((X,Y),[(Z,T)|RP],region(L),S) <=> 
				check_intersection_region((X,Y),RP,region(L),S1),
				S=[(Z,T)|S1].

check_intersection_region3 @ check_intersection_region((X,Y),[line(L2)|RP],region(L),S) <=> 
				 inside_line_region([(X,Y)|L2],region(L)) 
				|
				check_intersection_region((X,Y),RP,region(L),S1),
				(inside_point_obj((X,Y),line(L2)) -> 
				S=[line(L2)|S1];
				S=[line([(X,Y)|L2])|S1]).

check_intersection_region4 @ check_intersection_region((X,Y),[line(L2)|RP],region(L),S) <=> 
				check_intersection_region((X,Y),RP,region(L),S1),
				S=[line(L2)|S1].

check_intersection_region5 @ check_intersection_region((_,_),[],region(_),S) <=> S=[].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REMOVE POINTS (SET)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


remove_points([P|RP],region(L),[P|RNP]):-
			inside_point_obj(P,region(L)),!,
			remove_points(RP,region(L),RNP).
remove_points([_|RP],region(L),RNP):-
			remove_points(RP,region(L),RNP).
remove_points([],_,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADD NEW NODES (SET)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

add_new_nodes([],S,S).

add_new_nodes([O|RP],L2,S):-add_new_element(O,L2,S1),
				add_new_nodes(RP,S1,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADD NEW ELEMENT (SET)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_new_element((X,Y),[P1,P2|RP],[P1,(X,Y)|RP2]):-
		neq(P1,(X,Y)),neq(P2,(X,Y)),inside_obj((X,Y),line([P1,P2])),!,
		add_new_element((X,Y),[P2|RP],RP2).
add_new_element(line([(X,Y),(Z,T)]),[P1,P2|RP],[P1,(X,Y),(Z,T)|RP2]):-
		neq(P1,(X,Y)),neq(P2,(X,Y)),neq(P1,(Z,T)),neq(P2,(Z,T)),
		inside_obj(line([(X,Y),(Z,T)]),line([P1,P2])),!,
		add_new_element((X,Y),[P2|RP],RP2).
add_new_element(line([(X,Y),(Z,T)]),[P1,P2|RP],[P1,(X,Y)|RP2]):-
		neq(P1,(X,Y)),neq(P2,(X,Y)),
		inside_obj(line([(X,Y),(Z,T)]),line([P1,P2])),!,
		add_new_element((X,Y),[P2|RP],RP2).
add_new_element(line([(X,Y),(Z,T)]),[P1,P2|RP],[P1,(Z,T)|RP2]):-
		neq(P1,(Z,T)),neq(P2,(Z,T)),
		inside_obj(line([(X,Y),(Z,T)]),line([P1,P2])),!,
		add_new_element((X,Y),[P2|RP],RP2).
add_new_element(O,[P1,P2|RP],[P1|RNP]):-add_new_element(O,[P2|RP],RNP).
add_new_element(_,[P],[P]).
add_new_element(_,[],[]).


%------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TOPOLOGIC CONSTRAINTS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 

:- chr_constraint mbr_bound_first/2, mbr_bound_second/2. 

:- chr_constraint inside/2, inside_set/3, inside_pairs/4, inside_tree/4, inside_pairs_one/4, 		
	inside_obj/2,
	inside_points_line_line/2,intersection_segment_line/3,intersection_segment_segment/3, 		
	in_segment/3, in_segment2/3.

:- chr_constraint overlap/2, overlap_set/3, overlap_pairs/4, overlap_tree/4, overlap_pairs_one/4, 		
	overlap_obj/2, overlap_point_obj/2, inside_line_region/2.

:-chr_constraint disjoint/2, disjoint_set/3, disjoint_pairs/4, disjoint_tree/4, disjoint_pairs_one/4, disjoint_obj/2.
 
:- chr_constraint outside/2, outside_set/3.

:- chr_constraint meet/2, meet_set/3, meet_pairs/4, meet_tree/4, meet_pairs_one/4, meet_obj/2.

:- chr_constraint neighbor/2, neighbor_set/3, neighbor_pairs/4, neighbor_tree/4, 		
	neighbor_pairs_one/4, neighbor_obj/2.

:- chr_constraint cut/2, cut_set/3, cut_pairs/4, cut_tree/4, cut_pairs_one/4, cut_obj/2.
 
:- chr_constraint mcovers/2, mcovers_set/3, mcovers_pairs/4, mcovers_tree/4, mcovers_pairs_one/4, mcovers_obj/2.

:- chr_constraint ncovers/2, ncovers_set/3, ncovers_pairs/4, ncovers_tree/4, ncovers_pairs_one/4, ncovers_obj/2.
   
:- chr_constraint mcoveredby/2, mcoveredby_set/3.

:- chr_constraint ncoveredby/2, ncoveredby_set/3.

:- chr_constraint range/2, range_set/3, range_obj/2, range_line_window/2, range_line_circle/2,
	range_pairs/4, range_tree/4, range_pairs_one/4, mbr_bound/2.

 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSIDE (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inside1 @ inside(A,B) <=> 	ground(A),ground(B),
				mbr(A,MBrA), 
				mbr(B,MBrB), 
				up_mbr(MBrA,UP), 
				down_mbr(MBrA,DOWN),
				cox_point(UP,COXUP),
				coy_point(UP,COYUP),
				cox_point(DOWN,COXDOWN),
				coy_point(DOWN,COYDOWN),
				inside_point_mbr((COXUP,COYUP),MBrB), 
				inside_point_mbr((COXDOWN,COYDOWN),MBrB) | 
				inside_obj(A,B). 

inside2 @ inside(A,B) <=> ground(A),ground(B) | fail. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSIDE SET (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


inside_set1 @ inside_set([],_,L2) <=> L2=[].

inside_set2 @ inside_set(_,[],L2) <=> L2=[].


inside_set3 @ inside_set([X1|L1],[X2|L2],PS) <=> ground([X1|L1]),ground([X2|L2]),
				rtree([X1|L1],5,R1),
				rtree([X2|L2],5,R2) |
				inside_set(R1,R2,PS).

inside_set4 @ inside_set(A,B,PS) <=> 
				root(A,RA), 
				root(B,RB)
			|     
				inside_tree(_,PS,RA,RB).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSIDE_PAIRS (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inside_pairs1 @ inside_pairs(_,[],_,U) <=> U=[].

inside_pairs2 @ inside_pairs(NPS,[X|L],L2,U) <=>
				inside_pairs_one(NPS,X,L2,L4),
				inside_pairs(NPS,L,L2,L5),
				append(L4,L5,L6),
				U=L6.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% inside_tree (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


inside_tree1 @ inside_tree(NPS,PS,(MBrR1,Child1),(MBrR2,Child2)) <=>	   	
			   	(overlap_mbr(MBrR1,MBrR2) -> 
                                (root(Child1,RChild1),
				root(Child2,RChild2),
				inside_tree(NPS,PS,RChild1,RChild2));
				PS=[]). 
                     
			   
inside_tree2 @ inside_tree(NPS,PS,objects(Z1,_),objects(Z2,_)) <=> 	
				inside_pairs(NPS,Z1,Z2,Z),
				PS=Z.

inside_tree3 @ inside_tree(NPS,PS,objects(Z1,N),(_,Child2)) <=> 
               			root(Child2,RChild2) 
				| 
				inside_tree(NPS,PS,objects(Z1,N),RChild2).

inside_tree4 @ inside_tree(NPS,PS,(_,Child1),objects(Z2,N)) <=> 
	               		root(Child1,RChild1) 
				| 
				inside_tree(NPS,PS,RChild1,objects(Z2,N)).


inside_tree5 @ inside_tree(NPS,PS,[(MBr1,Child1)|RNodes1],R2) <=> 			
				inside_tree(NPS,PS1,(MBr1,Child1),R2),
				inside_tree(NPS,PS2,RNodes1,R2),
				append(PS1,PS2,PS).

inside_tree6 @ inside_tree(_,PS,[],_) <=> 
				PS=[].


inside_tree7 @ inside_tree(NPS,PS,(MBr1,Child1),[(MBr,Child)|RNodes]) <=> 
			inside_tree(NPS,PS1,(MBr1,Child1),(MBr,Child)),				
			inside_tree(NPS,PS2,(MBr1,Child1),RNodes),
                        append(PS1,PS2,PS).         
                         
                         			
inside_tree8 @ inside_tree(_,PS,(_,_),[]) <=> 	PS=[].

inside_tree9 @ inside_tree(NPS,PS,objects(Z,N),[(MBr,Child)|RNodes]) <=> 
				inside_tree(NPS,PS1,objects(Z,N),(MBr,Child)),
				inside_tree(NPS,PS2,objects(Z,N),RNodes),
				append(PS1,PS2,PS).

inside_tree10 @ inside_tree(_,PS,objects(_,_),[]) <=> PS=[].
 






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSIDE PAIRS ONE (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inside_pairs_one1 @ inside_pairs_one(_,_,[],U) <=> U=[].

inside_pairs_one2 @ inside_pairs_one(NPS,X,[Z|L2],U) <=> 
				ground(X),ground(Z) |
				(inside_obj(X,Z) ->  
				mbr_obj(X,MBrX),			
				mbr_obj(Z,MBrZ),
	              		mbr_bound_first(NPS,MBrX),
				mbr_bound_second(NPS,MBrZ),
				(inside_pairs_one(NPS,X,L2,L3), 				
				U=[(X,Z)|L3]);							
				(inside_pairs_one(NPS,X,L2,L3),
				U=L3)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSIDE OBJECTS (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inside_obj1 @ inside_obj((X,Y),Z) <=>
				inside_point_obj((X,Y),Z).


inside_obj2 @ inside_obj(line(L),Z) <=>
				inside_line_obj(line(L),Z).

inside_obj3 @ inside_obj(polygon([P1|L]),Z) <=>
				append([P1|L],[P1],L2),
				inside_line_obj(line(L2),Z).

inside_obj4 @ inside_obj(region([P1|L]),Z) <=>
		                append([P1|L],[P1],L2),
				inside_line_obj(line(L2),Z).

inside_obj5 @ inside_obj(diff(O1,O2),Z) <=>
				inside_obj(O1,Z),
				\+overlap_obj(O2,Z).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OUTSIDE (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

outside @ outside(A,B) <=> inside(B,A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OUTSIDE SET (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

outside_set1 @ outside_set([],_,L2) <=> L2=[].
outside_set2 @ outside_set(_,[],L2) <=> L2=[].

outside_set3 @ outside_set([X1|L1],[X2|L2],PS) <=> ground([X1|L1]),ground([X2|L2]),
						rtree([X1|L1],5,R1),
						rtree([X2|L2],5,R2) |
						outside_set(R1,R2,PS).



outside_set4 @ outside_set(A,B,S) <=> inside_set(B,A,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% disjoint (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disjoint @ disjoint(A,B) <=> ground(A),ground(B) | (overlap(A,B)->fail;true). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% disjoint SET (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disjoint_set1 @ disjoint_set([],_,L2) <=> L2=[].
disjoint_set2 @ disjoint_set(_,[],L2) <=> L2=[].

disjoint_set3 @ disjoint_set([X1|L1],[X2|L2],PS) <=> ground([X1|L1]),ground([X2|L2]),
							rtree([X1|L1],5,R1),
							rtree([X2|L2],5,R2) |
							disjoint_set(R1,R2,PS).



disjoint_set4 @ disjoint_set(A,B,PS) <=> 
				root(A,RA), 
				root(B,RB)
				|     
				disjoint_tree(_,PS,RA,RB).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% disjoint_PAIRS (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disjoint_pairs1 @ disjoint_pairs(_,[],_,U) <=> U=[].

disjoint_pairs2 @ disjoint_pairs(NPS,[X|L],L2,U) <=>
				disjoint_pairs_one(NPS,X,L2,L4),
				disjoint_pairs(NPS,L,L2,L5),
				append(L4,L5,L6),
				U=L6.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% disjoint TREE (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


disjoint_tree1 @ disjoint_tree(NPS,PS,(MBrR1,Child1),(MBrR2,Child2)) <=>	   	
			   	(overlap_mbr(MBrR1,MBrR2) ->
                               	(root(Child1,RChild1),
				root(Child2,RChild2),
				disjoint_tree(NPS,PS,RChild1,RChild2));
				leaves(Child1,PS1),
				leaves(Child2,PS2),
				append(PS1,PS2,PS)). 
                     
			   


disjoint_tree2 @ disjoint_tree(NPS,PS,objects(Z1,_),objects(Z2,_)) <=> 	
				disjoint_pairs(NPS,Z1,Z2,Z), 	
				PS=Z.

disjoint_tree3 @ disjoint_tree(NPS,PS,objects(Z1,N),(_,Child2)) <=>        		
               		root(Child2,RChild2) 
			| 
			disjoint_tree(NPS,PS,objects(Z1,N),RChild2).

disjoint_tree4 @ disjoint_tree(NPS,PS,(_,Child1),objects(Z2,N)) <=>    		
               		root(Child1,RChild1) 
			| 
			disjoint_tree(NPS,PS,RChild1,objects(Z2,N)).


disjoint_tree5 @ disjoint_tree(NPS,PS,[(MBr1,Child1)|RNodes1],R2) <=> 
				disjoint_tree(NPS,PS1,(MBr1,Child1),R2),
				disjoint_tree(NPS,PS2,RNodes1,R2),
				append(PS1,PS2,PS).

disjoint_tree6 @ disjoint_tree(_,PS,[],_) <=> 
				PS=[].


disjoint_tree7 @ disjoint_tree(NPS,PS,(MBr1,Child1),[(MBr,Child)|RNodes]) <=> 
		disjoint_tree(NPS,PS1,(MBr1,Child1),(MBr,Child)),				
		disjoint_tree(NPS,PS2,(MBr1,Child1),RNodes),
                append(PS1,PS2,PS).         
                         
                    
                        			
				

disjoint_tree8 @ disjoint_tree(_,PS,(_,_),[]) <=> 
				PS=[].

disjoint_tree9 @ disjoint_tree(NPS,PS,objects(Z,N),[(MBr,Child)|RNodes]) <=> 
				disjoint_tree(NPS,PS1,objects(Z,N),(MBr,Child)),
				disjoint_tree(NPS,PS2,objects(Z,N),RNodes),
				append(PS1,PS2,PS).

disjoint_tree10 @ disjoint_tree(_,PS,objects(_,_),[]) <=> 
				PS=[].
 






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% disjoint PAIRS ONE (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disjoint_pairs_one1 @ disjoint_pairs_one(_,_,[],U) <=> U=[].

disjoint_pairs_one2 @ disjoint_pairs_one(NPS,X,[Z|L2],U) <=> 
				ground(X),ground(Z) |
				(disjoint_obj(X,Z) ->  
				mbr_obj(X,MBrX),			
				mbr_obj(Z,MBrZ),
	              		mbr_bound_first(NPS,MBrX),
				mbr_bound_second(NPS,MBrZ),
				(disjoint_pairs_one(NPS,X,L2,L3),
				U=[(X,Z)|L3]);	
				(disjoint_pairs_one(NPS,X,L2,L3),
				U=L3)).

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DISJOINT OBJECTS (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disjoint_obj @ disjoint_obj(O,Z) <=> ground(O),ground(Z) | (overlap_obj(O,Z)->fail;true).


 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OVERLAP (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

overlap @ overlap(A,B) <=>      ground(A),ground(B),
				mbr(A,MBrA), 
				mbr(B,MBrB),
                     		overlap_mbr(MBrA,MBrB) |
				overlap_obj(A,B). 

overlap @ overlap(A,B) <=> ground(A),ground(B) | fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OVERLAP OBJECTS (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

overlap_obj1 @ overlap_obj((X,Y),Z) <=>
					overlap_point_obj((X,Y),Z).


overlap_obj2 @  overlap_obj(line(L),(X,Y)) <=> 
					overlap_point_obj((X,Y),line(L)).


overlap_obj3 @  overlap_obj(line(L1),line(L2))<=>  
					inside_points_line_line(L1,line(L2)).

overlap_obj4 @  overlap_obj(line(L1),polygon([P|L2]))<=>
					append([P|L2],[P],L3),  
					inside_points_line_line(L1,line(L3)).

overlap_obj5 @ overlap_obj(polygon(L),(X,Y)) <=> overlap_obj((X,Y),polygon(L)).

overlap_obj6 @  overlap_obj(polygon(L1),line(L2))<=>
					overlap_obj(line(L2),polygon(L1)).

overlap_obj7 @  overlap_obj(polygon([P1|L1]),polygon([P2|L2]))<=>
					append([P1|L1],[P1],L3),
					append([P2|L2],[P2],L4),  
					inside_points_line_line(L3,line(L4)).
 
overlap_obj8 @ overlap_obj(region(L),(X,Y)) <=> overlap_obj((X,Y),region(L)).

overlap_obj9 @ overlap_obj(line(L1),region(L2)) <=> overlap_obj(line(L1),polygon(L2)) | true.

overlap_obj10 @ overlap_obj(line(L1),region(L2)) <=> inside_line_region(L1,region(L2)).

overlap_obj11 @ overlap_obj(region(L2),line(L1)) <=> overlap_obj(line(L1),region(L2)). 

overlap_obj12 @ overlap_obj(polygon(L1),region(L2)) <=> 
					overlap_obj(polygon(L1),polygon(L2)) | true.

overlap_obj13 @ overlap_obj(polygon([P|L1]),region(L2)) <=> 
					append([P|L1],[P],L3),
					inside_line_region(L3,region(L2)).

overlap_obj14 @ overlap_obj(region(L2),polygon(L1)) <=> 
					overlap_obj(polygon(L1),region(L2)). 

overlap_obj15 @ overlap_obj(region(L1),region(L2)) <=> 
					overlap_obj(polygon(L1),polygon(L2)).

overlap_obj16 @ overlap_obj(region([P|L1]),region(L2)) <=> 
					append([P|L1],[P],L3),
					inside_line_region(L3,region(L2)).

overlap_obj17 @ overlap_obj((X,Y),diff(O1,O2)) <=> overlap_obj(diff(O1,O2),(X,Y)).

overlap_obj18 @ overlap_obj(line(L),diff(O1,O2)) <=> overlap_obj(diff(O1,O2),line(L)).

overlap_obj19 @ overlap_obj(polygon(L),diff(O1,O2)) <=> overlap_obj(diff(O1,O2),polygon(L)).

overlap_obj20 @ overlap_obj(region(L),diff(O1,O2)) <=> overlap_obj(diff(O1,O2),region(L)).

overlap_obj22 @ overlap_obj(diff(O1,O2),Z) <=> overlap_obj(O1,Z),\+inside_obj(Z,O2).

overlap_obj23 @ overlap_obj(_,_) <=> fail.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSIDE POINTS LINE LINE (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inside_points_line_line1 @ inside_points_line_line([],line(_)) <=> 
				fail.

inside_points_line_line2 @ inside_points_line_line([_],line(_)) <=> 
				fail.

inside_points_line_line3 @  inside_points_line_line([P1,P2|RP],line(L)) <=> 
				intersection_segment_line([P1,P2],L,_);
				inside_points_line_line([P2|RP],line(L)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERSECTION SEGMENT LINE (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


intersection_segment_line1 @ intersection_segment_line(_,[],POINTS) <=> POINTS=[].
intersection_segment_line1 @ intersection_segment_line(_,[_],POINTS) <=> POINTS=[].
intersection_segment_line2 @ intersection_segment_line([P1,P2],[P3,P4|RP],POINTS) <=> 
				intersection_segment_line([P1,P2],[P4|RP],POINTS2), 
				intersection_segment_segment([P1,P2],[P3,P4],POINTS3),
				append_clean(POINTS3,POINTS2,POINTS).


%%%%%%%%%%%%%%%%%%%%%%%%%
% IN SEGMENT (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%

in_segment((X,Y),(A,B),(C,D)) <=>  in_segment2((X,Y),(A,B),(C,D)) | true.

in_segment((X,Y),(A,B),(C,D)) <=>  in_segment2((X,Y),(C,D),(A,B)) | true.

in_segment((X,Y),(A,B),(C,D)) <=>  in_segment2((X,Y),(A,D),(C,B)) | true.

in_segment((X,Y),(A,B),(C,D)) <=>  in_segment2((X,Y),(C,B),(A,D)) | true.

in_segment((_,_),(_,_),(_,_)) <=>  fail.

%%%%%%%%%%%%%%%%%%%%%%%%%
% IN SEGMENT (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%

in_segment2((X,Y),(A,B),(C,D)) <=> leq(A,X),leq(X,C),leq(B,Y),leq(Y,D) | true.

in_segment2((_,_),(_,_),(_,_)) <=> fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERSECTION SEGMENT SEGMENT (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

intersection_segment_segment1 @ 
			intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
			inside_point_segment((A,B),(E,F),(G,H)),
			inside_point_segment((C,D),(E,F),(G,H))
			|  
			(eq2((A,B),(C,D))->POINT=[(A,B)];POINT=[line([(A,B),(C,D)])]).

%----- (E,F)---(A,B)--(C,D)---(G,H)----

intersection_segment_segment2 @ 
			intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
			inside_point_segment((E,F),(A,B),(C,D)),
			inside_point_segment((G,H),(A,B),(C,D))
			|
			(eq2((E,F),(G,H))->POINT=[(E,F)];POINT=[line([(E,F),(G,H)])]). 

%----- (A,B)---(E,F)--(G,H)---(C,D)----

intersection_segment_segment3 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		inside_point_segment((A,B),(E,F),(G,H)),
		inside_point_segment((E,F),(C,D),(A,B)) 
		|
		(eq2((E,F),(A,B))->POINT=[(E,F)];POINT=[line([(E,F),(A,B)])]).


%-----(C,D)--- (E,F)---(A,B)----(G,H)----

intersection_segment_segment4 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		inside_point_segment((C,D),(E,F),(G,H)),
		inside_point_segment((E,F),(A,B),(C,D))
		|
		(eq2((E,F),(C,D))->POINT=[(E,F)];POINT=[line([(E,F),(C,D)])]).

%-----(A,B)---(E,F)---(C,D)---(G,H)----

intersection_segment_segment5 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		inside_point_segment((A,B),(E,F),(G,H)),
		inside_point_segment((G,H),(A,B),(C,D)) 
		|
		(eq2((A,B),(G,H))->POINT=[(A,B)];POINT=[line([(A,B),(G,H)])]).


%-----(E,F)--- (A,B)---(G,H)----(C,D)----


intersection_segment_segment6 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		inside_point_segment((C,D),(E,F),(G,H)),
		inside_point_segment((G,H),(C,D),(A,B)) 
		|
		(eq2((C,D),(G,H))->POINT=[(C,D)];POINT=[line([(C,D),(G,H)])]).


%-----(E,F)--- (C,D)---(G,H)----(A,B)----

 				 
intersection_segment_segment7 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		neqn(D,B),neqn(H,F),neqn(A,C),neqn(E,G),
		SLOPE_1 is (C-A)/(D-B), SLOPE_2 is (G-E)/(H-F), 	 		
		neqn(SLOPE_1,SLOPE_2),
      		Y is ((-SLOPE_1*D)+(SLOPE_2*H)-G+C)/(SLOPE_2-SLOPE_1),
		X is (SLOPE_1*(Y-D))+C,    
		in_segment((X,Y),(E,F),(G,H)),in_segment((X,Y),(A,B),(C,D))  
		|
		POINT=[(X,Y)].

%--A=/C, B=/D, E=/G, F=/H

intersection_segment_segment8 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		eqn(D,B),neqn(H,F),neqn(G,E),neqn(C,A), 
		SLOPE_2 is (G-E)/(H-F), 
		X is ((D-F)*SLOPE_2)+E,  
		Y is D,
		in_segment((X,Y),(E,F),(G,H)),in_segment((X,Y),(A,B),(C,D))  
		|
		POINT=[(X,Y)].
				
%--D=B,A=/C,E=/G,F=/H

intersection_segment9 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		eqn(D,B),neqn(H,F),eqn(G,E),neqn(A,C), 
		X is G,
		Y is D,
		in_segment((X,Y),(E,F),(G,H)),in_segment((X,Y),(A,B),(C,D))  
		|
		POINT=[(X,Y)].
				
%--D=B,G=E,H=/F,A=/C

intersection_segment_segment10 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		neqn(D,B),neqn(H,F),eqn(G,E),neqn(A,C), 
		X is G,
		SLOPE_1 is (C-A)/(D-B), 
		Y is ((X-C)/SLOPE_1)+D,
		in_segment((X,Y),(E,F),(G,H)),in_segment((X,Y),(A,B),(C,D))   
		|
		POINT=[(X,Y)].
				
%--G=E,D/=B,H=/F,A=/C

intersection_segment_segment11 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		eqn(H,F),neqn(B,D),neqn(C,A),neqn(E,G),  
		SLOPE_1 is (C-A)/(D-B), 
		X is ((H-B)*SLOPE_1)+A, 
		Y is H,
		in_segment((X,Y),(E,F),(G,H)),in_segment((X,Y),(A,B),(C,D))  
		|
		POINT=[(X,Y)].

%--H=F,B=/D,C=/A,E=/G

intersection_segment_segment12 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		eqn(H,F),neqn(B,D),eqn(C,A),neqn(E,G), 
		X is C,
		Y is H,
		in_segment((X,Y),(E,F),(G,H)),in_segment((X,Y),(A,B),(C,D))  
		|
		POINT=[(X,Y)].
			
%--H=F,C=A,D/=B,E=/G

intersection_segment_segment13 @ 
		intersection_segment_segment([(A,B),(C,D)],[(E,F),(G,H)],POINT) <=>
		neqn(H,F),neqn(B,D),eqn(C,A),neqn(E,G), 
		X is C,
		SLOPE_1 is (H-F)/(G-E), 
		Y is ((X-G)/SLOPE_1)+H,
		in_segment((X,Y),(E,F),(G,H)),in_segment((X,Y),(A,B),(C,D))   			 			
		|
		POINT=[(X,Y)].
			
%--C=A,H/=F,D/=B,E=/G


intersection_segment_segment14 @ 
		intersection_segment_segment([(_,_),(_,_)],[(_,_),(_,_)],POINT) <=>
		POINT=[].	
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSIDE LINE REGION (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inside_line_region1 @ inside_line_region([],region(_)) <=> fail.

inside_line_region2 @ inside_line_region([P1,P2|_],region(L)) <=> 
			inside_point_obj(P1,region(L)),
			inside_point_obj(P2,region(L)) 
			| 
			true.

inside_line_region3 @ inside_line_region([_|RP],region(L)) <=> 
			inside_line_region(RP,region(L)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OVERLAP SET (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

overlap_set1 @ overlap_set([],_,L2) <=> L2=[].

overlap_set2 @ overlap_set(_,[],L2) <=> L2=[].

overlap_set3 @ overlap_set([X1|L1],[X2|L2],PS) <=> ground([X1|L1]),ground([X2|L2]),
			rtree([X1|L1],5,R1),
			rtree([X2|L2],5,R2) 
			|
			overlap_set(R1,R2,PS).



overlap_set4 @ overlap_set(A,B,PS) <=> 
			root(A,RA), 
			root(B,RB)
			|     
			overlap_tree(_,PS,RA,RB).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OVERLAP_PAIRS (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

overlap_pairs1 @ overlap_pairs(_,[],_,U) <=> U=[].

overlap_pairs2 @ overlap_pairs(NPS,[X|L],L2,U) <=>
				overlap_pairs_one(NPS,X,L2,L4),
				overlap_pairs(NPS,L,L2,L5),
				append(L4,L5,L6),
				U=L6.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% overlap_tree (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


overlap_tree1 @ overlap_tree(NPS,PS,(MBrR1,Child1),(MBrR2,Child2)) <=>	   	 
			   	(overlap_mbr(MBrR1,MBrR2) ->                
                     		(root(Child1,RChild1),
				root(Child2,RChild2),
				overlap_tree(NPS,PS,RChild1,RChild2));
				PS=[]). 
                     
			   
overlap_tree2 @ overlap_tree(NPS,PS,objects(Z1,_),objects(Z2,_)) <=> 	
				overlap_pairs(NPS,Z1,Z2,Z), 
				PS=Z.

overlap_tree3 @ overlap_tree(NPS,PS,objects(Z1,N),(_,Child2)) <=> 
	               		root(Child2,RChild2) 
				| 
				overlap_tree(NPS,PS,objects(Z1,N),RChild2).

overlap_tree4 @ overlap_tree(NPS,PS,(_,Child1),objects(Z2,N)) <=> 	
	               		root(Child1,RChild1) 
				| 
				overlap_tree(NPS,PS,RChild1,objects(Z2,N)).


overlap_tree5 @ overlap_tree(NPS,PS,[(MBr1,Child1)|RNodes1],R2) <=> 
				overlap_tree(NPS,PS1,(MBr1,Child1),R2),
				overlap_tree(NPS,PS2,RNodes1,R2),
				append(PS1,PS2,PS).

overlap_tree6 @ overlap_tree(_,PS,[],_) <=> 
				PS=[].


overlap_tree7 @ overlap_tree(NPS,PS,(MBr1,Child1),[(MBr,Child)|RNodes]) <=> 
		overlap_tree(NPS,PS1,(MBr1,Child1),(MBr,Child)),				
		overlap_tree(NPS,PS2,(MBr1,Child1),RNodes),
                append(PS1,PS2,PS).         
                         
overlap_tree8 @ overlap_tree(_,PS,(_,_),[]) <=> 
				PS=[].

overlap_tree9 @ overlap_tree(NPS,PS,objects(Z,N),[(MBr,Child)|RNodes]) <=> 
				overlap_tree(NPS,PS1,objects(Z,N),(MBr,Child)),
				overlap_tree(NPS,PS2,objects(Z,N),RNodes),
				append(PS1,PS2,PS).

overlap_tree10 @ overlap_tree(_,PS,objects(_,_),[]) <=> 
				PS=[].


 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OVERLAP PAIRS ONE (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

overlap_pairs_one1 @  overlap_pairs_one(_,_,[],U) <=> U=[].

overlap_pairs_one2 @  overlap_pairs_one(NPS,X,[Z|L2],U) <=> 
				ground(X),ground(Z) |
				(overlap_obj(X,Z) ->  
				mbr_obj(X,MBrX),			
				mbr_obj(Z,MBrZ),
	              		mbr_bound_first(NPS,MBrX),
				mbr_bound_second(NPS,MBrZ),
				(overlap_pairs_one(NPS,X,L2,L3),
				U=[(X,Z)|L3]);	
				(overlap_pairs_one(NPS,X,L2,L3),
				U=L3)).

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OVERLAP POINT OBJ (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

overlap_point_obj1 @ overlap_point_obj((X,Y),(Z,T)) <=> 
					eqn(X,Z), eqn(Y,T).

overlap_point_obj2 @ overlap_point_obj((X,Y),line(L)) <=> 
					inside_point_obj((X,Y),line(L)).

overlap_point_obj3 @ overlap_point_obj((X,Y),polygon([P|L])) <=> 
					append([P|L],[P],L2),
					inside_point_obj((X,Y),line(L2)).

overlap_point_obj4 @ overlap_point_obj((X,Y),region(L)) <=> 
					inside_point_obj((X,Y),region(L)).

 overlap_point_obj6 @ overlap_point_obj((X,Y),diff(O1,O2)) <=>
					overlap_point_obj((X,Y),O1),
					\+inside_point_obj((X,Y),O2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MEET (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

meet @ meet(A,B) <=> 		ground(A),ground(B),
				mbr(A,MBrA), 
				mbr(B,MBrB),
				overlap_mbr(MBrA,MBrB) |
				meet_obj(A,B). 

meet @ meet(A,B) <=> ground(A),ground(B) | fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MEET OBJECTS (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

meet_obj1 @  meet_obj(diff(O1,O2),O3) <=> inter_obj(O1,O3,I),in_set((X,Y),I),\+inside_obj((X,Y),O2).

meet_obj2 @  meet_obj(O1,diff(O2,O3)) <=> inter_obj(O1,O2,I),in_set((X,Y),I),\+inside_obj((X,Y),O3).

meet_obj3 @  meet_obj(O1,O2) <=> inter_obj(O1,O2,I), in_set((_,_),I).

 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MEET SET (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

meet_set1 @ meet_set([],_,L2) <=> L2=[].

meet_set2 @ meet_set(_,[],L2) <=> L2=[].

meet_set3 @ meet_set([X1|L1],[X2|L2],PS) <=> 
			ground([X1|L1]),ground([X2|L2]),
			rtree([X1|L1],5,R1),
			rtree([X2|L2],5,R2)
			|
			meet_set(R1,R2,PS).

meet_set4 @ meet_set(A,B,PS) <=> 
			root(A,RA), 
			root(B,RB)
			|     
			meet_tree(_,PS,RA,RB).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MEET PAIRS (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

meet_pairs1 @ meet_pairs(_,[],_,U) <=> U=[].

meet_pairs2 @ meet_pairs(NPS,[X|L],L2,U) <=>
				meet_pairs_one(NPS,X,L2,L4),
				meet_pairs(NPS,L,L2,L5),
				append(L4,L5,L6),
				U=L6.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MEET TREE (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


meet_tree1 @ meet_tree(NPS,PS,(MBrR1,Child1),(MBrR2,Child2)) <=>	   	
			   	(overlap_mbr(MBrR1,MBrR2) ->
				(root(Child1,RChild1),
				root(Child2,RChild2),
				meet_tree(NPS,PS,RChild1,RChild2));
				PS=[]). 
                     
			   
meet_tree2 @ meet_tree(NPS,PS,objects(Z1,_),objects(Z2,_)) <=> 	
				meet_pairs(NPS,Z1,Z2,Z), 
				PS=Z.

meet_tree3 @ meet_tree(NPS,PS,objects(Z1,N),(_,Child2)) <=> 
	               		root(Child2,RChild2) 
				| 
				meet_tree(NPS,PS,objects(Z1,N),RChild2).

meet_tree4 @ meet_tree(NPS,PS,(_,Child1),objects(Z2,N)) <=> 
           			root(Child1,RChild1) 
				| 
				meet_tree(NPS,PS,RChild1,objects(Z2,N)).


meet_tree5 @ meet_tree(NPS,PS,[(MBr1,Child1)|RNodes1],R2) <=> 
				meet_tree(NPS,PS1,(MBr1,Child1),R2),
				meet_tree(NPS,PS2,RNodes1,R2),
				append(PS1,PS2,PS).

meet_tree6 @ meet_tree(_,PS,[],_) <=> 
				PS=[].


meet_tree7 @ meet_tree(NPS,PS,(MBr1,Child1),[(MBr,Child)|RNodes]) <=> 
		meet_tree(NPS,PS1,(MBr1,Child1),(MBr,Child)),				
		meet_tree(NPS,PS2,(MBr1,Child1),RNodes),
                append(PS1,PS2,PS).         
                         		

meet_tree8 @ meet_tree(_,PS,(_,_),[]) <=> 
				PS=[].

meet_tree9 @ meet_tree(NPS,PS,objects(Z,N),[(MBr,Child)|RNodes]) <=> 
				meet_tree(NPS,PS1,objects(Z,N),(MBr,Child)),
				meet_tree(NPS,PS2,objects(Z,N),RNodes),
				append(PS1,PS2,PS).

meet_tree10 @ meet_tree(_,PS,objects(_,_),[]) <=> 
				PS=[].
 



 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% meet PAIRS ONE (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

meet_pairs_one1 @  meet_pairs_one(_,_,[],U) <=> U=[].

meet_pairs_one2 @  meet_pairs_one(NPS,X,[Z|L2],U) <=> 
				ground(X),ground(Z) |
				(meet_obj(X,Z) ->  
				mbr_obj(X,MBrX),			
				mbr_obj(Z,MBrZ),
	              		mbr_bound_first(NPS,MBrX),
				mbr_bound_second(NPS,MBrZ),
				(meet_pairs_one(NPS,X,L2,L3),
				U=[(X,Z)|L3]);			
				(meet_pairs_one(NPS,X,L2,L3),
				U=L3)).

 


 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CUT (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cut @ cut(A,B) <=>		ground(A),ground(B), 		
				mbr(A,MBrA), 
				mbr(B,MBrB), 
				overlap_mbr(MBrA,MBrB) |
				cut_obj(A,B). 

cut @ cut(A,B) <=> ground(A),ground(B) | fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CUT OBJECTS (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cut_obj1 @ cut_obj(diff(O1,_),O3) <=>
			cut_obj(O1,O3).

cut_obj2 @ cut_obj(O1,diff(O2,_)) <=>
			cut_obj(O1,O2).

cut_obj3 @  cut_obj(O2,line(L)) <=> 
			diff_obj(line(L),O2,line([P|RP]),H),
			in_set((X,Y),H),
			neq(P,(X,Y)),
			last([P|RP],Q),
			neq(Q,(X,Y)).

cut_obj4 @  cut_obj(O2,polygon(L)) <=> 
			diff_obj(polygon(L),O2,_,H),
			in_set(O3,H),
			in_set(O4,H),
			neq(O3,O4).

cut_obj5 @  cut_obj(O2,region(L)) <=> 
			cut_obj(O2,polygon(L)).



cut_obj6 @  cut_obj(_,_) <=> fail.


 

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CUT SET (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cut_set1 @ cut_set([],_,L2) <=> L2=[].

cut_set2 @ cut_set(_,[],L2) <=> L2=[].

cut_set3 @ cut_set([X1|L1],[X2|L2],PS) <=> ground([X1|L1]),ground([X2|L2]),
					rtree([X1|L1],5,R1),
					rtree([X2|L2],5,R2) |
					cut_set(R1,R2,PS).

cut_set4 @ cut_set(A,B,PS) <=> 
			root(A,RA), 
			root(B,RB)
			|     
			cut_tree(_,PS,RA,RB).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CUT PAIRS (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cut_pairs1 @ cut_pairs(_,[],_,U) <=> U=[].

cut_pairs2 @ cut_pairs(NPS,[X|L],L2,U) <=>
				cut_pairs_one(NPS,X,L2,L4),
				cut_pairs(NPS,L,L2,L5),
				append(L4,L5,L6),
				U=L6.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CUT TREE (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cut_tree1 @ cut_tree(NPS,PS,(MBrR1,Child1),(MBrR2,Child2)) <=>	   	 		   	(overlap_mbr(MBrR1,MBrR2) ->
                  	(root(Child1,RChild1),
			root(Child2,RChild2),
			cut_tree(NPS,PS,RChild1,RChild2));
			PS=[]). 
                     
			   
cut_tree2 @ cut_tree(NPS,PS,objects(Z1,_),objects(Z2,_)) <=> 	
			cut_pairs(NPS,Z1,Z2,Z), 
			PS=Z.

cut_tree3 @ cut_tree(NPS,PS,objects(Z1,N),(_,Child2)) <=> 
               		root(Child2,RChild2) 
			| 
			cut_tree(NPS,PS,objects(Z1,N),RChild2).

cut_tree4 @ cut_tree(NPS,PS,(_,Child1),objects(Z2,N)) <=> 
               		root(Child1,RChild1) 
			| 
			cut_tree(NPS,PS,RChild1,objects(Z2,N)).


cut_tree5 @ cut_tree(NPS,PS,[(MBr1,Child1)|RNodes1],R2) <=> 
				cut_tree(NPS,PS1,(MBr1,Child1),R2),
				cut_tree(NPS,PS2,RNodes1,R2),
				append(PS1,PS2,PS).

cut_tree6 @ cut_tree(_,PS,[],_) <=> 
				PS=[].


cut_tree7 @ cut_tree(NPS,PS,(MBr1,Child1),[(MBr,Child)|RNodes]) <=> 
			cut_tree(NPS,PS1,(MBr1,Child1),(MBr,Child)),				
			cut_tree(NPS,PS2,(MBr1,Child1),RNodes),
                        append(PS1,PS2,PS).        
                         
cut_tree8 @ cut_tree(_,PS,(_,_),[]) <=> 
				PS=[].

cut_tree9 @ cut_tree(NPS,PS,objects(Z,N),[(MBr,Child)|RNodes]) <=> 
				cut_tree(NPS,PS1,objects(Z,N),(MBr,Child)),
				cut_tree(NPS,PS2,objects(Z,N),RNodes),
				append(PS1,PS2,PS).

cut_tree10 @ cut_tree(_,PS,objects(_,_),[]) <=> 
				PS=[].
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cut PAIRS ONE (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cut_pairs_one1 @  cut_pairs_one(_,_,[],U) <=> U=[].

cut_pairs_one2 @  cut_pairs_one(NPS,X,[Z|L2],U) <=> 
				ground(X),ground(Z) |
				(cut_obj(X,Z) ->  
				mbr_obj(X,MBrX),			
				mbr_obj(Z,MBrZ),
	              		mbr_bound_first(NPS,MBrX),
				mbr_bound_second(NPS,MBrZ),
				(cut_pairs_one(NPS,X,L2,L3),
				U=[(X,Z)|L3]);			
				(cut_pairs_one(NPS,X,L2,L3),
				U=L3)).



 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NEIGHBOR (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

neighbor @ neighbor(A,B) <=> 	ground(A),ground(B),
				mbr(A,MBrA), 
				mbr(B,MBrB),
				overlap_mbr(MBrA,MBrB) |
				neighbor_obj(A,B). 

neighbor @ neighbor(A,B) <=> ground(A),ground(B) | fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NEIGHBOR OBJECTS (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

neighbor_obj1 @  neighbor_obj(diff(O1,O2),O3) <=> inter_obj(O1,O3,I),in_set(line(L),I),\+inside_obj(line(L),O2).  

neighbor_obj2 @  neighbor_obj(O1,diff(O2,O3)) <=> inter_obj(O1,O2,I),in_set(line(L),I),\+inside_obj(line(L),O3). 

neighbor_obj3 @  neighbor_obj(O1,O2) <=> inter_obj(O1,O2,I),in_set(line(_),I).

 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NEIGHBOR SET (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

neighbor_set1 @ neighbor_set([],_,L2) <=> L2=[].

neighbor_set2 @ neighbor_set(_,[],L2) <=> L2=[].

neighbor_set3 @ neighbor_set([X1|L1],[X2|L2],PS) <=> 
		ground([X1|L1]),ground([X2|L2]),
		rtree([X1|L1],5,R1),
		rtree([X2|L2],5,R2) |
		neighbor_set(R1,R2,PS).

neighbor_set4 @ neighbor_set(A,B,PS) <=> 
		root(A,RA), 
		root(B,RB)
		|     
		neighbor_tree(_,PS,RA,RB).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NEIGHBOR PAIRS (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

neighbor_pairs1 @ neighbor_pairs(_,[],_,U) <=> U=[].

neighbor_pairs2 @ neighbor_pairs(NPS,[X|L],L2,U) <=>
				neighbor_pairs_one(NPS,X,L2,L4),
				neighbor_pairs(NPS,L,L2,L5),
				append(L4,L5,L6),
				U=L6.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NEIGHBOR TREE (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


neighbor_tree1 @ neighbor_tree(NPS,PS,(MBrR1,Child1),(MBrR2,Child2)) <=>	   	
			(overlap_mbr(MBrR1,MBrR2) ->
                	(root(Child1,RChild1),
			root(Child2,RChild2),
			neighbor_tree(NPS,PS,RChild1,RChild2));
			PS=[]). 
                     
			   
neighbor_tree2 @ neighbor_tree(NPS,PS,objects(Z1,_),objects(Z2,_)) <=> 	
				neighbor_pairs(NPS,Z1,Z2,Z), 
				PS=Z.

neighbor_tree3 @ neighbor_tree(NPS,PS,objects(Z1,N),(_,Child2)) <=> 
               		root(Child2,RChild2) 
			| 
			neighbor_tree(NPS,PS,objects(Z1,N),RChild2).

neighbor_tree4 @ neighbor_tree(NPS,PS,(_,Child1),objects(Z2,N)) <=> 
               		root(Child1,RChild1) 
			| 
			neighbor_tree(NPS,PS,RChild1,objects(Z2,N)).


neighbor_tree5 @ neighbor_tree(NPS,PS,[(MBr1,Child1)|RNodes1],R2) <=> 
				neighbor_tree(NPS,PS1,(MBr1,Child1),R2),
				neighbor_tree(NPS,PS2,RNodes1,R2),
				append(PS1,PS2,PS).

neighbor_tree6 @ neighbor_tree(_,PS,[],_) <=> 
				PS=[].


neighbor_tree7 @ neighbor_tree(NPS,PS,(MBr1,Child1),[(MBr,Child)|RNodes]) <=> 
		neighbor_tree(NPS,PS1,(MBr1,Child1),(MBr,Child)),				
		neighbor_tree(NPS,PS2,(MBr1,Child1),RNodes),
                append(PS1,PS2,PS).         
                         
                         			
neighbor_tree8 @ neighbor_tree(_,PS,(_,_),[]) <=> 
				PS=[].

neighbor_tree9 @ neighbor_tree(NPS,PS,objects(Z,N),[(MBr,Child)|RNodes]) <=> 
				neighbor_tree(NPS,PS1,objects(Z,N),(MBr,Child)),
				neighbor_tree(NPS,PS2,objects(Z,N),RNodes),
				append(PS1,PS2,PS).

neighbor_tree10 @ neighbor_tree(_,PS,objects(_,_),[]) <=> 
				PS=[].
 



 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NEIGHBOR PAIRS ONE (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

neighbor_pairs_one1 @  neighbor_pairs_one(_,_,[],U) <=> U=[].

neighbor_pairs_one2 @  neighbor_pairs_one(NPS,X,[Z|L2],U) <=> 
				ground(X),ground(Z) |
				(neighbor_obj(X,Z) ->  
				mbr_obj(X,MBrX),			
				mbr_obj(Z,MBrZ),
	              		mbr_bound_first(NPS,MBrX),
				mbr_bound_second(NPS,MBrZ),
				(neighbor_pairs_one(NPS,X,L2,L3),
				U=[(X,Z)|L3]);			
				(neighbor_pairs_one(NPS,X,L2,L3),
				U=L3)).

 




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MCOVERS (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mcovers @ mcovers(A,B) <=> 
				mbr(A,MBrA), 
				mbr(B,MBrB),
				overlap_mbr(MBrA,MBrB) |
				mcovers_obj(A,B). 

mcovers @ mcovers(A,B) <=> ground(A),ground(B) | fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MCOVERS OBJECTS (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mcovers_obj @  mcovers_obj(O1,O2) <=> meet_obj(O1,O2),inside_obj(O1,O2).

 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MCOVERS SET (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mcovers_set1 @ mcovers_set([],_,L2) <=> L2=[].

mcovers_set2 @ mcovers_set(_,[],L2) <=> L2=[].

mcovers_set3 @ mcovers_set([X1|L1],[X2|L2],PS) <=> 
			ground([X1|L1]),ground([X2|L2]),
			rtree([X1|L1],5,R1),
			rtree([X2|L2],5,R2) 
			|
			mcovers_set(R1,R2,PS).



mcovers_set4 @ mcovers_set(A,B,PS) <=> 
				root(A,RA), 
				root(B,RB)
				|     
				mcovers_tree(_,PS,RA,RB).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mcovers_PAIRS (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mcovers_pairs1 @ mcovers_pairs(_,[],_,U) <=> U=[].

mcovers_pairs2 @ mcovers_pairs(NPS,[X|L],L2,U) <=>
				mcovers_pairs_one(NPS,X,L2,L4),
				mcovers_pairs(NPS,L,L2,L5),
				append(L4,L5,L6),
				U=L6.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mcovers_tree (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


mcovers_tree1 @ mcovers_tree(NPS,PS,(MBrR1,Child1),(MBrR2,Child2)) <=>	   	
			(overlap_mbr(MBrR1,MBrR2) ->
                	(root(Child1,RChild1),
			root(Child2,RChild2),
			mcovers_tree(NPS,PS,RChild1,RChild2));
			PS=[]). 
                     
			   
mcovers_tree2 @ mcovers_tree(NPS,PS,objects(Z1,_),objects(Z2,_)) <=> 	
				mcovers_pairs(NPS,Z1,Z2,Z), 
				PS=Z.

mcovers_tree3 @ mcovers_tree(NPS,PS,objects(Z1,N),(_,Child2)) <=> 
	               		root(Child2,RChild2) 
				| 
				mcovers_tree(NPS,PS,objects(Z1,N),RChild2).

mcovers_tree4 @ mcovers_tree(NPS,PS,(_,Child1),objects(Z2,N)) <=> 
               			root(Child1,RChild1) 
				| 
				mcovers_tree(NPS,PS,RChild1,objects(Z2,N)).


mcovers_tree5 @ mcovers_tree(NPS,PS,[(MBr1,Child1)|RNodes1],R2) <=> 
				mcovers_tree(NPS,PS1,(MBr1,Child1),R2),
				mcovers_tree(NPS,PS2,RNodes1,R2),
				append(PS1,PS2,PS).

mcovers_tree6 @ mcovers_tree(_,PS,[],_) <=> 
				PS=[].


mcovers_tree7 @ mcovers_tree(NPS,PS,(MBr1,Child1),[(MBr,Child)|RNodes]) <=> 
		mcovers_tree(NPS,PS1,(MBr1,Child1),(MBr,Child)),				
		mcovers_tree(NPS,PS2,(MBr1,Child1),RNodes),
                append(PS1,PS2,PS).         
                         			
				

mcovers_tree8 @ mcovers_tree(_,PS,(_,_),[]) <=> 
				PS=[].

mcovers_tree9 @ mcovers_tree(NPS,PS,objects(Z,N),[(MBr,Child)|RNodes]) <=> 
				mcovers_tree(NPS,PS1,objects(Z,N),(MBr,Child)),
				mcovers_tree(NPS,PS2,objects(Z,N),RNodes),
				append(PS1,PS2,PS).

mcovers_tree10 @ mcovers_tree(_,PS,objects(_,_),[]) <=> 
				PS=[].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mcovers PAIRS ONE (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mcovers_pairs_one1 @  mcovers_pairs_one(_,_,[],U) <=> U=[].

mcovers_pairs_one2 @  mcovers_pairs_one(NPS,X,[Z|L2],U) <=> 
				ground(X),ground(Z) |
				(mcovers_obj(X,Z) ->  
				mbr_obj(X,MBrX),			
				mbr_obj(Z,MBrZ),
	              		mbr_bound_first(NPS,MBrX),
				mbr_bound_second(NPS,MBrZ),
				(mcovers_pairs_one(NPS,X,L2,L3),
				U=[(X,Z)|L3]);			
				(mcovers_pairs_one(NPS,X,L2,L3),
				U=L3)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NCOVERS (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ncovers @ ncovers(A,B) <=> 
				mbr(A,MBrA), 
				mbr(B,MBrB),
				overlap_mbr(MBrA,MBrB)
				|
			 	ncovers_obj(A,B). 

ncovers @ ncovers(A,B) <=> ground(A),ground(B) | fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NCOVERS OBJECTS (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ncovers_obj @  ncovers_obj(O1,O2) <=> neighbor_obj(O1,O2),inside_obj(O1,O2).

 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NCOVERS SET (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ncovers_set1 @ ncovers_set([],_,L2) <=> L2=[].

ncovers_set2 @ ncovers_set(_,[],L2) <=> L2=[].

ncovers_set3 @ ncovers_set([X1|L1],[X2|L2],PS) <=> 
				ground([X1|L1]),ground([X2|L2]),
				rtree([X1|L1],5,R1),
				rtree([X2|L2],5,R2)
				|
				ncovers_set(R1,R2,PS).

ncovers_set4 @ ncovers_set(A,B,PS) <=> 
				root(A,RA), 
				root(B,RB)
				|     
				ncovers_tree(_,PS,RA,RB).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NCOVERS PAIRS (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ncovers_pairs1 @ ncovers_pairs(_,[],_,U) <=> U=[].

ncovers_pairs2 @ ncovers_pairs(NPS,[X|L],L2,U) <=>
				ncovers_pairs_one(NPS,X,L2,L4),
				ncovers_pairs(NPS,L,L2,L5),
				append(L4,L5,L6),
				U=L6.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NCOVERS PAIRS (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


ncovers_tree1 @ ncovers_tree(NPS,PS,(MBrR1,Child1),(MBrR2,Child2)) <=>	   	 
			   	(overlap_mbr(MBrR1,MBrR2) ->
                		(root(Child1,RChild1),
				root(Child2,RChild2),
				ncovers_tree(NPS,PS,RChild1,RChild2));
				PS=[]). 
                     
			   
ncovers_tree2 @ ncovers_tree(NPS,PS,objects(Z1,_),objects(Z2,_)) <=> 	
				ncovers_pairs(NPS,Z1,Z2,Z), 
				PS=Z.

ncovers_tree3 @ ncovers_tree(NPS,PS,objects(Z1,N),(_,Child2)) <=> 
	               		root(Child2,RChild2) 
				| 
				ncovers_tree(NPS,PS,objects(Z1,N),RChild2).

ncovers_tree4 @ ncovers_tree(NPS,PS,(_,Child1),objects(Z2,N)) <=> 
               			root(Child1,RChild1) 
				| 
				ncovers_tree(NPS,PS,RChild1,objects(Z2,N)).


ncovers_tree5 @ ncovers_tree(NPS,PS,[(MBr1,Child1)|RNodes1],R2) <=> 
				ncovers_tree(NPS,PS1,(MBr1,Child1),R2),
				ncovers_tree(NPS,PS2,RNodes1,R2),
				append(PS1,PS2,PS).

ncovers_tree6 @ ncovers_tree(_,PS,[],_) <=> 
				PS=[].


ncovers_tree7 @ ncovers_tree(NPS,PS,(MBr1,Child1),[(MBr,Child)|RNodes]) <=> 
		ncovers_tree(NPS,PS1,(MBr1,Child1),(MBr,Child)),				
		ncovers_tree(NPS,PS2,(MBr1,Child1),RNodes),
                append(PS1,PS2,PS).         
                         
                 
ncovers_tree8 @ ncovers_tree(_,PS,(_,_),[]) <=> 
				PS=[].

ncovers_tree9 @ ncovers_tree(NPS,PS,objects(Z,N),[(MBr,Child)|RNodes]) <=> 
				ncovers_tree(NPS,PS1,objects(Z,N),(MBr,Child)),
				ncovers_tree(NPS,PS2,objects(Z,N),RNodes),
				append(PS1,PS2,PS).

ncovers_tree10 @ ncovers_tree(_,PS,objects(_,_),[]) <=> 
				PS=[].
 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mcovers PAIRS ONE (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ncovers_pairs_one1 @  ncovers_pairs_one(_,_,[],U) <=> U=[].

ncovers_pairs_one2 @  ncovers_pairs_one(NPS,X,[Z|L2],U) <=> 
				ground(X),ground(Z) |
				(ncovers_obj(X,Z) ->  
				mbr_obj(X,MBrX),			
				mbr_obj(Z,MBrZ),
	              		mbr_bound_first(NPS,MBrX),
				mbr_bound_second(NPS,MBrZ),
				(ncovers_pairs_one(NPS,X,L2,L3),
				U=[(X,Z)|L3]);		
				(ncovers_pairs_one(NPS,X,L2,L3),
				U=L3)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MCOVEREDBY (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mcoveredby @ mcoveredby(O1,O2) <=> mcovers(O2,O1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MCOVEREDBY SET (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mcoveredby_set1 @ mcoveredby_set([],_,L2) <=> L2=[].

mcoveredby_set2 @ mcoveredby_set(_,[],L2) <=> L2=[].

mcoveredby_set3 @ mcoveredby_set([X1|L1],[X2|L2],PS) <=> 
			ground([X1|L1]),ground([X2|L2]),
			rtree([X1|L1],5,R1),
			rtree([X2|L2],5,R2)
			|
			mcoveredby_set(R1,R2,PS).

mcoveredby_set4 @ mcoveredby_set(S1,S2,PS) <=> mcovers_set(S2,S1,PS).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NCOVEREDBY (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ncoveredby @ ncoveredby(O1,O2) <=> ncovers(O2,O1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NCOVEREDBY SET (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ncoveredby_set1 @ ncoveredby_set([],_,L2) <=> L2=[].

ncoveredby_set2 @ ncoveredby_set(_,[],L2) <=> L2=[].

ncoveredby_set3 @ ncoveredby_set([X1|L1],[X2|L2],PS) <=> 
			ground([X1|L1]),ground([X2|L2]),
			rtree([X1|L1],5,R1),
			rtree([X2|L2],5,R2)
			|
			ncoveredby_set(R1,R2,PS).

ncoveredby_set4 @ ncoveredby_set(S1,S2,PS) <=> 
			ncovers_set(S2,S1,PS).

 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RANGE (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

range1 @ range(O,window((X,Y),(Z,T))) <=> 
				ground(O),ground(window((X,Y),(Z,T))),
				mbr(O,MBr),
				overlap_mbr(MBr,mbr((X,Y),(Z,T)))
				|
				range_obj(O,window((X,Y),(Z,T))).

range2 @ range(O,circle((X,Y),D)) <=>
				mbr(O,MBr),
				MBrCXDown is X-D,
				MBrCYDown is Y-D,
				MBrCXUp is X+D,
				MBrCYUp is Y+D,
				overlap_mbr(MBr,mbr((MBrCXDown,MBrCYDown),(MBrCXUp,MBrCYUp)))
				|
				range_obj(O,circle((X,Y),D)).

range3 @ range(A,B) <=> ground(A),ground(B) | fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RANGE OBJ (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

range_obj1 @ range_obj((X,Y),window((Z,T),(U,V))) <=>  
				leq(Z,X),leq(T,Y),leq(X,U),leq(Y,V) | true.

range_obj2 @ range_obj(line(L),window((Z,T),(U,V))) <=>
		  		range_line_window(L,window((Z,T),(U,V))).		 

range_obj3 @ range_obj(polygon([P|L]),window((Z,T),(U,V))) <=>
				append([P|L],[P],L2),
		  		range_line_window(L2,window((Z,T),(U,V))).	

range_obj4 @ range_obj(region([P|L]),window((Z,T),(U,V))) <=>
				append([P|L],[P],L2),
		  		range_line_window(L2,window((Z,T),(U,V))).	

range_obj5 @ range_obj(diff(O1,_),W) <=> range_obj(O1,W).
 

range_obj6 @ range_obj((X,Y),circle((Z,T),D)) <=> 
			 	pmindist_obj((X,Y),(Z,T),_,E), E=<D | true.

range_obj7 @ range_obj(line(L),circle((Z,T),D)) <=>
		  		range_line_circle(L,circle((Z,T),D)).		 

range_obj8 @ range_obj(polygon([P|L]),circle((Z,T),D)) <=>
				append([P|L],[P],L2),
		  		range_line_circle(L2,circle((Z,T),D)).	
	 
range_obj9 @ range_obj(region([P|L]),circle((Z,T),D)) <=>
			append([P|L],[P],L2),
		  	range_line_circle(L2,circle((Z,T),D)).	

rangeobj10 @ range_obj(_,_) <=> fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RANGE LINE WINDOW (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

range_line_window1 @ range_line_window([],_) <=> true.

range_line_window2 @ range_line_window([P|RP],W) <=> 
					range_obj(P,W),	
					range_line_window(RP,W).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RANGE LINE WINDOW (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

range_line_circle1 @ range_line_circle([],_) <=> true.

range_line_circle2 @ range_line_circle([P|RP],C) <=> 
				range_obj(P,C),
				range_line_circle(RP,C). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% range SET (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

range_set1 @ range_set([],_,L2) <=> L2=[].
 
range_set2 @ range_set([X1|L1],D,PS) <=> 
			ground([X1|L1]),
			ground(D),
			rtree([X1|L1],5,R1)
			|
			range_set(R1,D,PS).

range_set3 @ range_set(A,D,PS) <=> 
				root(A,RA)
				|     
				range_tree(_,PS,RA,D).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% range_PAIRS (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

range_pairs1 @ range_pairs(_,[],_,U) <=> U=[].

range_pairs2 @ range_pairs(NPS,[X|L],D,U) <=>  
				range_pairs_one(NPS,X,D,L4),
				range_pairs(NPS,L,D,L5),
				append(L4,L5,L6),
				U=L6.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% range PAIRS ONE (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

range_pairs_one1 @  range_pairs_one(NPS,X,D,U) <=>  
			range_obj(X,D) 
			|
			mbr_obj(X,MBr),
			mbr_bound(NPS,MBr),
			U=[X].

range_pairs_one2 @  range_pairs_one(_,_,_,U) <=> U=[].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% range_tree (TOPOLOGICAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


range_tree1 @ range_tree(NPS,PS,(MBrR1,Child1),D) <=>	   	
			   	mbr(D,MBrD),  
				(overlap_mbr(MBrR1,MBrD) ->  
                		root(Child1,RChild1),
				range_tree(NPS,PS,RChild1,D)
				; PS=[]). 
                     
			   
 
range_tree2 @ range_tree(NPS,PS,objects(Z1,_),D) <=> 	  
				range_pairs(NPS,Z1,D,Z), 
				PS=Z.

range_tree3 @ range_tree(NPS,PS,[(MBr1,Child1)|RNodes1],D) <=> 
				range_tree(NPS,PS1,(MBr1,Child1),D),
				range_tree(NPS,PS2,RNodes1,D),
				append(PS1,PS2,PS).

range_tree4 @ range_tree(_,PS,[],_) <=> 
				PS=[].



%---------------------------------------



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% METRIC CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- chr_constraint lower_bound/2,upper_bound/2,dist_bound_min/3, dist_bound_max/3. 

:- chr_constraint mindist/3, pmindist/3, omindist/3, pmindist/4, omindist/4, filtermin_pairs/3.

:-chr_constraint mindist_set/3, mindist_tree/3,mindist_tree_node/3,  
			mindist_pairs/4,mindist_pairs_one/4.

:-chr_constraint pmindist_set/3, pmindist_tree/3,pmindist_tree_node/3, pmindist_obj/4,
			 pmindist_pairs/6,pmindist_pairs_one/6.

:-chr_constraint omindist_set/3, omindist_tree/3,omindist_tree_node/3, omindist_obj/4,
			 omindist_pairs/6,omindist_pairs_one/6.

:- chr_constraint minimum_pmindist_point_line/4, minimum_pmindist_point_line_aux/6,
			 pmindist_point_segment/5, pmindist_lines/4,pmindist_lines_aux/6.

:- chr_constraint minimum_omindist_point_line/3, minimum_omindist_point_line_aux/4, 
				omindist_lines/3,omindist_lines_aux/4.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MINDIST (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


mindist @ mindist(A,B,D) <=> ground(A),ground(B) | pmindist_obj(A,B,_,D). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILTER PAIRS (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

filtermin_pairs1 @ filtermin_pairs([],_,U) <=> U=[].

filtermin_pairs2 @ filtermin_pairs([(X,Y)|L],DIST,U) <=> 
					(pmindist_obj(X,Y,_,K), 
					leq(K,DIST) -> 
					(filtermin_pairs(L,DIST,L2),
					U=[(X,Y)|L2]);
					(filtermin_pairs(L,DIST,L2),
					U=L2)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MINDIST_SET (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mindist_set1 @ mindist_set([X1|L1],[X2|L2],D) <=> 
			ground([X1|L1]),ground([X2|L2]),
			rtree([X1|L1],5,R1),
			rtree([X2|L2],5,R2)
			|
			mindist_set(R1,R2,D).



mindist_set2 @ mindist_set(A,B,D) <=> 
			root(A,RA), 
			root(B,RB) 
			| 
			mindist_tree(D,RA,RB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MINDIST_TREE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mindist_tree1 @ mindist_tree(D,[(MBr1,Child1)|RNodes1],R2) <=> 
				 mindist_tree_node(D,(MBr1,Child1),R2),
				 mindist_tree(D,RNodes1,R2).
			    

mindist_tree2 @ lower_bound(D,Lower), mindist_tree(D,[],_) <=>
		  		lower_bound(D,Lower), 
		  		D=Lower.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MINDIST_TREE_NODE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 
mindist_tree_node1 @ lower_bound(D,Lower), 
			mindist_tree_node(D,(MBrR1,Child1),(MBrR2,Child2))	
			<=>	  
			mindist_mbr(MBrR1,MBrR2,Dist),
		        leq(Dist,Lower) 
			|	    
                     	root(Child1,RChild1),
			root(Child2,RChild2), 
			lower_bound(D,Lower),
			mindist_tree_node(D,RChild1,RChild2).

mindist_tree_node2 @ lower_bound(D,Lower), 
			mindist_tree_node(D,(MBrR1,_),(MBrR2,_)) 
			<=>
			mindist_mbr(MBrR1,MBrR2,Dist),
		        lt(Lower,Dist) 
			|    
			lower_bound(D,Lower). 
				
                     
			   
mindist_tree_node3 @ lower_bound(D,Lower), 
			mindist_tree_node(D,objects(Z1,_),objects(Z2,_)) 
			<=> 
			mindist_pairs(Z1,Z2,Lower,Z),
			leq(Z,Lower) 
			|    
                        lower_bound(D,Z).
			    
			   

mindist_tree_node4 @ lower_bound(D,Lower), 
			mindist_tree_node(D,objects(Z1,_),objects(Z2,_)) 
			<=> 
			mindist_pairs(Z1,Z2,Lower,Z),		
			lt(Lower,Z) 
			|
			lower_bound(D,Lower).                     
			   

mindist_tree_node5 @ mindist_tree_node(D,objects(Z1,N),(_,Child2)) 
			<=> 
               		root(Child2,RChild2) 
			|
               		mindist_tree_node(D,objects(Z1,N),RChild2).

mindist_tree_node6 @ mindist_tree_node(D,(_,Child1),objects(Z2,N)) 
				<=> 
               			root(Child1,RChild1) 
				|
               			mindist_tree_node(D,RChild1,objects(Z2,N)).

mindist_tree_node7 @ mindist_tree_node(D,objects(Z1,N),[Node|RNodes]) 
			<=> 
               		mindist_tree_node(D,objects(Z1,N),Node),
               		mindist_tree_node(D,objects(Z1,N),RNodes).

mindist_tree_node8 @ mindist_tree_node(_,objects(_,_),[]) 
				<=> 
                		true.


mindist_tree_node9 @ mindist_tree_node(D,[Node|RNodes],objects(Z2,N)) 
			<=> 
               		mindist_tree_node(D,Node,objects(Z2,N)),
               		mindist_tree_node(D,RNodes,objects(Z2,N)).

mindist_tree_node10 @ mindist_tree_node(_,[],objects(_,_)) 
				<=> 
				true.
               



mindist_tree_node11 @ mindist_tree_node(D,[(MBr1,Child1)|RNodes1],R2) 
				<=> 
			    mindist_tree_node(D,(MBr1,Child1),R2),
			    mindist_tree_node(D,RNodes1,R2).
			    

mindist_tree_node12 @ lower_bound(D,Lower), mindist_tree_node(D,[],_) 
			<=> 
			lower_bound(D,Lower).


mindist_tree_node13 @ mindist_tree_node(D,(MBr1,Child1),[(MBr,Child)|RNodes]) 
			<=> 
			maxdist_mbr(MBr1,MBr,Lower), 
			lower_bound(D,Lower), 
                        mindist_tree_node(D,(MBr1,Child1),(MBr,Child)),
                      	mindist_tree_node(D,(MBr1,Child1),RNodes).
                      

mindist_tree_node14 @ lower_bound(D,Lower), 
			mindist_tree_node(D,(_,_),[]) 
			<=> 
			lower_bound(D,Lower).
 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MINDIST PAIRS (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mindist_pairs1 @ mindist_pairs([],_,MIN,U) <=> U=MIN.

mindist_pairs2 @ mindist_pairs([X|L],L2,INIT,MIN) <=>
			mindist_pairs_one(X,L2,INIT,MIN1),
			mindist_pairs(L,L2,MIN1,MIN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MINDIST PAIRS ONE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mindist_pairs_one1 @ mindist_pairs_one(_,[],MIN,K) <=> K=MIN.

mindist_pairs_one2 @ mindist_pairs_one(X,[Z|L2],INIT,MIN) <=>
					pmindist_obj(X,Z,_,K),
					((var(INIT);leq(K,INIT)) ->
					mindist_pairs_one(X,L2,K,MIN);
					mindist_pairs_one(X,L2,INIT,MIN)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PMINDIST (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pmindist @ pmindist(A,B,P) <=> ground(A),ground(B) | pmindist(A,B,P,_).

pmindist @ pmindist(A,B,P,D) <=> pmindist_obj(A,B,P,D). 

 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PMINDIST_SET (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pmindist_set1 @ pmindist_set([],_,L2) <=> L2=[].

pmindist_set2 @ pmindist_set(_,[],L2) <=> L2=[].

pmindist_set3 @ pmindist_set([X1|L1],[X2|L2],PS) <=> 
			ground([X1|L1]),ground([X2|L2]),
			rtree([X1|L1],5,R1),
			rtree([X2|L2],5,R2)
			|
			pmindist_set(R1,R2,PS).

pmindist_set4 @ pmindist_set(A,B,PS) <=> 
			root(A,RA), 
			root(B,RB) 
			|
			pmindist_tree(PS,RA,RB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PMINDIST_TREE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pmindist_tree1 @ pmindist_tree(PS,[(MBr1,Child1)|RNodes1],R2) <=> 
				pmindist_tree_node(PS,(MBr1,Child1),R2),
				pmindist_tree(PS,RNodes1,R2).

pmindist_tree2 @ dist_bound_min(PS,PR,Lower), pmindist_tree(PS,[],_) <=>  
				PS=PR,  
               			dist_bound_min(PS,PR,Lower).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PMINDIST_TREE_NODE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pmindist_tree_node1 @ dist_bound_min(PS,PR,Lower), 
			pmindist_tree_node(PS,(MBrR1,Child1),(MBrR2,Child2)) <=> 	
			mindist_mbr(MBrR1,MBrR2,Dist),
                 	leq(Dist,Lower),	 
                  	root(Child1,RChild1),
			root(Child2,RChild2) 
			|       
                  	dist_bound_min(PS,PR,Lower),  
                        pmindist_tree_node(PS,RChild1,RChild2).

pmindist_tree_node2 @ dist_bound_min(PS,PR,Lower), 
			pmindist_tree_node(PS,(MBrR1,_),(MBrR2,_)) <=>	
			mindist_mbr(MBrR1,MBrR2,Dist),
		      	lt(Lower,Dist)    
			|     
			dist_bound_min(PS,PR,Lower). 
            

pmindist_tree_node3 @ dist_bound_min(PS,PR,Lower), 
			pmindist_tree_node(PS,objects(Z1,_),objects(Z2,_)) <=> 
			pmindist_pairs(Z1,Z2,[],Lower,Z,ZP), 
			eqn(Z,Lower), 
			ground(PR) 
			| 		 	    
                      	dist_bound_min(PS,ZP,Z),
                      	dist_bound_min(PS,PR,Lower).	

pmindist_tree_node4 @ dist_bound_min(PS,PR,Lower), 
			pmindist_tree_node(PS,objects(Z1,_),objects(Z2,_)) <=> 
			pmindist_pairs(Z1,Z2,[],Lower,Z,ZP), 
			eqn(Z,Lower), 
			var(PR)
			| 
			dist_bound_min(PS,ZP,Z).
                      


		    
pmindist_tree_node5 @ dist_bound_min(PS,_,Lower), 
			pmindist_tree_node(PS,objects(Z1,_),objects(Z2,_)) <=> 
			pmindist_pairs(Z1,Z2,[],Lower,Z,ZP), 
			lt(Z,Lower)		  
			|  
			dist_bound_min(PS,ZP,Z).
                      			    			   

pmindist_tree_node6 @ dist_bound_min(PS,PR,Lower), 
			pmindist_tree_node(PS,objects(Z1,_),objects(Z2,_)) <=> 
			pmindist_pairs(Z1,Z2,[],Lower,Z,_),  
			lt(Lower,Z)			    	
			|   
			dist_bound_min(PS,PR,Lower).

                            
pmindist_tree_node7 @ pmindist_tree_node(PS,objects(Z1,N),(_,Child2)) <=> 
               			root(Child2,RChild2) 
				| 
				pmindist_tree_node(PS,objects(Z1,N),RChild2).

pmindist_tree_node8 @ pmindist_tree_node(PS,(_,Child1),objects(Z2,N)) <=> 
               			root(Child1,RChild1) 
				| 
				pmindist_tree_node(PS,RChild1,objects(Z2,N)).



pmindist_tree_node9 @ pmindist_tree_node(PS,[(MBr1,Child1)|RNodes1],R2) <=> 
				pmindist_tree_node(PS,(MBr1,Child1),R2), 
				pmindist_tree_node(PS,RNodes1,R2).

pmindist_tree_node10 @ dist_bound_min(PS,PR,Lower), 
				pmindist_tree_node(PS,[],_) <=> 
				dist_bound_min(PS,PR,Lower).

pmindist_tree_node11 @ pmindist_tree_node(PS,(MBr1,Child1),[(MBr,Child)|RNodes]) <=> 
                  	maxdist_mbr(MBr1,MBr,Lower) 
			|      
                  	dist_bound_min(PS,[],Lower), 
			pmindist_tree_node(PS,(MBr1,Child1),(MBr,Child)),            
			pmindist_tree_node(PS,(MBr1,Child1),RNodes).
                  

pmindist_tree_node12 @ dist_bound_min(PS,PR,Lower), 
			pmindist_tree_node(PS,(_,_),[]) <=> 
			dist_bound_min(PS,PR,Lower).

pmindist_tree_node13 @ pmindist_tree_node(PS,objects(Z,N),[(MBr,Child)|RNodes]) <=> 
			pmindist_tree_node(PS,objects(Z,N),(MBr,Child)),
			pmindist_tree_node(PS,objects(Z,N),RNodes).

pmindist_tree_node14 @ dist_bound_min(PS,PR,Lower), 
			pmindist_tree_node(PS,objects(_,_),[]) <=> 
			dist_bound_min(PS,PR,Lower).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PMINDIST PAIRS (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pmindist_pairs1 @ pmindist_pairs([],_,K,MIN,V,U) <=> U=K,V=MIN.

pmindist_pairs2 @ pmindist_pairs([X|L],L2,K,INIT,MIN,KRES) <=> 
			pmindist_pairs_one(X,L2,K,INIT,MIN1,KRES1),
			pmindist_pairs(L,L2,KRES1,MIN1,MIN,KRES).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PMINDIST PAIRS ONE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pmindist_pairs_one1 @ pmindist_pairs_one(_,[],K,MIN,V,U) <=> U=K,V=MIN.

pmindist_pairs_one2 @ pmindist_pairs_one(X,[Z|L2],LP,INIT,MIN,KRES) <=>
			(pmindist_obj(X,Z,POINTS,K),
			eqn(K,INIT) ->
			append(POINTS,LP,NEWPOINTS),
			pmindist_pairs_one(X,L2,NEWPOINTS,K,MIN,KRES);
			(pmindist_obj(X,Z,POINTS,K),  
			lt(K,INIT) ->
			pmindist_pairs_one(X,L2,POINTS,K,MIN,KRES);
			pmindist_pairs_one(X,L2,LP,INIT,MIN,KRES))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PMINDIST OBJ (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pmindist_obj1 @ pmindist_obj((X,Y),(Z,T),POINTS,D) <=>
			A is (X-Z)*(X-Z), 
			B is (Y-T)*(Y-T), 
			D is sqrt(A+B),
			POINTS=[((X,Y),(Z,T))].

pmindist_obj2 @ pmindist_obj((X,Y),line(L),POINTS,D)<=> 
			minimum_pmindist_point_line((X,Y),L,POINTS,D).

pmindist_obj3 @ pmindist_obj((X,Y),polygon([P|L]),POINTS,D)<=>
			append([P|L],[P],L2), 
			minimum_pmindist_point_line((X,Y),L2,POINTS,D).

pmindist_obj4 @ pmindist_obj(line(L),(X,Y),POINTS,D)<=> 
			pmindist_obj((X,Y),line(L),POINTS,D).

pmindist_obj5 @ pmindist_obj(polygon(L),(X,Y),POINTS,D)<=> 
			pmindist_obj((X,Y),polygon(L),POINTS,D).

pmindist_obj6 @ pmindist_obj(region(L),(X,Y),POINTS,D)<=> 
			pmindist_obj((X,Y),region(L),POINTS,D).

pmindist_obj7 @ pmindist_obj(line(L1),line(L2),POINTS,D) <=> 
			pmindist_lines(L1,line(L2),POINTS,D).

pmindist_obj8 @ pmindist_obj(line(L1),polygon([P|L2]),POINTS,D) <=>
			append([P|L2],[P],L3), 
			pmindist_lines(L1,line(L3),POINTS,D).

pmindist_obj9 @ pmindist_obj(polygon(L),line(L2),POINTS,D)<=> 
			pmindist_obj(line(L2),polygon(L),POINTS,D).

pmindist_obj10 @ pmindist_obj(polygon([P1|L1]),polygon([P2|L2]),POINTS,D) <=>
			append([P1|L1],[P1],L3), 
			append([P2|L2],[P2],L4),
			pmindist_lines(L3,line(L4),POINTS,D).

pmindist_obj11 @ pmindist_obj((X,Y),region(L),POINTS,D)<=>
			inside_point_obj((X,Y),region(L)) 
			| 
			POINTS=[((X,Y),(X,Y))], D=0.

pmindist_obj12 @ pmindist_obj((X,Y),region([P|L]),POINTS,D)<=>
			append([P|L],[P],L2), 
			minimum_pmindist_point_line((X,Y),L2,POINTS,D).

pmindist_obj13 @ pmindist_obj(line(L1),region(L2),POINTS,D)<=>
			overlap(line(L1),region(L2)) | 
			inter_obj(line(L1),region(L2),POINTS2), 
			setpoints_set(POINTS2,NODES), 
			create_pairs(NODES,NODES,POINTS),
			D=0.

pmindist_obj14 @ pmindist_obj(line(L1),region(L2),POINTS,D)<=>
			pmindist_obj(line(L1),polygon(L2),POINTS,D).

pmindist_obj15 @ pmindist_obj(region(L2),line(L1),POINTS,D) <=>
			pmindist_obj(line(L1),region(L2),POINTS,D).

pmindist_obj16 @ pmindist_obj(polygon(L1),region(L2),POINTS,D)<=>
			overlap(polygon(L1),region(L2)) | 
			inter_obj(polygon(L1),region(L2),POINTS2), 
			setpoints_set(POINTS2,NODES),  
			create_pairs(NODES,NODES,POINTS),
			D=0.

pmindist_obj17 @ pmindist_obj(polygon(L1),region(L2),POINTS,D)<=>
			pmindist_obj(polygon(L1),polygon(L2),POINTS,D).


pmindist_obj18 @ pmindist_obj(region(L2),polygon(L1),POINTS,D) <=>
			pmindist_obj(polygon(L1),region(L2),POINTS,D).

pmindist_obj19 @ pmindist_obj(region(L1),region(L2),POINTS,D)<=>
			overlap(region(L1),region(L2)) | 
			inter_obj(region(L1),region(L2),POINTS2), 
			setpoints_set(POINTS2,NODES), 
			create_pairs(NODES,NODES,POINTS),
			D=0.

pmindist_obj20 @ pmindist_obj(region(L1),region(L2),POINTS,D)<=>
			pmindist_obj(polygon(L1),polygon(L2),POINTS,D).

pmindist_obj21 @ pmindist_obj(diff(O1,O2),Z,POINTS,D) <=> pmindist_obj(O1,Z,POINTS1,D),filter_bottom1(POINTS1,O2,POINTS),POINTS\=[] | true.

pmindist_obj22 @ pmindist_obj(Z,diff(O1,O2),POINTS,D) <=> pmindist_obj(Z,O1,POINTS1,D),filter_bottom2(POINTS1,O2,POINTS),POINTS\=[] | true.

pmindist_obj23 @ pmindist_obj(diff(_,_),_,POINTS,D) <=> POINTS=[],D=100000000000000.

pmindist_obj23 @ pmindist_obj(_,diff(_,_),POINTS,D) <=> POINTS=[],D=100000000000000.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILTER BOTTOM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

filter_bottom1([],_,[]).
filter_bottom1([(O,_)|RO],O2,RO2):-inside_obj(O,O2),!,filter_bottom1(RO,O2,RO2).
filter_bottom1([(O,Q)|RO],O2,[(O,Q)|RO2]):-filter_bottom1(RO,O2,RO2).

filter_bottom2([],_,[]).
filter_bottom2([(_,O)|RO],O2,RO2):-inside_obj(O,O2),!,filter_bottom2(RO,O2,RO2).
filter_bottom2([(Q,O)|RO],O2,[(Q,O)|RO2]):-filter_bottom2(RO,O2,RO2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CREATE PAIRS (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_pairs([],_,[]).

create_pairs([X|R],L,P):-create_pairs_one(X,L,P1),
			create_pairs(R,L,P2),
			append(P1,P2,P).

create_pairs_one(_,[],[]).

create_pairs_one(X,[Y|R],[(X,Y)|RP]):-
			create_pairs_one(X,R,RP).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PMINDIST LINES (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pmindist_lines @ pmindist_lines([P|RP],line(L),POINTS,D) <=> 
				minimum_pmindist_point_line(P,L,_,D1),
				pmindist_lines_aux([P|RP],line(L),[],POINTS,D1,D).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PMINDIST LINES AUX (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pmindist_lines_aux1 @ pmindist_lines_aux([],line(_),POINTSOLD,POINTS,DOLD,D) <=> 
						D=DOLD,POINTS=POINTSOLD.


pmindist_lines_aux2 @ pmindist_lines_aux([P|RP],line(L),_,POINTS,DOLD,D) <=> 
				minimum_pmindist_point_line(P,L,POINTSNEW,DNEW), 
				lt(DNEW,DOLD) 
				|
				pmindist_lines_aux(RP,line(L),POINTSNEW,POINTS,DNEW,D).

pmindist_lines_aux3 @  pmindist_lines_aux([P|RP],line(L),POINTSOLD,POINTS,DOLD,D) <=> 
				minimum_pmindist_point_line(P,L,_,DNEW), 
				gt(DNEW,DOLD) 
				|
				pmindist_lines_aux(RP,line(L),POINTSOLD,POINTS,DOLD,D).

pmindist_lines_aux4 @  pmindist_lines_aux([P|RP],line(L),POINTSOLD,POINTS,DOLD,D) <=> 
				minimum_pmindist_point_line(P,L,POINTSNEW,DNEW),
				eqn(DNEW,DOLD) 
				|
				append(POINTSOLD,POINTSNEW,POINTSTO),
				pmindist_lines_aux(RP,line(L),POINTSTO,POINTS,DOLD,D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MINIMUM PMINDIST POINT LINE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

minimum_pmindist_point_line @ 
		minimum_pmindist_point_line((X,Y),[P1,P2|RP],POINTS,D) <=> 
			pmindist_point_segment((X,Y),P1,P2,POINTS1,D1),
			minimum_pmindist_point_line_aux((X,Y),[P2|RP],POINTS1,POINTS,D1,D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MINIMUM PMINDIST POINT LINE AUX (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


minimum_pmindist_point_line_aux1 @ 
	minimum_pmindist_point_line_aux((X,Y),[P1,P2|RP],_,POINTS,DOLD,D) <=> 
		pmindist_point_segment((X,Y),P1,P2,POINTSNEW,DIST), 
		lt(DIST,DOLD) 
		|  
		minimum_pmindist_point_line_aux((X,Y),[P2|RP],POINTSNEW,POINTS,DIST,D).

minimum_pmindist_point_line_aux2 @ 
	minimum_pmindist_point_line_aux((X,Y),[P1,P2|RP],POINTSOLD,POINTS,DOLD,D) <=> 
		pmindist_point_segment((X,Y),P1,P2,_,DIST), 
		gt(DIST,DOLD) 
		|  
		minimum_pmindist_point_line_aux((X,Y),[P2|RP],POINTSOLD,POINTS,DOLD,D).

minimum_pmindist_point_line_aux3 @ 
	minimum_pmindist_point_line_aux((X,Y),[P1,P2|RP],POINTSOLD,POINTS,DOLD,D) <=> 
		pmindist_point_segment((X,Y),P1,P2,POINTSNEW,DIST),
		eqn(DIST,DOLD) 
		|  
		append(POINTSOLD,POINTSNEW,POINTSTO),
		minimum_pmindist_point_line_aux((X,Y),[P2|RP],POINTSTO,POINTS,DOLD,D).

minimum_pmindist_point_line_aux4 @ 
	minimum_pmindist_point_line_aux((_,_),_,POINTSOLD,POINTS,DOLD,D) <=>  
		D=DOLD,POINTS=POINTSOLD.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MINDIST POINT SEGMENT (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pmindist_point_segment1 @ pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>
				inside_point_obj((U,V),line([(X,Y),(Z,T)]))
				|
				POINTS=[((U,V),(U,V))],
				D=0.


pmindist_point_segment2 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), neqn(Y,T),   
		SLOPE1 is (Y-T)/(X-Z),
		SLOPE2 is -1/SLOPE1, 
                C1 is (-1*SLOPE2*V + V-Y+SLOPE1*X)/ (SLOPE1-SLOPE2),
		C2 is SLOPE1 * (C1 - X) + Y, 
 		leq(X,C1), leq(C1,Z), leq(Y,C2), leq(C2,T), 
		DIST is sqrt((U-C1)*(U-C1)+(V-C2)*(V-C2)) 
		| 
		D=DIST, POINTS=[((U,V),(C1,C2))].

pmindist_point_segment3 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), neqn(Y,T),   
		SLOPE1 is (Y-T)/(X-Z),
		SLOPE2 is -1/SLOPE1,
                C1 is (-1*SLOPE2*V + V-Y+SLOPE1*X)/ (SLOPE1-SLOPE2),		C2 is SLOPE1 * (C1 - X) + Y,
		leq(Z,C1), leq(C1,X), leq(Y,C2), leq(C2,T), 
		DIST is sqrt((U-C1)*(U-C1)+(V-C2)*(V-C2))
		| 
		D=DIST, POINTS=[((U,V),(C1,C2))].


pmindist_point_segment4 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), neqn(Y,T),  
		SLOPE1 is (Y-T)/(X-Z),
		SLOPE2 is -1/SLOPE1, 
                C1 is (-1*SLOPE2*V + V-Y+SLOPE1*X)/ (SLOPE1-SLOPE2),
		C2 is SLOPE1 * (C1 - X) + Y, 
		lt(C1,X), leq(X,Z), leq(Y,C2), leq(C2,T), 
		DIST is sqrt((U-X)*(U-X)+(V-C2)*(V-C2))
		| 
		D=DIST, POINTS=[((U,V),(X,C2))].

pmindist_point_segment5 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), neqn(Y,T),   
		SLOPE1 is (Y-T)/(X-Z),
		SLOPE2 is -1/SLOPE1, 
                C1 is (-1*SLOPE2*V + V-Y+SLOPE1*X)/ (SLOPE1-SLOPE2),
		C2 is SLOPE1 * (C1 - X) + Y, 
		lt(C1,X), leq(X,Z), leq(T,C2), leq(C2,Y), 
		DIST is sqrt((U-X)*(U-X)+(V-C2)*(V-C2)) 
		| 
		D=DIST, POINTS=[((U,V),(X,C2))].

 

pmindist_point_segment6 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), neqn(Y,T),  
		SLOPE1 is (Y-T)/(X-Z),
		SLOPE2 is -1/SLOPE1, 
                C1 is (-1*SLOPE2*V + V-Y+SLOPE1*X)/ (SLOPE1-SLOPE2),
		C2 is SLOPE1 * (C1 - X) + Y, 
		leq(X,Z), lt(Z,C1), leq(Y,C2), leq(C2,T), 
		DIST is sqrt((U-Z)*(U-Z)+(V-C2)*(V-C2))
		| 
		D=DIST, POINTS=[((U,V),(Z,C2))].

pmindist_point_segment7 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), neqn(Y,T),   
		SLOPE1 is (Y-T)/(X-Z),
		SLOPE2 is -1/SLOPE1, 
                C1 is (-1*SLOPE2*V + V-Y+SLOPE1*X)/ (SLOPE1-SLOPE2),
		C2 is SLOPE1 * (C1 - X) + Y, 
		leq(X,Z), lt(Z,C1), leq(T,C2), leq(C2,Y), 
		DIST is sqrt((U-Z)*(U-Z)+(V-C2)*(V-C2)) 
		| 
		D=DIST, POINTS=[((U,V),(Z,C2))].

 
pmindist_point_segment8 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=> 	
		neqn(X,Z), neqn(Y,T),   
		SLOPE1 is (Y-T)/(X-Z),
		SLOPE2 is -1/SLOPE1, 
                C1 is (-1*SLOPE2*V + V-Y+SLOPE1*X)/ (SLOPE1-SLOPE2),
		C2 is SLOPE1 * (C1 - X) + Y, 
		leq(X,C1), leq(C1,Z), lt(C2,Y), leq(Y,T), 
		DIST is sqrt((U-C1)*(U-C1)+(V-Y)*(V-Y)) 
		| 
		D=DIST, POINTS=[((U,V),(C1,Y))].

pmindist_point_segment9 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), neqn(Y,T),   
		SLOPE1 is (Y-T)/(X-Z),
		SLOPE2 is -1/SLOPE1, 
                C1 is (-1*SLOPE2*V + V-Y+SLOPE1*X)/ (SLOPE1-SLOPE2),
		C2 is SLOPE1 * (C1 - X) + Y, 
		leq(Z,C1), leq(C1,X), lt(C2,Y), leq(Y,T), 
		DIST is sqrt((U-C1)*(U-C1)+(V-Y)*(V-Y)) 
		| 
		D=DIST, POINTS=[((U,V),(C1,Y))].

 
pmindist_point_segment10 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), neqn(Y,T),   
		SLOPE1 is (Y-T)/(X-Z),
		SLOPE2 is -1/SLOPE1, 
                C1 is (-1*SLOPE2*V + V-Y+SLOPE1*X)/ (SLOPE1-SLOPE2),
		C2 is SLOPE1 * (C1 - X) + Y, 
		leq(X,C1), leq(C1,Z), leq(Y,T), lt(T,C2), 
		DIST is sqrt((U-C1)*(U-C1)+(V-T)*(V-T)) 
		| 
		D=DIST, POINTS=[((U,V),(C1,T))].

pmindist_point_segment11 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), neqn(Y,T),   
		SLOPE1 is (Y-T)/(X-Z),
		SLOPE2 is -1/SLOPE1, 
                C1 is (-1*SLOPE2*V + V-Y+SLOPE1*X)/ (SLOPE1-SLOPE2),
		C2 is SLOPE1 * (C1 - X) + Y, 
		leq(Z,C1), leq(C1,X), leq(Y,T), lt(T,C2), 
		DIST is sqrt((U-C1)*(U-C1)+(V-T)*(V-T)) 
		| 
		D=DIST, POINTS=[((U,V),(C1,T))].

 
pmindist_point_segment12 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), neqn(Y,T),   
		SLOPE1 is (Y-T)/(X-Z),
		SLOPE2 is -1/SLOPE1, 
                C1 is (-1*SLOPE2*V + V-Y+SLOPE1*X)/ (SLOPE1-SLOPE2),
		C2 is SLOPE1 * (C1 - X) + Y, 
		lt(C1,X), leq(X,Z), lt(C2,Y), leq(Y,T), 
		DIST is sqrt((U-X)*(U-X)+(V-Y)*(V-Y)) 
		| 
		D=DIST, POINTS=[((U,V),(X,Y))].

pmindist_point_segment13 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), neqn(Y,T),   
		SLOPE1 is (Y-T)/(X-Z),
		SLOPE2 is -1/SLOPE1, 
                C1 is (-1*SLOPE2*V + V-Y+SLOPE1*X)/ (SLOPE1-SLOPE2),
		C2 is SLOPE1 * (C1 - X) + Y, 
		lt(C1,Z), leq(Z,X), lt(C2,Y), leq(Y,T), 
		DIST is sqrt((U-Z)*(U-Z)+(V-Y)*(V-Y)) 
		| 
		D=DIST, POINTS=[((U,V),(Z,Y))].

pmindist_point_segment14 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), neqn(Y,T),   
		SLOPE1 is (Y-T)/(X-Z),
		SLOPE2 is -1/SLOPE1, 
                C1 is (-1*SLOPE2*V + V-Y+SLOPE1*X)/ (SLOPE1-SLOPE2),
		C2 is SLOPE1 * (C1 - X) + Y, 
		lt(C1,X), leq(X,Z), leq(Y,T), lt(T,C2), 
		DIST is sqrt((U-X)*(U-X)+(V-T)*(V-T)) 
		| 
		D=DIST, POINTS=[((U,V),(X,T))].

pmindist_point_segment15 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), neqn(Y,T),   
		SLOPE1 is (Y-T)/(X-Z),
		SLOPE2 is -1/SLOPE1, 
                C1 is (-1*SLOPE2*V + V-Y+SLOPE1*X)/ (SLOPE1-SLOPE2),
		C2 is SLOPE1 * (C1 - X) + Y, 
		lt(C1,Z),  leq(Z,X), leq(Y,T), lt(T,C2), 
		DIST is sqrt((U-Z)*(U-Z)+(V-T)*(V-T)) 
		| 
		D=DIST, POINTS=[((U,V),(Z,T))].

pmindist_point_segment16 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), neqn(Y,T),   
		SLOPE1 is (Y-T)/(X-Z),
		SLOPE2 is -1/SLOPE1, 
                C1 is (-1*SLOPE2*V + V-Y+SLOPE1*X)/ (SLOPE1-SLOPE2),
		C2 is SLOPE1 * (C1 - X) + Y, 
		leq(X,Z), lt(Z,C1), lt(C2,Y), leq(Y,T), 
		DIST is sqrt((U-Z)*(U-Z)+(V-Y)*(V-Y)) 
		| 
		D=DIST, POINTS=[((U,V),(Z,Y))].

pmindist_point_segment17 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), neqn(Y,T),   
		SLOPE1 is (Y-T)/(X-Z),
		SLOPE2 is -1/SLOPE1, 
                C1 is (-1*SLOPE2*V + V-Y+SLOPE1*X)/ (SLOPE1-SLOPE2),
		C2 is SLOPE1 * (C1 - X) + Y, 
		leq(Z,X), lt(X,C1), lt(C2,Y), leq(Y,T), 
		DIST is sqrt((U-X)*(U-X)+(V-Y)*(V-Y)) 
		| 
		D=DIST, POINTS=[((U,V),(X,Y))].

 
pmindist_point_segment18 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), neqn(Y,T),   
		SLOPE1 is (Y-T)/(X-Z),
		SLOPE2 is -1/SLOPE1, 
                C1 is (-1*SLOPE2*V + V-Y+SLOPE1*X)/ (SLOPE1-SLOPE2),
		C2 is SLOPE1 * (C1 - X) + Y, 
		leq(X,Z), lt(Z,C1), leq(Y,T), lt(T,C2), 
		DIST is sqrt((U-Z)*(U-Z)+(V-T)*(V-T)) 
		| 
		D=DIST, POINTS=[((U,V),(Z,T))].

pmindist_point_segment19 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), neqn(Y,T),   
		SLOPE1 is (Y-T)/(X-Z),
		SLOPE2 is -1/SLOPE1, 
                C1 is (-1*SLOPE2*V + V-Y+SLOPE1*X)/ (SLOPE1-SLOPE2),
		C2 is SLOPE1 * (C1 - X) + Y, 
		leq(Z,X), lt(X,C1), leq(Y,T), lt(T,C2), 
		DIST is sqrt((U-X)*(U-X)+(V-T)*(V-T)) 
		| 
		D=DIST, POINTS=[((U,V),(X,T))].

pmindist_point_segment20 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), neqn(Y,T) 
		|
		pmindist_point_segment((U,V),(Z,T),(X,Y),POINTS,D).	 


 
pmindist_point_segment21 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		eqn(X,Z), neqn(Y,T),   
		C1 is X,
		C2 is V, leq(Y,V), leq(V,T),
		DIST is sqrt((U-C1)*(U-C1)+(V-C2)*(V-C2)) 
		| 
		D=DIST, POINTS=[((U,V),(C1,C2))].

pmindist_point_segment22 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		eqn(X,Z), neqn(Y,T),   
		C1 is X,
		lt(V,Y), leq(Y,T),  
		DIST is sqrt((U-C1)*(U-C1)+(V-Y)*(V-Y)) 
		| 
		D=DIST, POINTS=[((U,V),(C1,Y))].

pmindist_point_segment23 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		eqn(X,Z), neqn(Y,T),  
		C1 is X,
		leq(Y,T), lt(T,V), 
		DIST is sqrt((U-C1)*(U-C1)+(V-T)*(V-T)) 
		| 
		D=DIST, POINTS=[((U,V),(C1,T))].

pmindist_point_segment24 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		eqn(X,Z), neqn(Y,T),   
		C1 is X,
		C2 is V, leq(T,V), leq(V,Y),
		DIST is sqrt((U-C1)*(U-C1)+(V-C2)*(V-C2)) 
		| 
		D=DIST, POINTS=[((U,V),(C1,C2))].

pmindist_point_segment25 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		eqn(X,Z), neqn(Y,T),   
		C1 is X,
		lt(V,T), leq(T,Y), 
		DIST is sqrt((U-C1)*(U-C1)+(V-T)*(V-T)) 
		| 
		D=DIST, POINTS=[((U,V),(C1,T))].
 
pmindist_point_segment26 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		eqn(X,Z), neqn(Y,T),  
		C1 is X,
		leq(T,Y), lt(Y,V),  
		DIST is sqrt((U-C1)*(U-C1)+(V-Y)*(V-Y)) 
		| 
		D=DIST, POINTS=[((U,V),(C1,Y))].

 
pmindist_point_segment27 @ 
		pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), eqn(Y,T),   
		C1 is U,
		C2 is Y, leq(X,U), leq(U,Z),
		DIST is sqrt((U-C1)*(U-C1)+(V-C2)*(V-C2)) 
		| 
		D=DIST, POINTS=[((U,V),(C1,C2))].

pmindist_point_segment28 @ pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), eqn(Y,T),   
		C2 is Y, lt(U,X), leq(X,Z),
		DIST is sqrt((U-X)*(U-X)+(V-C2)*(V-C2)) 
		| 
		D=DIST, POINTS=[((U,V),(X,C2))].

pmindist_point_segment29 @ pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), eqn(Y,T),  
		C2 is Y, leq(X,Z), lt(Z,U),
		DIST is sqrt((U-Z)*(U-Z)+(V-C2)*(V-C2)) 
		| 
		D=DIST, POINTS=[((U,V),(Z,C2))].

pmindist_point_segment30 @ pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), eqn(Y,T),   
		C1 is U,
		C2 is Y, leq(Z,U), leq(U,X),
		DIST is sqrt((U-C1)*(U-C1)+(V-C2)*(V-C2)) 
		| 
		D=DIST, POINTS=[((U,V),(C1,C2))].

pmindist_point_segment31 @ pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), eqn(Y,T),   
		C2 is Y, lt(U,Z), leq(Z,X),
		DIST is sqrt((U-Z)*(U-Z)+(V-C2)*(V-C2)) 
		| 
		D=DIST, POINTS=[((U,V),(Z,C2))].
 
pmindist_point_segment32 @ pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		neqn(X,Z), eqn(Y,T),  
		 C2 is Y, leq(Z,X), lt(X,U),
		 DIST is sqrt((U-X)*(U-X)+(V-C2)*(V-C2)) 
		 | 
		 D=DIST, POINTS=[((U,V),(X,C2))].

pmindist_point_segment33 @ pmindist_point_segment((U,V),(X,Y),(Z,T),POINTS,D) <=>  
		eqn(X,Z), eqn(Y,T),  
		  
		 DIST is sqrt((U-X)*(U-X)+(V-Y)*(V-Y)) 
		 | 
		 D=DIST, POINTS=[((U,V),(X,Y))].
 

 				   				  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OMINDIST (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

omindist1 @ omindist(A,B,P) <=> ground(A),ground(B) | omindist(A,B,P,_).

omindist2 @ omindist(A,B,O,D) <=> omindist_obj(A,B,O,D). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% omindist_SET (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

omindist_set1 @ omindist_set([],_,L2) <=> L2=[].

omindist_set2 @ omindist_set(_,[],L2) <=> L2=[].

omindist_set3 @ omindist_set([X1|L1],[X2|L2],PS) <=> ground([X1|L1]),ground([X2|L2]),
  			rtree([X1|L1],5,R1),
  			rtree([X2|L2],5,R2) |
  			omindist_set(R1,R2,PS).

omindist_set4 @ omindist_set(A,B,PS) <=> 
			root(A,RA), 
			root(B,RB) 
			|
			omindist_tree(PS,RA,RB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% omindist_TREE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

omindist_tree1 @ omindist_tree(PS,[(MBr1,Child1)|RNodes1],R2) <=> 
				omindist_tree_node(PS,(MBr1,Child1),R2),
				omindist_tree(PS,RNodes1,R2).

omindist_tree2 @ dist_bound_min(PS,PR,Lower), omindist_tree(PS,[],_) <=>  
				PS=PR,
               			dist_bound_min(PS,PR,Lower).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% omindist_TREE_NODE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

omindist_tree_node1 @ dist_bound_min(PS,PR,Lower), 
			omindist_tree_node(PS,(MBrR1,Child1),(MBrR2,Child2)) <=>	
		 	mindist_mbr(MBrR1,MBrR2,Dist),
                 	leq(Dist,Lower),	   
                  	root(Child1,RChild1),
			root(Child2,RChild2) 
			 |      
                  	dist_bound_min(PS,PR,Lower),  
                        omindist_tree_node(PS,RChild1,RChild2).

omindist_tree_node2 @ dist_bound_min(PS,PR,Lower), 
			omindist_tree_node(PS,(MBrR1,_),(MBrR2,_)) <=>	 
 			mindist_mbr(MBrR1,MBrR2,Dist),
		      	lt(Lower,Dist)   
			|    
			dist_bound_min(PS,PR,Lower). 
            

omindist_tree_node3 @ dist_bound_min(PS,PR,Lower), 
			omindist_tree_node(PS,objects(Z1,_),objects(Z2,_)) <=> 
			omindist_pairs(Z1,Z2,[],Lower,Z,ZP),
			eqn(Z,Lower), 
			ground(PR) 
			| 			    
                      	dist_bound_min(PS,ZP,Z),
                      	dist_bound_min(PS,PR,Lower).	

omindist_tree_node4 @ dist_bound_min(PS,PR,Lower), 
			omindist_tree_node(PS,objects(Z1,_),objects(Z2,_)) <=> 
			omindist_pairs(Z1,Z2,[],Lower,Z,ZP),
			eqn(Z,Lower), 
			var(PR)
			| 
			dist_bound_min(PS,ZP,Z).
                      


		    
omindist_tree_node5 @ dist_bound_min(PS,_,Lower), 
				omindist_tree_node(PS,objects(Z1,_),objects(Z2,_)) <=> 
			    	omindist_pairs(Z1,Z2,[],Lower,Z,ZP),
			    	lt(Z,Lower)			  
				| 
			    	dist_bound_min(PS,ZP,Z).
                      			    			   

omindist_tree_node6 @ dist_bound_min(PS,PR,Lower), 
				omindist_tree_node(PS,objects(Z1,_),objects(Z2,_)) <=> 
			    	omindist_pairs(Z1,Z2,[],Lower,Z,_),
			    	lt(Lower,Z)			    	
				|  
			    	dist_bound_min(PS,PR,Lower).

                            
omindist_tree_node7 @ omindist_tree_node(PS,objects(Z1,N),(_,Child2)) <=> 
  				root(Child2,RChild2) 
				| 
				omindist_tree_node(PS,objects(Z1,N),RChild2).

omindist_tree_node8 @ omindist_tree_node(PS,(_,Child1),objects(Z2,N)) <=> 
 				root(Child1,RChild1) 
				| 
				omindist_tree_node(PS,RChild1,objects(Z2,N)).



omindist_tree_node9 @ omindist_tree_node(PS,[(MBr1,Child1)|RNodes1],R2) <=> 
				omindist_tree_node(PS,(MBr1,Child1),R2), 
				omindist_tree_node(PS,RNodes1,R2).

omindist_tree_node10 @ dist_bound_min(PS,PR,Lower), 
			omindist_tree_node(PS,[],_) <=> 
			dist_bound_min(PS,PR,Lower).

omindist_tree_node11 @ omindist_tree_node(PS,(MBr1,Child1),[(MBr,Child)|RNodes]) <=> 
                  	maxdist_mbr(MBr1,MBr,Lower) 
		 	|      
                  	dist_bound_min(PS,[],Lower), 
			omindist_tree_node(PS,(MBr1,Child1),(MBr,Child)),            
			omindist_tree_node(PS,(MBr1,Child1),RNodes).
                  

omindist_tree_node12 @ dist_bound_min(PS,PR,Lower), 
				omindist_tree_node(PS,(_,_),[]) <=> 
				dist_bound_min(PS,PR,Lower).

omindist_tree_node13 @ omindist_tree_node(PS,objects(Z,N),[(MBr,Child)|RNodes]) <=> 
				omindist_tree_node(PS,objects(Z,N),(MBr,Child)),
				omindist_tree_node(PS,objects(Z,N),RNodes).

omindist_tree_node14 @ dist_bound_min(PS,PR,Lower), 
				omindist_tree_node(PS,objects(_,_),[]) <=> 
				dist_bound_min(PS,PR,Lower).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% omindist PAIRS (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


omindist_pairs1 @ omindist_pairs([],_,K,MIN,V,U) <=> U=K,V=MIN.

omindist_pairs2 @ omindist_pairs([X|L],L2,K,INIT,MIN,KRES) <=> 
					omindist_pairs_one(X,L2,K,INIT,MIN1,KRES1),
					omindist_pairs(L,L2,KRES1,MIN1,MIN,KRES).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% omindist PAIRS ONE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


omindist_pairs_one1 @ omindist_pairs_one(_,[],K,MIN,V,U) <=> U=K,V=MIN.

omindist_pairs_one2 @ omindist_pairs_one(X,[Z|L2],LP,INIT,MIN,KRES) <=>
				(omindist_obj(X,Z,POINTS,K),
				eqn(K,INIT) ->
				append(POINTS,LP,NEWPOINTS),
				omindist_pairs_one(X,L2,NEWPOINTS,K,MIN,KRES);
				(omindist_obj(X,Z,POINTS,K),
				lt(K,INIT) ->
				omindist_pairs_one(X,L2,POINTS,K,MIN,KRES);
				omindist_pairs_one(X,L2,LP,INIT,MIN,KRES))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% omindist OBJ (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

omindist_obj1 @ omindist_obj((X,Y),(Z,T),OBJECTS,D) <=>
				A is (X-Z)*(X-Z), 
				B is (Y-T)*(Y-T), 
				D is sqrt(A+B),
				OBJECTS=[((X,Y),(Z,T))].

omindist_obj2 @ omindist_obj((X,Y),line(L),OBJECTS,D)<=> 
				minimum_omindist_point_line((X,Y),L,D),
				OBJECTS=[((X,Y),line(L))].

omindist_obj3 @ omindist_obj((X,Y),polygon([P|L]),OBJECTS,D)<=>
				append([P|L],[P],L2), 
				minimum_omindist_point_line((X,Y),L2,D),
				OBJECTS=[((X,Y),polygon([P|L]))].

omindist_obj4 @ omindist_obj(line(L),(X,Y),OBJECTS,D)<=> 
				omindist_obj((X,Y),line(L),OBJECTS,D).

omindist_obj5 @ omindist_obj(polygon(L),(X,Y),OBJECTS,D)<=> 
				omindist_obj((X,Y),polygon(L),OBJECTS,D).

omindist_obj6 @ omindist_obj(region(L),(X,Y),OBJECTS,D)<=> 
				omindist_obj((X,Y),region(L),OBJECTS,D).

omindist_obj7 @ omindist_obj(line(L1),line(L2),OBJECTS,D) <=> 
				omindist_lines(L1,line(L2),D),
				OBJECTS=[(line(L1),line(L2))].

omindist_obj8 @ omindist_obj(line(L1),polygon([P|L2]),OBJECTS,D) <=>
				append([P|L2],[P],L3), 
				omindist_lines(L1,line(L3),D),
				OBJECTS=[(line(L1),polygon([P|L2]))].

omindist_obj9 @ omindist_obj(polygon(L),line(L2),OBJECTS,D)<=> 
				omindist_obj(line(L2),polygon(L),OBJECTS,D).

omindist_obj10 @ omindist_obj(polygon([P1|L1]),polygon([P2|L2]),OBJECTS,D) <=>
				append([P1|L1],[P1],L3), 
				append([P2|L2],[P2],L4), 
				omindist_lines(L3,line(L4),D),
				OBJECTS=[(polygon([P1|L1]),polygon([P2|L2]))].

omindist_obj11 @ omindist_obj((X,Y),region(L),OBJECTS,D)<=>
				inside_point_obj((X,Y),region(L)) 
				| 
				OBJECTS=[((X,Y),region(L))], D=0.

omindist_obj12 @ omindist_obj((X,Y),region([P|L]),OBJECTS,D)<=>
				append([P|L],[P],L2), 
				minimum_omindist_point_line((X,Y),L2,D),
				OBJECTS=[((X,Y),region([P|L]))].

omindist_obj13 @ omindist_obj(line(L1),region(L2),OBJECTS,D)<=>
				overlap(line(L1),region(L2)) 
				| 
				OBJECTS=[(line(L1),region(L2))],
				D=0.

omindist_obj14 @ omindist_obj(line(L1),region(L2),OBJECTS,D)<=>
				pmindist_obj(line(L1),polygon(L2),_,D),
				OBJECTS=[(line(L1),region(L2))].

omindist_obj15 @ omindist_obj(region(L2),line(L1),OBJECTS,D) <=>
				omindist_obj(line(L1),region(L2),OBJECTS,D).

omindist_obj16 @ omindist_obj(polygon(L1),region(L2),OBJECTS,D)<=>
				overlap(polygon(L1),region(L2)) 
				| 
				OBJECTS=[(polygon(L1),region(L2))], 
				D=0.

omindist_obj17 @ omindist_obj(polygon(L1),region(L2),OBJECTS,D)<=>
				pmindist_obj(polygon(L1),polygon(L2),_,D),
				OBJECTS=[(polygon(L1),region(L2))].


omindist_obj18 @ omindist_obj(region(L2),polygon(L1),OBJECTS,D) <=>
				omindist_obj(polygon(L1),region(L2),OBJECTS,D).
				
				

omindist_obj19 @ omindist_obj(region(L1),region(L2),OBJECTS,D)<=>
				overlap(region(L1),region(L2)) 
				| 
				OBJECTS=[(region(L1),region(L2))],
				D=0.

omindist_obj20 @ omindist_obj(region(L1),region(L2),OBJECTS,D)<=>
				omindist_obj(polygon(L1),polygon(L2),_,D),
				OBJECTS=[(region(L1),region(L2))].

omindist_obj21 @ omindist_obj(diff(O1,O2),Z,OBJECTS,D) <=> omindist_obj(O1,Z,OBJECTS1,D),filter_bottom1(OBJECTS1,O2,OBJECTS),OBJECTS\=[] | true.

omindist_obj22 @ omindist_obj(Z,diff(O1,O2),OBJECTS,D) <=> omindist_obj(Z,O1,OBJECTS1,D),filter_bottom2(OBJECTS1,O2,OBJECTS),OBJECTS\=[] | true.

omindist_obj23 @ omindist_obj(diff(_,_),_,OBJECTS,D) <=> OBJECTS=[],D=100000000000000.

omindist_obj23 @ omindist_obj(_,diff(_,_),OBJECTS,D) <=> OBJECTS=[],D=100000000000000.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% omindist LINES (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

omindist_lines1 @ omindist_lines([P|RP],line(L),D) <=> 
				minimum_omindist_point_line(P,L,D1),
				omindist_lines_aux(RP,line(L),D1,D).


omindist_lines2 @ omindist_lines_aux([],line(_),DOLD,D) <=> 
				D=DOLD.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% omindist LINES AUX (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


omindist_lines_aux1 @ omindist_lines_aux([P|RP],line(L),DOLD,D) <=> 
				minimum_omindist_point_line(P,L,DNEW),
				lt(DNEW,DOLD) 
				|
				omindist_lines_aux(RP,line(L),DNEW,D).

omindist_lines_aux2 @  omindist_lines_aux([P|RP],line(L),DOLD,D) <=> 
				minimum_omindist_point_line(P,L,DNEW),
				gt(DNEW,DOLD) 
				|
				omindist_lines_aux(RP,line(L),DOLD,D).

omindist_lines_aux3 @  omindist_lines_aux([P|RP],line(L),DOLD,D) <=> 
				minimum_omindist_point_line(P,L,DNEW),
				DNEW = DOLD 
				|
				omindist_lines_aux(RP,line(L),DOLD,D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MINIMUM omindist POINT LINE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

minimum_omindist_point_line @ 
		minimum_omindist_point_line((X,Y),[P1,P2|RP],D) <=> 
				pmindist_point_segment((X,Y),P1,P2,_,D1),
				minimum_omindist_point_line_aux((X,Y),[P2|RP],D1,D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MINIMUM omindist POINT LINE AUX (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


minimum_omindist_point_line_aux1 @ 
			minimum_omindist_point_line_aux((X,Y),[P1,P2|RP],DOLD,D) <=> 
				pmindist_point_segment((X,Y),P1,P2,_,DIST),
				lt(DIST,DOLD) 
				|  
				minimum_omindist_point_line_aux((X,Y),[P2|RP],DIST,D).

minimum_omindist_point_line_aux2 @ 
			minimum_omindist_point_line_aux((X,Y),[P1,P2|RP],DOLD,D) <=> 
				pmindist_point_segment((X,Y),P1,P2,_,DIST),
				gt(DIST,DOLD) 
				|  
				minimum_omindist_point_line_aux((X,Y),[P2|RP],DOLD,D).

minimum_omindist_point_line_aux3 @ 
			minimum_omindist_point_line_aux((X,Y),[P1,P2|RP],DOLD,D) <=> 
				pmindist_point_segment((X,Y),P1,P2,_,DIST),
				eqn(DIST,DOLD) 
				|  
 				minimum_omindist_point_line_aux((X,Y),[P2|RP],DOLD,D).

minimum_omindist_point_line_aux4 @ 
			minimum_omindist_point_line_aux((_,_),_,DOLD,D) <=>  
				D=DOLD.
 



 

 


:- chr_constraint maxdist/3, pmaxdist/3, omaxdist/3, pmaxdist/4, omaxdist/4, filtermax_pairs/3.

:-chr_constraint maxdist_set/3, maxdist_tree/3,maxdist_tree_node/3,  maxdist_obj/3,maxdist_pairs/4,
		maxdist_pairs_one/4,maximum_maxdist_outside/3,maxdist_outside_aux/4.

:-chr_constraint pmaxdist_set/3, pmaxdist_tree/3,pmaxdist_tree_node/3, pmaxdist_obj/4, 		
	pmaxdist_pairs/6,pmaxdist_pairs_one/6,maximum_pmaxdist_outside/4,pmaxdist_outside_aux/6.

:-chr_constraint omaxdist_set/3, omaxdist_tree/3,omaxdist_tree_node/3, omaxdist_obj/4, 
	omaxdist_pairs/6,omaxdist_pairs_one/6.


:- chr_constraint maximum_maxdist_point_line/3, maximum_maxdist_point_line_aux/4, 		
	maxdist_point_segment/4, maxdist_lines/3,maxdist_lines_aux/4.

:- chr_constraint maximum_pmaxdist_point_line/4, maximum_pmaxdist_point_line_aux/6, 	
	pmaxdist_point_segment/5, pmaxdist_lines/4,pmaxdist_lines_aux/6.

:- chr_constraint maximum_omaxdist_point_line/3, maximum_omaxdist_point_line_aux/4, 	
	omaxdist_point_segment/4, omaxdist_lines/3,omaxdist_lines_aux/4.

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% maxdist (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


maxdist @ maxdist(A,B,D) <=> ground(A),ground(B) | maxdist_obj(A,B,D). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILTER MAX PAIRS (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

filtermax_pairs1 @ filtermax_pairs([],_,U) <=> U=[].

filtermax_pairs2 @ filtermax_pairs([(X,Y)|L],DIST,U) <=>  
				maxdist_obj(X,Y,K),
				(geq(K,DIST) -> 
				filtermax_pairs(L,DIST,L2),
				U=[(X,Y)|L2];
				filtermax_pairs(L,DIST,L2),
				U=L2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% maxdist_SET (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 

maxdist_set1 @ maxdist_set([X1|L1],[X2|L2],D) <=> 
			ground([X1|L1]),ground([X2|L2]),
			rtree([X1|L1],5,R1),	
			rtree([X2|L2],5,R2)
			|	
			maxdist_set(R1,R2,D).



maxdist_set2 @ maxdist_set(A,B,D) <=> 
			root(A,RA), 
			root(B,RB) 
			| 
			maxdist_tree(D,RA,RB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% maxdist_TREE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maxdist_tree1 @ maxdist_tree(D,[(MBr1,Child1)|RNodes1],R2) <=> 
			    maxdist_tree_node(D,(MBr1,Child1),R2),
			    maxdist_tree(D,RNodes1,R2).
			    

maxdist_tree2 @ upper_bound(D,Upper), maxdist_tree(D,[],_) <=>
		  	upper_bound(D,Upper), 
		  	D=Upper.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% maxdist_TREE_NODE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maxdist_tree_node1 @ upper_bound(D,Upper), 
		maxdist_tree_node(D,(MBrR1,Child1),(MBrR2,Child2))	<=>	  
		 	maxdist_mbr(MBrR1,MBrR2,Dist),
		        geq(Dist,Upper),	   
                    	root(Child1,RChild1),
			root(Child2,RChild2) 
			|   
			upper_bound(D,Upper),
			maxdist_tree_node(D,RChild1,RChild2).

maxdist_tree_node2 @ upper_bound(D,Upper), 
		maxdist_tree_node(D,(MBrR1,_),(MBrR2,_)) <=>
			maxdist_mbr(MBrR1,MBrR2,Dist),
		        gt(Upper,Dist)   
			|    
			upper_bound(D,Upper). 
				
                     
			   
maxdist_tree_node3 @ upper_bound(D,Upper), 
		maxdist_tree_node(D,objects(Z1,_),objects(Z2,_)) <=> 
			maxdist_pairs(Z1,Z2,Upper,Z),
			geq(Z,Upper)
			|
                        upper_bound(D,Z).
			    
			   

maxdist_tree_node4 @ upper_bound(D,Upper), 
			maxdist_tree_node(D,objects(Z1,_),objects(Z2,_)) <=> 
			maxdist_pairs(Z1,Z2,Upper,Z),		
			gt(Upper,Z)
			|  
			upper_bound(D,Upper).                     
			   

maxdist_tree_node5 @ maxdist_tree_node(D,objects(Z1,N),(_,Child2)) <=> 
               		root(Child2,RChild2) 
			|
               		maxdist_tree_node(D,objects(Z1,N),RChild2).

maxdist_tree_node6 @ maxdist_tree_node(D,(_,Child1),objects(Z2,N)) <=> 
               		root(Child1,RChild1) 
			|
               		maxdist_tree_node(D,RChild1,objects(Z2,N)).

maxdist_tree_node7 @ maxdist_tree_node(D,objects(Z1,N),[Node|RNodes]) <=> 
               		maxdist_tree_node(D,objects(Z1,N),Node),
               		maxdist_tree_node(D,objects(Z1,N),RNodes).

maxdist_tree_node8 @ maxdist_tree_node(_,objects(_,_),[]) <=> 
                		true.


maxdist_tree_node9 @ maxdist_tree_node(D,[Node|RNodes],objects(Z2,N)) <=> 
               		maxdist_tree_node(D,Node,objects(Z2,N)),
               		maxdist_tree_node(D,RNodes,objects(Z2,N)).

maxdist_tree_node10 @ maxdist_tree_node(_,[],objects(_,_)) <=> 
				true.
               



maxdist_tree_node11 @ maxdist_tree_node(D,[(MBr1,Child1)|RNodes1],R2) <=> 
			maxdist_tree_node(D,(MBr1,Child1),R2),
			maxdist_tree_node(D,RNodes1,R2).
			    

maxdist_tree_node12 @ upper_bound(D,Upper), maxdist_tree_node(D,[],_) <=> 
			upper_bound(D,Upper).


maxdist_tree_node13 @ maxdist_tree_node(D,(MBr1,Child1),[(MBr,Child)|RNodes]) <=> 
			mindist_mbr(MBr1,MBr,Upper) 
			|
	                upper_bound(D,Upper), 
                        maxdist_tree_node(D,(MBr1,Child1),(MBr,Child)),
                      	maxdist_tree_node(D,(MBr1,Child1),RNodes).
                      

maxdist_tree_node14 @ upper_bound(D,Upper), 
			maxdist_tree_node(D,(_,_),[]) <=> 
			upper_bound(D,Upper).
 



 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% maxdist OBJ (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maxdist_obj1 @ maxdist_obj((X,Y),(Z,T),D) <=>
			A is (X-Z)*(X-Z), 
			B is (Y-T)*(Y-T), 
			D is sqrt(A+B).

maxdist_obj2 @ maxdist_obj((X,Y),line(L),D)<=> 
			maximum_maxdist_point_line((X,Y),L,D).

maxdist_obj3 @ maxdist_obj(line(L),(X,Y),D)<=> 
			maxdist_obj((X,Y),line(L),D).

maxdist_obj4 @ maxdist_obj(line(L1),line(L2),D) <=> 
			maxdist_lines(L1,line(L2),D).

maxdist_obj5 @ maxdist_obj((X,Y),polygon([P|L]),D) <=> 
			append([P|L],[P],L2),
			maximum_maxdist_point_line((X,Y),L2,D).

maxdist_obj6 @ maxdist_obj(polygon(L),(X,Y),D) <=> 
			maxdist_obj((X,Y),
			polygon(L),D).

maxdist_obj7 @ maxdist_obj(line(L1),polygon([P|L2]),D) <=> 
			append([P|L2],[P],L3), 
			maxdist_lines(L1,line(L3),D).

maxdist_obj8 @ maxdist_obj(polygon(L1),line(L2),D) <=> 
			maxdist_obj(line(L2),polygon(L1),D).

maxdist_obj9 @ maxdist_obj(polygon([P1|L1]),polygon([P2|L2]),D) <=> 
			append([P1|L1],[P1],L3), 				
			append([P2|L2],[P2],L4),
			maxdist_lines(L3,line(L4),D).

maxdist_obj10 @ maxdist_obj((X,Y),region(L),D) <=> 
			inside_obj((X,Y),region(L)) 
			| 
			D=0.

maxdist_obj11 @ maxdist_obj((X,Y),region(L),D) <=> 
			maxdist_obj((X,Y),polygon(L),D).

maxdist_obj12 @ maxdist_obj(region(L),(X,Y),D) <=> 
			maxdist_obj((X,Y),region(L),D).

maxdist_obj13 @ maxdist_obj(line(L1),region(L2),D) <=> 
			maximum_maxdist_outside(L1,region(L2),D).

maxdist_obj14 @ maxdist_obj(region(L),line(L1),D) <=> 
			maxdist_obj(line(L1),region(L),D).
		
maxdist_obj15 @ maxdist_obj(polygon(L1),region(L2),D) <=> 
			maximum_maxdist_outside(L1,region(L2),D).

maxdist_obj16 @ maxdist_obj(region(L),polygon(L1),D) <=> 
			maxdist_obj(polygon(L1),region(L),D).

maxdist_obj17 @ maxdist_obj(region(L1),region(L2),D) <=> 
			maximum_maxdist_outside(L1,region(L2),D).

maxdist_obj18 @ maxdist_obj(diff(O1,O2),Z,D) <=> pmaxdist_obj(diff(O1,O2),Z,_,D).

maxdist_obj19 @ maxdist_obj(Z,diff(O1,O2),D) <=> pmaxdist_obj(Z,diff(O1,O2),_,D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% maxdist LINES (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maxdist_lines @ maxdist_lines([P|RP],line(L),D) <=> 
				maximum_maxdist_point_line(P,L,D1),
				maxdist_lines_aux(RP,line(L),D1,D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% maxdist LINES AUX (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maxdist_lines_aux1 @  maxdist_lines_aux([],line(_),DOLD,D) <=> D=DOLD.

maxdist_lines_aux2 @  maxdist_lines_aux([P|RP],line(L),DOLD,D) <=> 
				maximum_maxdist_point_line(P,L,DNEW),
				gt(DNEW,DOLD) 
				|
				maxdist_lines_aux(RP,line(L),DNEW,D).

maxdist_lines_aux3 @  maxdist_lines_aux([P|RP],line(L),DOLD,D) <=> 
				maximum_maxdist_point_line(P,L,DNEW),
				leq(DNEW,DOLD) 
				|
				maxdist_lines_aux(RP,line(L),DOLD,D).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MINIMUM maxdist POINT LINE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maximum_maxdist_point_line1 @  
		maximum_maxdist_point_line((X,Y),[P1,P2|RP],D) <=> 
				maxdist_point_segment((X,Y),P1,P2,D1),
				maximum_maxdist_point_line_aux((X,Y),[P2|RP],D1,D).

maximum_maxdist_point_line2 @ 
		maximum_maxdist_point_line_aux((X,Y),[P1,P2|RP],DOLD,D) <=> 
				maxdist_point_segment((X,Y),P1,P2,DIST),
				gt(DIST,DOLD) 
				|  
				maximum_maxdist_point_line_aux((X,Y),[P2|RP],DIST,D).

maximum_maxdist_point_line3 @ 
		maximum_maxdist_point_line_aux((X,Y),[P1,P2|RP],DOLD,D) <=> 
				maxdist_point_segment((X,Y),P1,P2,DIST),
				leq(DIST,DOLD) 
				|  
				maximum_maxdist_point_line_aux((X,Y),[P2|RP],DOLD,D).

maximum_maxdist_point_line4 @ 
		maximum_maxdist_point_line_aux((_,_),_,DOLD,D) <=>  				D=DOLD.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% maxdist POINT SEGMENT (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


maxdist_point_segment @ maxdist_point_segment((X,Y),(U,V),(Z,T),D) <=>  
				CAT1 is sqrt((U-X)*(U-X)+(V-Y)*(V-Y)),
				CAT2 is  sqrt((Z-X)*(Z-X)+(T-Y)*(T-Y)), 
				(gt(CAT1,CAT2) -> D=CAT1; D=CAT2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% maxdist PAIRS (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maxdist_pairs1 @ maxdist_pairs([],_,MAX,U) <=> U=MAX.

maxdist_pairs2 @ maxdist_pairs([X|L],L2,INIT,MAX) <=>
				maxdist_pairs_one(X,L2,INIT,MAX1),
				maxdist_pairs(L,L2,MAX1,MAX).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% maxdist PAIRS ONE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maxdist_pairs_one1 @ maxdist_pairs_one(_,[],MAX,K) <=> K=MAX.

maxdist_pairs_one2 @ maxdist_pairs_one(X,[Z|L2],INIT,MAX) <=> 
				(maxdist_obj(X,Z,K),
				geq(K,INIT) ->
				maxdist_pairs_one(X,L2,K,MAX);
				maxdist_pairs_one(X,L2,INIT,MAX)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAXIMUM MAXDIST OUTSIDE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	


maximum_maxdist_outside1 @ maximum_maxdist_outside([],region(_),D) <=> D=0.

maximum_maxdist_outside2 @ maximum_maxdist_outside([P|RP],region(L),D) <=> 
			 	inside_point_obj(P,region(L)) 
				|
				maximum_maxdist_outside(RP,region(L),D).

maximum_maxdist_outside3 @ maximum_maxdist_outside([P|RP],region(L),D) <=>
				maximum_maxdist_point_line(P,L,D1),
				maxdist_outside_aux(RP,region(L),D1,D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAXIMUM MAXDIST OUTSIDE AUX (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		                


maxdist_outside_aux1 @  maxdist_outside_aux([],region(_),DOLD,D) <=> D=DOLD.

maxdist_outside_aux2 @  maxdist_outside_aux([P|RP],region(L),DOLD,D) <=> 
				inside_point_obj(P,region(L)) 
				|
				maxdist_outside_aux(RP,region(L),DOLD,D).

maxdist_outside_aux3 @  maxdist_outside_aux([P|RP],region(L),DOLD,D) <=>  
				maximum_maxdist_point_line(P,L,DNEW),
				gt(DNEW,DOLD) 
				|
				maxdist_outside_aux(RP,region(L),DNEW,D).

maxdist_outside_aux4 @  maxdist_outside_aux([P|RP],region(L),DOLD,D) <=> 
				maximum_maxdist_point_line(P,L,DNEW),
				leq(DNEW,DOLD) 
				|
				maxdist_outside_aux(RP,region(L),DOLD,D).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PMAXDIST (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pmaxdist @ pmaxdist(A,B,P) <=> ground(A),ground(B) | pmaxdist(A,B,P,_).

pmaxdist @ pmaxdist(A,B,P,D) <=> pmaxdist_obj(A,B,P,D). 

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PMAXDIST SET (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pmaxdist_set1 @ pmaxdist_set([],_,L2) <=> L2=[].

pmaxdist_set2 @ pmaxdist_set(_,[],L2) <=> L2=[].

pmaxdist_set3 @ pmaxdist_set([X1|L1],[X2|L2],PS) <=> 
			ground([X1|L1]),ground([X2|L2]),
			rtree([X1|L1],5,R1),
			rtree([X2|L2],5,R2)
			|
			pmaxdist_set(R1,R2,PS).

pmaxdist_set4 @ pmaxdist_set(A,B,PS) <=> 
			root(A,RA), 
			root(B,RB) 
			|
			pmaxdist_tree(PS,RA,RB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PMAXDIST TREE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pmaxdist_tree1 @ pmaxdist_tree(PS,[(MBr1,Child1)|RNodes1],R2) <=> 
			pmaxdist_tree_node(PS,(MBr1,Child1),R2),
			pmaxdist_tree(PS,RNodes1,R2).

pmaxdist_tree2 @ dist_bound_max(PS,PR,Upper), pmaxdist_tree(PS,[],_) <=>  
			PS=PR,
               		dist_bound_max(PS,PR,Upper).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PMAXDIST TREE NODE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pmaxdist_tree_node1 @ dist_bound_max(PS,PR,Upper), 
			pmaxdist_tree_node(PS,(MBrR1,Child1),(MBrR2,Child2)) <=>	
			maxdist_mbr(MBrR1,MBrR2,Dist),
                 	geq(Dist,Upper),	   
                  	root(Child1,RChild1),
			root(Child2,RChild2) 
			|      
                  	dist_bound_max(PS,PR,Upper),  
                        pmaxdist_tree_node(PS,RChild1,RChild2).

pmaxdist_tree_node2 @ dist_bound_max(PS,PR,Upper), 
			pmaxdist_tree_node(PS,(MBrR1,_),(MBrR2,_)) <=>	 
			maxdist_mbr(MBrR1,MBrR2,Dist),
		      	gt(Upper,Dist)   
			|    
			dist_bound_max(PS,PR,Upper). 
            

pmaxdist_tree_node3 @ dist_bound_max(PS,PR,Upper), 
			pmaxdist_tree_node(PS,objects(Z1,_),objects(Z2,_)) <=> 
			pmaxdist_pairs(Z1,Z2,[],Upper,Z,ZP), 
			eqn(Z,Upper), 
			ground(PR) 
			| 			    
                      	dist_bound_max(PS,ZP,Z),
                      	dist_bound_max(PS,PR,Upper).	

pmaxdist_tree_node4 @ dist_bound_max(PS,PR,Upper), 
			pmaxdist_tree_node(PS,objects(Z1,_),objects(Z2,_)) <=> 
			pmaxdist_pairs(Z1,Z2,[],Upper,Z,ZP), 
			eqn(Z,Upper), 
			var(PR)
			| 
			dist_bound_max(PS,ZP,Z).
                      		    
pmaxdist_tree_node5 @ dist_bound_max(PS,_,Upper), 
			pmaxdist_tree_node(PS,objects(Z1,_),objects(Z2,_)) <=> 
			pmaxdist_pairs(Z1,Z2,[],Upper,Z,ZP), 
			gt(Z,Upper)			  
			| 
			dist_bound_max(PS,ZP,Z).
                      			    			   

pmaxdist_tree_node6 @ dist_bound_max(PS,PR,Upper), 
			pmaxdist_tree_node(PS,objects(Z1,_),objects(Z2,_)) <=> 
			pmaxdist_pairs(Z1,Z2,[],Upper,Z,_),
			gt(Upper,Z)
		    	|  
			dist_bound_max(PS,PR,Upper).

                            
pmaxdist_tree_node7 @ pmaxdist_tree_node(PS,objects(Z1,N),(_,Child2)) <=> 
               		root(Child2,RChild2) 
			| 
			pmaxdist_tree_node(PS,objects(Z1,N),RChild2).

pmaxdist_tree_node8 @ pmaxdist_tree_node(PS,(_,Child1),objects(Z2,N)) <=> 
               		root(Child1,RChild1) 
			| 
			pmaxdist_tree_node(PS,RChild1,objects(Z2,N)).



pmaxdist_tree_node9 @ pmaxdist_tree_node(PS,[(MBr1,Child1)|RNodes1],R2) <=> 
			pmaxdist_tree_node(PS,(MBr1,Child1),R2), 
			pmaxdist_tree_node(PS,RNodes1,R2).

pmaxdist_tree_node10 @ dist_bound_max(PS,PR,Upper), 
			pmaxdist_tree_node(PS,[],_) <=> 
			dist_bound_max(PS,PR,Upper).

pmaxdist_tree_node11 @ pmaxdist_tree_node(PS,(MBr1,Child1),[(MBr,Child)|RNodes]) <=> 
                  	mindist_mbr(MBr1,MBr,Upper) 
			|      
                  	dist_bound_max(PS,[],Upper), 
			pmaxdist_tree_node(PS,(MBr1,Child1),(MBr,Child)),            
			pmaxdist_tree_node(PS,(MBr1,Child1),RNodes).
                  

pmaxdist_tree_node12 @ dist_bound_max(PS,PR,Upper), 
			pmaxdist_tree_node(PS,(_,_),[]) <=> 
			dist_bound_max(PS,PR,Upper).

pmaxdist_tree_node13 @ pmaxdist_tree_node(PS,objects(Z,N),[(MBr,Child)|RNodes]) <=> 
			pmaxdist_tree_node(PS,objects(Z,N),(MBr,Child)),
			pmaxdist_tree_node(PS,objects(Z,N),RNodes).

pmaxdist_tree_node14 @ dist_bound_max(PS,PR,Upper), 
			pmaxdist_tree_node(PS,objects(_,_),[]) <=> 
			dist_bound_max(PS,PR,Upper).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pmaxdist PAIRS (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pmaxdist_pairs1 @ pmaxdist_pairs([],_,K,MAX,V,U) <=> U=K,V=MAX.

pmaxdist_pairs2 @ pmaxdist_pairs([X|L],L2,K,INIT,MAX,KRES) <=> 
			pmaxdist_pairs_one(X,L2,K,INIT,MAX1,KRES1),
			pmaxdist_pairs(L,L2,KRES1,MAX1,MAX,KRES).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pmaxdist PAIRS ONE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pmaxdist_pairs_one1 @ pmaxdist_pairs_one(_,[],K,MAX,V,U) <=> U=K,V=MAX.

pmaxdist_pairs_one2 @ pmaxdist_pairs_one(X,[Z|L2],LP,INIT,MAX,KRES) <=>
				(pmaxdist_obj(X,Z,POINTS,K),
				eqn(K,INIT) ->
				append(POINTS,LP,NEWPOINTS),
				pmaxdist_pairs_one(X,L2,NEWPOINTS,K,MAX,KRES);
				(pmaxdist_obj(X,Z,POINTS,K),
				gt(K,INIT) ->
				pmaxdist_pairs_one(X,L2,POINTS,K,MAX,KRES);
				pmaxdist_pairs_one(X,L2,LP,INIT,MAX,KRES))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pmaxdist OBJ (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pmaxdist_obj1 @ pmaxdist_obj((X,Y),(Z,T),POINTS,D) <=>
				A is (X-Z)*(X-Z), 
				B is (Y-T)*(Y-T), 
				D is sqrt(A+B),
				POINTS=[((X,Y),(Z,T))].

pmaxdist_obj2 @ pmaxdist_obj((X,Y),line(L),POINTS,D)<=> 
				maximum_pmaxdist_point_line((X,Y),L,POINTS,D).

pmaxdist_obj3 @ pmaxdist_obj((X,Y),polygon([P|L]),POINTS,D)<=>
				append([P|L],[P],L2), 
				maximum_pmaxdist_point_line((X,Y),L2,POINTS,D).

pmaxdist_obj4 @ pmaxdist_obj(line(L),(X,Y),POINTS,D)<=> 
				pmaxdist_obj((X,Y),line(L),POINTS,D).

pmaxdist_obj5 @ pmaxdist_obj(polygon(L),(X,Y),POINTS,D)<=> 
				pmaxdist_obj((X,Y),polygon(L),POINTS,D).

pmaxdist_obj6 @ pmaxdist_obj(line(L1),line(L2),POINTS,D) <=> 
				pmaxdist_lines(L1,line(L2),POINTS,D).

pmaxdist_obj7 @ pmaxdist_obj(line(L1),polygon([P|L2]),POINTS,D) <=>
				append([P|L2],[P],L3), 
				pmaxdist_lines(L1,line(L3),POINTS,D).

pmaxdist_obj8 @ pmaxdist_obj(polygon(L),line(L2),POINTS,D)<=> 
				pmaxdist_obj(line(L2),polygon(L),POINTS,D).

pmaxdist_obj9 @ pmaxdist_obj(polygon([P1|L1]),polygon([P2|L2]),POINTS,D) <=>
				append([P1|L1],[P1],L3), 
				append([P2|L2],[P2],L4),
				pmaxdist_lines(L3,line(L4),POINTS,D).



pmaxdist_obj10 @ pmaxdist_obj((X,Y),region(L),POINTS,D) <=> 
				inside_obj((X,Y),region(L)) 
				| 
				POINTS=[((X,Y),region(L))],D=0.

pmaxdist_obj11 @ pmaxdist_obj((X,Y),region(L),POINTS,D) <=> 
				pmaxdist_obj((X,Y),polygon(L),POINTS,D).

pmaxdist_obj12 @ pmaxdist_obj(region(L),(X,Y),POINTS,D) <=> 
				pmaxdist_obj((X,Y),region(L),POINTS,D).

pmaxdist_obj13 @ pmaxdist_obj(line(L1),region(L2),POINTS,D) <=> 
				maximum_pmaxdist_outside(L1,region(L2),POINTS,D).

pmaxdist_obj14 @ pmaxdist_obj(region(L),line(L1),POINTS,D) <=> 
				pmaxdist_obj(line(L1),region(L),POINTS,D).
		
pmaxdist_obj15 @ pmaxdist_obj(polygon([P|L1]),region(L2),POINTS,D) <=> 
				append([P|L1],[P],L3),
				maximum_pmaxdist_outside(L3,region(L2),POINTS,D).

pmaxdist_obj16 @ pmaxdist_obj(region(L),polygon(L1),POINTS,D) <=> 
				pmaxdist_obj(polygon(L1),region(L),POINTS,D).

pmaxdist_obj17 @ pmaxdist_obj(region([P|L1]),region(L2),POINTS,D) <=> 
				append([P|L1],[P],L3),
				maximum_pmaxdist_outside(L3,region(L2),POINTS,D).

pmaxdist_obj21 @ pmaxdist_obj(diff(O1,O2),Z,POINTS,D) <=> pmaxdist_obj(O1,Z,POINTS1,D),filter_bottom1(POINTS1,O2,POINTS),POINTS\==[] | true.

pmaxdist_obj22 @ pmaxdist_obj(Z,diff(O1,O2),POINTS,D) <=> pmaxdist_obj(Z,O1,POINTS1,D),filter_bottom2(POINTS1,O2,POINTS),POINTS\==[] | true.

pmaxdist_obj23 @ pmaxdist_obj(diff(_,_),_,POINTS,D) <=> 
  POINTS=[],D is -100000000000000.

pmaxdist_obj23 @ pmaxdist_obj(_,diff(_,_),POINTS,D) <=> 
  POINTS=[],D is -100000000000000.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pmaxdist LINES (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pmaxdist_lines1 @ pmaxdist_lines([P|RP],line(L),POINTS,D) <=> 
			maximum_pmaxdist_point_line(P,L,POINTS1,D1),
			pmaxdist_lines_aux(RP,line(L),POINTS1,POINTS,D1,D).


pmaxdist_lines2 @ pmaxdist_lines_aux([],line(_),POINTSOLD,POINTS,DOLD,D) <=> 
			D=DOLD,POINTS=POINTSOLD.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pmaxdist LINES AUX (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pmaxdist_lines_aux1 @ pmaxdist_lines_aux([P|RP],line(L),_,POINTS,DOLD,D) <=> 
			maximum_pmaxdist_point_line(P,L,POINTSNEW,DNEW),
			gt(DNEW,DOLD) 
			|
			pmaxdist_lines_aux(RP,line(L),POINTSNEW,POINTS,DNEW,D).

pmaxdist_lines_aux2 @  pmaxdist_lines_aux([P|RP],line(L),POINTSOLD,POINTS,DOLD,D) <=> 
			maximum_pmaxdist_point_line(P,L,_,DNEW),
			lt(DNEW,DOLD) 
			|
			pmaxdist_lines_aux(RP,line(L),POINTSOLD,POINTS,DOLD,D).

pmaxdist_lines_aux3 @  pmaxdist_lines_aux([P|RP],line(L),POINTSOLD,POINTS,DOLD,D) <=> 
			maximum_pmaxdist_point_line(P,L,POINTSNEW,DNEW),
			eqn(DNEW,DOLD) 
			|
			append(POINTSOLD,POINTSNEW,POINTSTO),
			pmaxdist_lines_aux(RP,line(L),POINTSTO,POINTS,DOLD,D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MINIMUM Pmaxdist POINT LINE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maximum_pmaxdist_point_line @ 
		maximum_pmaxdist_point_line((X,Y),[P1,P2|RP],POINTS,D) <=> 
			pmaxdist_point_segment((X,Y),P1,P2,POINTS1,D1),
			maximum_pmaxdist_point_line_aux((X,Y),[P2|RP],POINTS1,POINTS,D1,D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MINIMUM Pmaxdist POINT LINE AUX (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


maximum_pmaxdist_point_line_aux1 @ 
	maximum_pmaxdist_point_line_aux((X,Y),[P1,P2|RP],_,POINTS,DOLD,D) <=> 
		pmaxdist_point_segment((X,Y),P1,P2,POINTSNEW,DIST),
		gt(DIST,DOLD) 
		|  
		maximum_pmaxdist_point_line_aux((X,Y),[P2|RP],POINTSNEW,POINTS,DIST,D).

maximum_pmaxdist_point_line_aux2 @ 
	maximum_pmaxdist_point_line_aux((X,Y),[P1,P2|RP],POINTSOLD,POINTS,DOLD,D) <=> 
		pmaxdist_point_segment((X,Y),P1,P2,_,DIST),
		lt(DIST,DOLD) 
		|  
		maximum_pmaxdist_point_line_aux((X,Y),[P2|RP],POINTSOLD,POINTS,DOLD,D).

maximum_pmaxdist_point_line_aux3 @ 
	maximum_pmaxdist_point_line_aux((X,Y),[P1,P2|RP],POINTSOLD,POINTS,DOLD,D) <=> 
		pmaxdist_point_segment((X,Y),P1,P2,POINTSNEW,DIST),
		eqn(DIST,DOLD) 
		|  
		append(POINTSOLD,POINTSNEW,POINTSTO),
		maximum_pmaxdist_point_line_aux((X,Y),[P2|RP],POINTSTO,POINTS,DOLD,D).

maximum_pmaxdist_point_line_aux4 @ 
	maximum_pmaxdist_point_line_aux((_,_),_,POINTSOLD,POINTS,DOLD,D) <=>  
		D=DOLD,POINTS=POINTSOLD.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pmaxdist POINT SEGMENT (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pmaxdist_point_segment @ 
		pmaxdist_point_segment((X,Y),(Z,T),(U,V),POINTS,D) <=>
		DIST1 is sqrt(((Z-X)*(Z-X))+((T-Y)*(T-Y))),
		DIST2 is sqrt(((U-X)*(U-X))+((V-Y)*(V-Y))),
		(gt(DIST1,DIST2)->D=DIST1,POINTS=[((X,Y),(Z,T))];
		D=DIST2,POINTS=[((X,Y),(U,V))]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% maximum pmaxdist outside (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maximum_pmaxdist_outside1 @ 
		maximum_pmaxdist_outside([P|RP],region(L),POINTS,D) <=> 
			inside_point_obj(P,region(L)) 
			|
			generate_pairs_maxdist(P,L,POINTS1),  
			D1=0,
			pmaxdist_outside_aux(RP,region(L),POINTS1,POINTS,D1,D).

maximum_pmaxdist_outside2 @ 
		maximum_pmaxdist_outside([P|RP],region(L),POINTS,D) <=> 
			maximum_pmaxdist_point_line(P,L,POINTS1,D1),
			pmaxdist_outside_aux(RP,region(L),POINTS1,POINTS,D1,D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GENERATE PAIRS MAXDIST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


generate_pairs_maxdist(_,[],[]).

generate_pairs_maxdist(P,[X|L],[(P,X)|RP]):-generate_pairs_maxdist(P,L,RP).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PMAXDIST LINES AUX (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pmaxdist_outside_aux1 @ 
		pmaxdist_outside_aux([],region(_),POINTSOLD,POINTS,DOLD,D) <=> 
			D=DOLD,POINTS=POINTSOLD.

pmaxdist_outside_aux2 @ 
		pmaxdist_outside_aux([P|RP],region(L),POINTSOLD,POINTS,DOLD,D) <=> 
			inside_point_obj(P,region(L)),
			lt(0,DOLD)
			| 
			pmaxdist_outside_aux(RP,region(L),POINTSOLD,POINTS,DOLD,D).

pmaxdist_outside_aux3 @ 
		pmaxdist_outside_aux([P|RP],region(L),POINTSOLD,POINTS,DOLD,D) <=> 
			inside_point_obj(P,region(L)),  
			eqn(0,DOLD)
			| 
			generate_pairs_maxdist(P,L,POINTSNEW),
			append(POINTSOLD,POINTSNEW,POINTSTO),
			pmaxdist_outside_aux(RP,region(L),POINTSTO,POINTS,DOLD,D).

pmaxdist_outside_aux4 @ 
		pmaxdist_outside_aux([P|RP],region(L),_,POINTS,DOLD,D) <=> 
			maximum_pmaxdist_point_line(P,L,POINTSNEW,DNEW),
			gt(DNEW,DOLD) 
			|
			pmaxdist_outside_aux(RP,region(L),POINTSNEW,POINTS,DNEW,D).

pmaxdist_outside_aux5 @  
		pmaxdist_outside_aux([P|RP],region(L),POINTSOLD,POINTS,DOLD,D) <=> 
			maximum_pmaxdist_point_line(P,L,_,DNEW),
			lt(DNEW,DOLD) 
			|
			pmaxdist_outside_aux(RP,region(L),POINTSOLD,POINTS,DOLD,D).

pmaxdist_outside_aux6 @  
		pmaxdist_outside_aux([P|RP],region(L),POINTSOLD,POINTS,DOLD,D) <=> 
			maximum_pmaxdist_point_line(P,L,POINTSNEW,DNEW),
			eqn(DNEW,DOLD) 
			|
			append(POINTSOLD,POINTSNEW,POINTSTO),
			pmaxdist_outside_aux(RP,region(L),POINTSTO,POINTS,DOLD,D).


 

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Omaxdist (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

omaxdist1 @ omaxdist(A,B,P) <=> ground(A),ground(B) | omaxdist(A,B,P,_).

omaxdist2 @ omaxdist(A,B,O,D) <=> omaxdist_obj(A,B,O,D). 

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% omaxdist_SET (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


omaxdist_set1 @ omaxdist_set([],_,L2) <=> L2=[].

omaxdist_set2 @ omaxdist_set(_,[],L2) <=> L2=[].

omaxdist_set3 @ omaxdist_set([X1|L1],[X2|L2],PS) <=> 
			ground([X1|L1]),ground([X2|L2]),
			rtree([X1|L1],5,R1),
			rtree([X2|L2],5,R2)
			|
			omaxdist_set(R1,R2,PS).



omaxdist_set4 @ omaxdist_set(A,B,PS) <=> 
			root(A,RA), 
			root(B,RB) 
			|
			omaxdist_tree(PS,RA,RB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% omaxdist_TREE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

omaxdist_tree1 @ omaxdist_tree(PS,[(MBr1,Child1)|RNodes1],R2) <=> 
			omaxdist_tree_node(PS,(MBr1,Child1),R2),
			omaxdist_tree(PS,RNodes1,R2).

omaxdist_tree2 @ dist_bound_max(PS,PR,Upper), omaxdist_tree(PS,[],_) <=>  
			PS=PR,
               		dist_bound_max(PS,PR,Upper).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% omaxdist_TREE_NODE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

omaxdist_tree_node1 @ dist_bound_max(PS,PR,Upper), 
		omaxdist_tree_node(PS,(MBrR1,Child1),(MBrR2,Child2)) <=>	
	    		maxdist_mbr(MBrR1,MBrR2,Dist),
                 	geq(Dist,Upper),	   
                  	root(Child1,RChild1),
			root(Child2,RChild2) 
			|      
                  	dist_bound_max(PS,PR,Upper),  
                        omaxdist_tree_node(PS,RChild1,RChild2).

omaxdist_tree_node2 @ dist_bound_max(PS,PR,Upper), 
		omaxdist_tree_node(PS,(MBrR1,_),(MBrR2,_)) <=>	 
			maxdist_mbr(MBrR1,MBrR2,Dist),
		      	gt(Upper,Dist)   
			|    
			dist_bound_max(PS,PR,Upper). 
            

omaxdist_tree_node3 @ dist_bound_max(PS,PR,Upper), 
		omaxdist_tree_node(PS,objects(Z1,_),objects(Z2,_)) <=> 
			omaxdist_pairs(Z1,Z2,[],Upper,Z,ZP),
			eqn(Z,Upper), 
			ground(PR) 
			| 			    
                      	dist_bound_max(PS,ZP,Z),
                      	dist_bound_max(PS,PR,Upper).	

omaxdist_tree_node4 @ dist_bound_max(PS,PR,Upper), 
		omaxdist_tree_node(PS,objects(Z1,_),objects(Z2,_)) <=> 
			omaxdist_pairs(Z1,Z2,[],Upper,Z,ZP),
			eqn(Z,Upper), 
			var(PR)
			| 
			dist_bound_max(PS,ZP,Z).
                      
omaxdist_tree_node5 @ dist_bound_max(PS,_,Upper), 
			omaxdist_tree_node(PS,objects(Z1,_),objects(Z2,_)) <=> 
			omaxdist_pairs(Z1,Z2,[],Upper,Z,ZP),
			gt(Z,Upper)		  
			| 
			dist_bound_max(PS,ZP,Z).
                      			    			   

omaxdist_tree_node6 @ dist_bound_max(PS,PR,Upper), 
			omaxdist_tree_node(PS,objects(Z1,_),objects(Z2,_)) <=> 
			omaxdist_pairs(Z1,Z2,[],Upper,Z,_),
			gt(Upper,Z)			    	
			|  
			dist_bound_max(PS,PR,Upper).

                            
omaxdist_tree_node7 @ omaxdist_tree_node(PS,objects(Z1,N),(_,Child2)) <=> 
               		root(Child2,RChild2) 
			| 
			omaxdist_tree_node(PS,objects(Z1,N),RChild2).

omaxdist_tree_node8 @ omaxdist_tree_node(PS,(_,Child1),objects(Z2,N)) <=> 
               		root(Child1,RChild1) 
			| 
			omaxdist_tree_node(PS,RChild1,objects(Z2,N)).

omaxdist_tree_node9 @ omaxdist_tree_node(PS,[(MBr1,Child1)|RNodes1],R2) <=> 
			omaxdist_tree_node(PS,(MBr1,Child1),R2), 
			omaxdist_tree_node(PS,RNodes1,R2).

omaxdist_tree_node10 @ dist_bound_max(PS,PR,Upper), 
			omaxdist_tree_node(PS,[],_) <=> 
			dist_bound_max(PS,PR,Upper).

omaxdist_tree_node11 @ omaxdist_tree_node(PS,(MBr1,Child1),[(MBr,Child)|RNodes]) <=> 
                  	mindist_mbr(MBr1,MBr,Upper) 
			|      
                  	dist_bound_max(PS,[],Upper), 
			omaxdist_tree_node(PS,(MBr1,Child1),(MBr,Child)),            
			omaxdist_tree_node(PS,(MBr1,Child1),RNodes).
                  

omaxdist_tree_node12 @ dist_bound_max(PS,PR,Upper), 
			omaxdist_tree_node(PS,(_,_),[]) <=> 
			dist_bound_max(PS,PR,Upper).

omaxdist_tree_node13 @ omaxdist_tree_node(PS,objects(Z,N),[(MBr,Child)|RNodes]) <=> 
			omaxdist_tree_node(PS,objects(Z,N),(MBr,Child)),
			omaxdist_tree_node(PS,objects(Z,N),RNodes).

omaxdist_tree_node14 @ dist_bound_max(PS,PR,Upper), 
			omaxdist_tree_node(PS,objects(_,_),[]) <=> 
			dist_bound_max(PS,PR,Upper).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% omaxdist PAIRS (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


omaxdist_pairs1 @ omaxdist_pairs([],_,K,MAX,V,U) <=> U=K,V=MAX.

omaxdist_pairs2 @ omaxdist_pairs([X|L],L2,K,INIT,MAX,KRES) <=> 
			omaxdist_pairs_one(X,L2,K,INIT,MAX1,KRES1),
			omaxdist_pairs(L,L2,KRES1,MAX1,MAX,KRES).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% omaxdist PAIRS ONE (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


omaxdist_pairs_one1 @ omaxdist_pairs_one(_,[],K,MAX,V,U) <=> U=K,V=MAX.

omaxdist_pairs_one2 @ omaxdist_pairs_one(X,[Z|L2],LP,INIT,MAX,KRES) <=>
			(omaxdist_obj(X,Z,POINTS,K),
			eqn(K,INIT) ->
			append(POINTS,LP,NEWPOINTS),
			omaxdist_pairs_one(X,L2,NEWPOINTS,K,MAX,KRES);
			(omaxdist_obj(X,Z,POINTS,K),
			gt(K,INIT) ->
			omaxdist_pairs_one(X,L2,POINTS,K,MAX,KRES);
			omaxdist_pairs_one(X,L2,LP,INIT,MAX,KRES))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% omaxdist OBJ (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

omaxdist_obj1 @ omaxdist_obj((X,Y),(Z,T),OBJECTS,D) <=>
			A is (X-Z)*(X-Z), 
			B is (Y-T)*(Y-T), 
			D is sqrt(A+B),
			OBJECTS=[((X,Y),(Z,T))].

omaxdist_obj2 @ omaxdist_obj((X,Y),line(L),OBJECTS,D)<=> 
			maximum_omaxdist_point_line((X,Y),L,D),
			OBJECTS=[((X,Y),line(L))].

omaxdist_obj3 @ omaxdist_obj((X,Y),polygon([P|L]),OBJECTS,D)<=>
			append([P|L],[P],L2), 
			maximum_omaxdist_point_line((X,Y),L2,D),
			OBJECTS=[((X,Y),polygon([P|L]))].

omaxdist_obj4 @ omaxdist_obj(line(L),(X,Y),OBJECTS,D)<=> 
			omaxdist_obj((X,Y),line(L),OBJECTS,D).

omaxdist_obj5 @ omaxdist_obj(polygon(L),(X,Y),OBJECTS,D)<=> 
			omaxdist_obj((X,Y),polygon(L),OBJECTS,D).

omaxdist_obj6 @ omaxdist_obj(line(L1),line(L2),OBJECTS,D) <=> 
			omaxdist_lines(L1,line(L2),D),
			OBJECTS=[(line(L1),line(L2))].

omaxdist_obj7 @ omaxdist_obj(line(L1),polygon([P|L2]),OBJECTS,D) <=>
			append([P|L2],[P],L3), 
			omaxdist_lines(L1,line(L3),D),
			OBJECTS=[(line(L1),polygon([P|L2]))].

omaxdist_obj8 @ omaxdist_obj(polygon(L),line(L2),OBJECTS,D)<=> 
			omaxdist_obj(line(L2),polygon(L),OBJECTS,D).

omaxdist_obj9 @ omaxdist_obj(polygon([P1|L1]),polygon([P2|L2]),OBJECTS,D) <=>
			append([P1|L1],[P1],L3), 
			append([P2|L2],[P2],L4), 
			omaxdist_lines(L3,line(L4),D),
			OBJECTS=[(polygon([P1|L1]),polygon([P2|L2]))].

omaxdist_obj10 @ omaxdist_obj((X,Y),region(L),OBJECTS,D) <=>
			maxdist_obj((X,Y),region(L),D),
			OBJECTS=[((X,Y),region(L))].

omaxdist_obj11 @ omaxdist_obj(region(L),(X,Y),OBJECTS,D) <=> 
			omaxdist_obj((X,Y),region(L),OBJECTS,D).


omaxdist_obj12 @ omaxdist_obj(line(L1),region(L2),OBJECTS,D) <=> 
			pmaxdist_obj(line(L1),region(L2),_,D),
			OBJECTS=[(line(L1),region(L2))].

omaxdist_obj13 @ omaxdist_obj(region(L),line(L1),OBJECTS,D) <=> 
			omaxdist_obj(line(L1),region(L),OBJECTS,D).

omaxdist_obj14 @ omaxdist_obj(polygon(L1),region(L2),OBJECTS,D) <=> 
			pmaxdist_obj(polygon(L1),region(L2),_,D),
			OBJECTS=[(polygon(L1),region(L2))].

omaxdist_obj15 @ omaxdist_obj(region(L),polygon(L1),OBJECTS,D) <=> 
			omaxdist_obj(polygon(L1),region(L),OBJECTS,D).

omaxdist_obj16 @ omaxdist_obj(region(L1),region(L2),OBJECTS,D) <=> 
			pmaxdist_obj(region(L1),region(L2),_,D),
			OBJECTS=[(region(L1),region(L2))].

omaxdist_obj21 @ omaxdist_obj(diff(O1,O2),Z,OBJECTS,D) <=> omaxdist_obj(O1,Z,OBJECTS1,D),filter_bottom1(OBJECTS1,O2,OBJECTS),OBJECTS\=[] | true.

omaxdist_obj22 @ omaxdist_obj(Z,diff(O1,O2),OBJECTS,D) <=> omaxdist_obj(Z,O1,OBJECTS1,D),filter_bottom2(OBJECTS1,O2,OBJECTS),OBJECTS\=[] | true.

omaxdist_obj23 @ omaxdist_obj(diff(_,_),_,OBJECTS,D) <=> OBJECTS=[],D is -100000000000000.

omaxdist_obj23 @ omaxdist_obj(_,diff(_,_),OBJECTS,D) <=> OBJECTS=[],D is -100000000000000.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% omaxdist LINES (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

omaxdist_lines1 @ omaxdist_lines([P|RP],line(L),D) <=> 
			maximum_omaxdist_point_line(P,L,D1),
			omaxdist_lines_aux(RP,line(L),D1,D).


omaxdist_lines2 @ omaxdist_lines_aux([],line(_),DOLD,D) <=> 
			D=DOLD.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% omaxdist LINES AUX (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


omaxdist_lines_aux1 @ omaxdist_lines_aux([P|RP],line(L),DOLD,D) <=> 
			maximum_omaxdist_point_line(P,L,DNEW),
			gt(DNEW,DOLD) 
			|
			omaxdist_lines_aux(RP,line(L),DNEW,D).

omaxdist_lines_aux2 @  omaxdist_lines_aux([P|RP],line(L),DOLD,D) <=> 
			maximum_omaxdist_point_line(P,L,DNEW),
			lt(DNEW,DOLD) 
			|
			omaxdist_lines_aux(RP,line(L),DOLD,D).

omaxdist_lines_aux3 @  omaxdist_lines_aux([P|RP],line(L),DOLD,D) <=> 
			maximum_omaxdist_point_line(P,L,DNEW),
			eqn(DNEW,DOLD) 
			|
			omaxdist_lines_aux(RP,line(L),DOLD,D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MINIMUM omaxdist POINT LINE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maximum_omaxdist_point_line @ 
	maximum_omaxdist_point_line((X,Y),[P1,P2|RP],D) <=> 
		omaxdist_point_segment((X,Y),P1,P2,D1),
		maximum_omaxdist_point_line_aux((X,Y),[P2|RP],D1,D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MINIMUM omaxdist POINT LINE AUX (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


maximum_omaxdist_point_line_aux1 @ 
	maximum_omaxdist_point_line_aux((X,Y),[P1,P2|RP],DOLD,D) <=> 
		omaxdist_point_segment((X,Y),P1,P2,DIST),
		gt(DIST,DOLD) 
		|  
		maximum_omaxdist_point_line_aux((X,Y),[P2|RP],DIST,D).

maximum_omaxdist_point_line_aux2 @ 
	maximum_omaxdist_point_line_aux((X,Y),[P1,P2|RP],DOLD,D) <=> 
		omaxdist_point_segment((X,Y),P1,P2,DIST),
		lt(DIST,DOLD) 
		|  
		maximum_omaxdist_point_line_aux((X,Y),[P2|RP],DOLD,D).

maximum_omaxdist_point_line_aux3 @ 
	maximum_omaxdist_point_line_aux((X,Y),[P1,P2|RP],DOLD,D) <=> 
		omaxdist_point_segment((X,Y),P1,P2,DIST),
		eqn(DIST,DOLD) 
		|  
 		maximum_omaxdist_point_line_aux((X,Y),[P2|RP],DOLD,D).

maximum_omaxdist_point_line_aux4 @ 
	maximum_omaxdist_point_line_aux((_,_),_,DOLD,D) <=>  
		D=DOLD.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% omaxdist POINT SEGMENT (METRIC)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

omaxdist_point_segment @ 
		omaxdist_point_segment((X,Y),(Z,T),(U,V),D) <=>
			DIST1 is sqrt(((Z-X)*(Z-X))+((T-Y)*(T-Y))),
			DIST2 is sqrt(((U-X)*(U-X))+((V-Y)*(V-Y))),
			(gt(DIST1,DIST2)->D=DIST1;D=DIST2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STRUCTURAL OPERATIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- chr_constraint length_line/2, perimeter/2, area/2, length_set/2, perimeter_set/2, area_set/2,
  		setpoints/2, setsegments/2, setpoints_set/2, setsegments_set/2, 			
length_objects/2,perimeter_objects/2,area_objects/2,
			setpoints_objects/2,setsegments_objects/2.
:- chr_constraint length_segments/2,length_segment/3,setsegments_line/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LENGTH_LINE (STRUCTURAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

length_line @ length_line(line(L),LENGTH) <=> ground(L) |
			length_segments(L,LENGTH).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PERIMETER (STRUCTURAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%

perimeter1 @ perimeter(polygon([Q|L]),P) <=> ground([Q|L])|
			append([Q|L],[Q],L2),
			length_segments(L2,P).

perimeter2 @ perimeter(region([Q|L]),P) <=> ground([Q|L])|
			append([Q|L],[Q],L2),
			length_segments(L2,P).

 
%%%%%%%%%%%%%%%%%%%%%%%%%%
% AREA (STRUCTURAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SETPOINTS (STRUCTURAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


setpoints1 @  setpoints((X,Y),SET) <=> SET=[(X,Y)].

setpoints2 @  setpoints(line(L),SET) <=> SET=L.

setpoints3 @  setpoints(polygon(L),SET) <=> SET=L.

setpoints4 @  setpoints(region(L),SET) <=> SET=L.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SETSEGMENTS (STRUCTURAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setsegments1 @  setsegments(line(L),SET) <=> 
				setsegments_line(L,SET).

setsegments2 @  setsegments(polygon([P|L]),SET) <=> 
				append([P|L],[P],L2),
				setsegments_line(L2,SET).

setsegments3 @  setsegments(region([P|L]),SET) <=> 
				append([P|L],[P],L2),
				setsegments_line(L2,SET).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LENGTH SET (STRUCTURAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 
length_set @ length_set(S,L) <=> ground(S) | length_objects(S,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LENGTH OBJECTS (STRUCTURAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


length_objects1 @ length_objects([],L) <=> L=0.

length_objects2 @ length_objects([line(L)|RO],M) <=> 
				length_line(line(L),N), 
				length_objects(RO,K),
				M is N+K.

length_objects3 @ length_objects([_|RO],N) <=> 
				length_objects(RO,N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PERIMETER SET (STRUCTURAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 perimeter_set @ perimeter_set(S,L) <=> ground(S) | perimeter_objects(S,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PERIMETER OBJECTS (STRUCTURAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

perimeter_objects1 @ perimeter_objects([],L) <=> L=0.

perimeter_objects2 @ perimeter_objects([polygon(L)|RO],M) <=> 
					perimeter(polygon(L),N), 
					perimeter_objects(RO,K),
					M is N+K.

perimeter_objects3 @ perimeter_objects([region(L)|RO],M) <=> 
					perimeter(region(L),N), 
					perimeter_objects(RO,K),
					M is N+K.

perimeter_objects4 @ perimeter_objects([_|RO],N) <=> 
					perimeter_objects(RO,N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% AREA SET (STRUCTURAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

area_set @ area_set(S,L) <=> ground(S) | area_objects(S,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% AREA OBJECTS (STRUCTURAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

area_objects1 @ area_objects([],L) <=> 
				L=0.

area_objects2 @ area_objects([region(L)|RO],M) <=> 
				area(region(L),N), 
				area_objects(RO,K),
				M is N+K.

area_objects3 @ area_objects([_|RO],N) <=> 
				area_objects(RO,N).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SET POINTS SET (STRUCTURAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 
setpoints_set @ setpoints_set(S,L) <=> setpoints_objects(S,L).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SET POINTS OBJECTS (STRUCTURAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setpoints_objects1 @ setpoints_objects([],L) <=> L=[].

setpoints_objects2 @ setpoints_objects([O|RO],M) <=> 
				setpoints(O,N), 
				setpoints_objects(RO,K),
				append_clean(N,K,M).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SET SEGMENTS SET (STRUCTURAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setsegments_set @ setsegments_set(S,L) <=> setsegments_objects(S,L).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SET SEGMENTS OBJECTS (STRUCTURAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setsegments_objects1 @ setsegments_objects([],L) <=> L=[].

setsegments_objects2 @ setsegments_objects([(_,_)|RO],K) <=> 
						setsegments_objects(RO,K).

setsegments_objects3 @ setsegments_objects([O|RO],M) <=> 
						setsegments(O,N), 
						setsegments_objects(RO,K),
						append_clean(N,K,M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LENGTH SEGMENTS (STRUCTURAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

length_segments1 @ length_segments([P1,P2|RP],LENGTH) <=> 
					length_segment(P1,P2,L1),
					length_segments([P2|RP],LENGTH2),
					LENGTH is LENGTH2+L1.

length_segments2 @  length_segments(_,LENGTH) <=> LENGTH=0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LENGTH SEGMENT (STRUCTURAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

length_segment @  length_segment((X,Y),(Z,T),L) <=> 
				L is sqrt((X-Z)*(X-Z)+(Y-T)*(Y-T)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SET SEGMENTS LINE (STRUCTURAL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


setsegments_line1 @ setsegments_line([P1,P2|RP],SET) <=> 
				setsegments_line([P2|RP],SET2),
				SET=[line([P1,P2])|SET2].

setsegments_line2 @ setsegments_line(_,SET) <=> SET=[].

%---------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LIST OPERATIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

:-chr_constraint head/2,tail/2.
%logic: write_list 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HEAD (LIST)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

head1 @ head([],_) <=> fail.
head2 @ head([X|_],Y) <=> X=Y.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TAIL (LIST)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tail1 @ tail([],_) <=> fail.
tail2 @ tail([_|L],L2) <=> L=L2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% WRITE_LIST (LIST)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_list([]).
write_list([X|L]):-write(X),nl,write_list(L).

 


%-----------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OBJECT OPERATIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- chr_constraint up/2,down/2,mbr/2.
:- chr_constraint cox_point/2,coy_point/2.
:- chr_constraint up_point/2,down_point/2,mbr_point/2.
:- chr_constraint up_line/2,down_line/2,mbr_line/2.
:- chr_constraint union_mbr_points/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UP (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

up @ up((X,Y),Z) <=> up_point((X,Y),Z). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DOWN (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

down @ down((X,Y),Z) <=> down_point((X,Y),Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UP (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

up @ up(line(L),Z) <=> up_line(line(L),Z). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DOWN (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

down @ down(line(L),Z) <=> down_line(line(L),Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UP (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

up @ up(polygon(L),Z) <=> up_line(line(L),Z). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DOWN (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

down @ down(polygon(L),Z) <=> down_line(line(L),Z).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UP (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

up @ up(region(L),Z) <=> up_line(line(L),Z). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DOWN (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

down @ down(region(L),Z) <=> down_line(line(L),Z).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UP (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

up @ up(diff(O1,_),Z) <=> up(O1,Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DOWN (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

down @ down(diff(O1,_),Z) <=> down(O1,Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UP_LINE (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

up_line @ up_line(line(L),Z) <=> mbr(line(L),mbr((_,_),(U,V))) 
				 | 
				 Z=(U,V).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DOWN_LINE (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

down_line @ down_line(line(L),Z) <=> mbr(line(L),mbr((X,Y),(_,_))) 
				     | 
				     Z=(X,Y).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COX_POINT   (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cox_point @ cox_point((A,_),C) <=> C=A.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COY_POINT   (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

coy_point @ coy_point((_,B),C) <=> C=B.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UP_POINT (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

up_point @ up_point((X,Y),Z) <=> Z=(X,Y). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DOWN_POINT (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

down_point @ down_point((X,Y),Z) <=> Z=(X,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MBR (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mbr @ mbr((X,Y),Z) <=> \+compound(X),\+compound(Y) 
			| 
			mbr_point((X,Y),Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MBR_POINT (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mbr_point @ mbr_point((X,Y),Z) <=> Z=mbr((X,Y),(X,Y)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MBR (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mbr @ mbr(line(L),Z) <=> mbr_line(line(L),Z).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MBR (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mbr @ mbr(polygon(L),Z) <=> mbr_line(line(L),Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MBR (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mbr @ mbr(region(L),Z) <=> mbr_line(line(L),Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MBR (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mbr @ mbr(diff(O1,_),Z) <=> mbr(O1,Z).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MBR (PAIR)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mbr((O1,O2),MBr) <=> mbr(O1,MBr1),
		     mbr(O2,MBr2),
		     union_mbr(MBr1,MBr2,MBr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MBR_LINE (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mbr_line @ mbr_line(line(L),Z) <=> union_mbr_points(L,Z).

%%%%%%%%%%%%%%%%%%%%%%%%%
% MBR
%%%%%%%%%%%%%%%%%%%%%%%%

mbr @ mbr(window((X,Y),(Z,T)),MBr) <=> MBr=mbr((X,Y),(Z,T)).

mbr @ mbr(circle((X,Y),D),MBr) <=> MBrCXDown is X-D,
			MBrCYDown is Y-D,
			MBrCXUp is X+D,
			MBrCYUp is Y+D, 
			MBr=mbr((MBrCXDown,MBrCYDown),(MBrCXUp,MBrCYUp)).
			

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UNION MBR POINTS (OBJECT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

union_mbr_points1 @ union_mbr_points([X],M) <=> 
				mbr_point(X,M).
union_mbr_points2 @ union_mbr_points([X,Y|L],M) <=> 
				mbr_point(X,MBrX),
				union_mbr_points([Y|L],MBrR),
				union_mbr(MBrX,MBrR,M).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MBR OPERATIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-chr_constraint up_mbr/2,down_mbr/2,union_mbr/3, mindist_mbr/3,compute_mindist/6, maxdist_mbr/3,compute_maxdist/6, overlap_mbr/2, overlap_aux/2.
:-chr_constraint minimum_number/3,maximum_number/3.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UP_MBR (MBR)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

up_mbr @ up_mbr(mbr((_,_),(Z,T)),A) <=> A=(Z,T).

%%%%%%%%%%%%%%%%%%%%%%%%%%
% DOWN_MBR (MBR)
%%%%%%%%%%%%%%%%%%%%%%%%%%

down_mbr @ down_mbr(mbr((X,Y),(_,_)),A) <=> A=(X,Y).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UNION MBR (MBR)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

union_mbr @ union_mbr(mbr((S1,S2),(T1,T2)),mbr((P1,P2),(Q1,Q2)),MBr) <=> 
				minimum_number(S1,P1,A1),                
				minimum_number(S2,P2,A2),
				maximum_number(T1,Q1,B1),
				maximum_number(T2,Q2,B2),
				MBr=mbr((A1,A2),(B1,B2)).


 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MINIMUM NUMBER (MBR)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
minimum_number(A,B,C) <=> 	(leq(A,B),
				C=A;				
				(lt(B,A),
				C=B)).
			


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAXIMUM NUMBER (MBR)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maximum_number(A,B,C) <=> (leq(A,B),
				C=B;				
			 (lt(B,A),
				C=A)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MINDIST MBR (MBR)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


mindist_mbr @ mindist_mbr(mbr((S1,S2),(T1,T2)),mbr((P1,P2),(Q1,Q2)),MIN) <=>
                         compute_mindist((S1,S2),(T1,T2),(P1,P2),(Q1,Q2),Y1,Y2),
                         Y1S is Y1*Y1, 
			 Y2S is Y2*Y2,
                         SUM is Y1S+Y2S,
                         MIN is sqrt(SUM).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COMPUTE MINDIST (MBR)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compute_mindist @ compute_mindist((S1,S2),(T1,T2),(P1,P2),(Q1,Q2),U,V) <=>
                  (gt(S1,Q1), 
		gt(S2,Q2) ->
		U is S1-Q1,
		V is S2-Q2);
                  (gt(S1,Q1), 
		gt(P2,T2) ->
		U is S1-Q1,
		V is P2-T2); 
                  (gt(P1,T1), 
		gt(S2,Q2) ->
		U is P1-T1,
		V is S2-Q2);
                  (gt(P1,T1), 
		gt(P2,T2) ->
		U is P1-T1,
		V is P2-T2);
                  (gt(S1,Q1), 
		gt(S2,Q2) ->
		U is S1-Q1,
		V is S2-Q2);
                  (leq(S1,Q1), 
		leq(P1,T1), 
		gt(S2,Q2) ->
		U is 0,
		V is S2-Q2);
                  (leq(S1,Q1), 
		leq(P1,T1), 
		gt(P2,T2) ->
		U is 0,
		V is P2-T2);
                  (leq(S2,Q2), 
		leq(P2,T2), 
		gt(S1,Q1) ->
		U is S1-Q1,
		V is 0);
                  (leq(S2,Q2), 
		leq(P2,T2), 
		gt(P1,T1) ->
		U is P1-T1,
		V is 0);
                  (leq(S1,Q1), 
		leq(P1,T1), 
		leq(S2,Q2), 
		leq(P2,T2) ->
		U is 0,
		V is 0).
					
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAXDIST MBR (MBR)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
maxdist_mbr @ maxdist_mbr(mbr((S1,S2),(T1,T2)),mbr((P1,P2),(Q1,Q2)),MIN)<=>
                        compute_maxdist((S1,S2),(T1,T2),(P1,P2),(Q1,Q2),Y1,Y2),
                        Y1S is Y1*Y1, 
     			Y2S is Y2*Y2,
                        SUM is Y1S+Y2S,
                        MIN is sqrt(SUM).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COMPUTE MAXDIST (MBR)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compute_maxdist @ compute_maxdist((S1,S2),(T1,T2),(P1,P2),(Q1,Q2),Y1,Y2) <=>
        		DIFA1 is abs(S1-Q1), 
			DIFB1 is abs(T1-P1),
	  		maximum_number(DIFA1,DIFB1,Y1),
        		DIFA2 is abs(S2-Q2), 
			DIFB2 is abs(T2-P2),
	  		maximum_number(DIFA2,DIFB2,Y2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OVERLAP MBR (MBR)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

overlap_mbr(A,B) <=> overlap_aux(A,B) | true.
overlap_mbr(A,B) <=> overlap_aux(B,A) | true.
overlap_mbr(_,_) <=> fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% overlap_aux (MBR)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

overlap_aux(mbr((X,Y),(Z,T)),mbr((U,V),(W,Q))) <=> 
			neqn(X,Z),neqn(Y,T),neqn(U,W),neqn(V,Q)
			|
			inside_point_mbr((Z,T),mbr((U,V),(W,Q)));
			inside_point_mbr((X,T),mbr((U,V),(W,Q))).
				 
overlap_aux(mbr((X,Y),(Z,T)),mbr((U,V),(W,Q))) <=>
			eqn(X,Z),neqn(U,W),neqn(V,Q) 
			|
			leq(U,X),leq(X,W), 
			(inside_point_mbr((X,Y),mbr((U,V),(W,Q)));
			inside_point_mbr((Z,T),mbr((U,V),(W,Q)));
			leq(Y,V),leq(Q,T)).

overlap_aux(mbr((X,Y),(Z,T)),mbr((U,V),(W,Q))) <=>
			eqn(Y,T),neqn(U,W),neqn(V,Q) 
			|
			leq(V,Y),leq(Y,Q), 
			(inside_point_mbr((X,Y),mbr((U,V),(W,Q)));
			inside_point_mbr((Z,T),mbr((U,V),(W,Q)));
			leq(X,U),leq(W,Z)).

overlap_aux(mbr((X,Y),(Z,_)),mbr((U,V),(W,Q))) <=>
			eqn(X,Z),eqn(U,W),neqn(V,Q) 
			|
			inside_point_mbr((X,Y),mbr((U,V),(W,Q))).

overlap_aux(mbr((X,Y),(_,T)),mbr((U,V),(W,Q))) <=>
			eqn(Y,T),neqn(U,W),eqn(V,Q) 
			|
			inside_point_mbr((X,Y),mbr((U,V),(W,Q))).

overlap_aux(mbr((X,Y),(Z,T)),mbr((U,V),(W,Q))) <=>
			eqn(X,Z),eqn(V,Q) 
			|
			leq(U,X),leq(X,W),leq(Y,V),leq(V,T). 

 
overlap_aux(_,_) <=> fail.


 
%-------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TREE OPERATIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% logic: root,root_nodes,select_nodes,ptree,leaves.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ROOT (TREE) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
root(node(NodeList,_),K):- K=NodeList.

root(objects(Z,L),A):- A=objects(Z,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LEAVES (TREE)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

leaves(objects(Leaves,_),Leaves).
leaves(node(NodeList,_),Leaves):-
		leaves_children(NodeList,Leaves).

leaves_children([],[]).
leaves_children([(_,Child)|RNodes],Leaves):-
			leaves(Child,Leaves_Child),
			leaves_children(RNodes,Leaves_RNodes),
			append(Leaves_Child,Leaves_RNodes,Leaves).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ROOT_NODES (TREE)
%%%%%%%%%%%%%%%%%%%%%%%%%%%

root_nodes(node(NodeList,_),Node):- select_nodes(NodeList,Node).

%%%%%%%%%%%%%%%%%%%%%%%%
% select_nodes (TREE)
%%%%%%%%%%%%%%%%%%%%%%%

select_nodes([X|L],Y):- Y=X; select_nodes(L,Y).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PTREE (TREE)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ptree(node(NodeList,_)):-
				ptree(NodeList).

ptree([(mbr(A,B),node(ChildList,_))|RMbr]):-
				write(mbr(A,B)),nl,
				append(RMbr,ChildList,Tree),
				ptree(Tree).


ptree([(mbr(A,B),objects(L,N))|RMbr]):-
				write(mbr(A,B)),nl,
				append(RMbr,[objects(L,N)],Tree),
				ptree(Tree).

ptree([objects(L,N)|RPoints]):-
				write(objects(L,N)),nl,
				ptree(RPoints).

ptree([]).


  



 
 





