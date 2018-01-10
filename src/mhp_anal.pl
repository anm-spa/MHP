:- module(mhp_anal,[mhpOfGraphs/2,mhpOfFiles/2]).
:- use_module(src/helper).





mhpOfGraphs(G1,G2):-
	absolute_file_name('src/autogen/graph.pl',FAbs1),
	use_module(FAbs1,[graphInfo/2,graphLoc/2]),
	graphInfo(Id1,G1),
	graphInfo(Id2,G2),
	graphLoc(Id1,Loc1),
	graphLoc(Id2,Loc2),
	getMHPResults(Id1,Loc1,M1),
	getMHPResults(Id2,Loc2,M2),
	crossProduct(M1,M2,M3),
	length(M3,L3),
	write(L3),
	abolish(graphInfo/2),
	abolish(graphLoc/2).

mhpOfGraphs(_G1,_G2):-
	write("Graph not found").

mhpOfFiles(F1,F2):-
%	multifile mhp/2,
	absolute_file_name(F1,FAbs1),
	use_module(FAbs1),
	getTextualFileName(FAbs1,F1m),
	findall((P,Q),F1m:mhp(P,Q),M1),
	
	abolish(mhp/2),

	absolute_file_name(F2,FAbs2),
	use_module(FAbs2),
	getTextualFileName(FAbs2,F2m),

	findall((P,Q),F2m:mhp(P,Q),M2),
	abolish(mhp/2),

	crossProduct(M1,M2,M3),
	length(M3,L3),
	write(L3).


crossProduct(M1,M2,M3):-
	findall((T,K),(member((P,Q),M1),member((R,S),M2),(T=P;T=Q),(K=R;K=S)),Mp1), %%efficient? maybe use hash function?
	union(M1,M2,Mp),
	union(Mp,Mp1,Mp2),
	list_to_set(Mp2,Mhp),
	remove_dummyTask(Mhp,M3).


remove_dummyTask([],[]).
remove_dummyTask([(P,Q)|Rs],[(P,Q)|Ds]):-
	\+ P=dummyTask, \+ Q=dummyTask,!,
	remove_dummyTask(Rs,Ds).

remove_dummyTask([_|Rs],Ds):-
	remove_dummyTask(Rs,Ds).


getMHPResults(Id,Loc,M):-
	getTextualFileName(Loc,Gfile),
	use_module(Loc,[node/3]),
	atom_concat('graph_',Gf,Gfile),
	atom_concats(['src/autogen/mhp_',Gf,'.pl'],MHPFile),
	exists_file(MHPFile),!,
	getTextualFileName(MHPFile,Module),
	use_module(MHPFile),
	findall((P,Q),(Module:mhp(P,Q),(node(P,class:exec,Id),node(Q,class:exec,Id))),M),
	abolish(node/3),
	abolish(mhp/2).
	
	
getMHPResults(Id,Loc,M):-
	use_module(src/dataflowAnalysis,[main/1]),
	use_module(Loc,[node/3]),
	main(Loc),                                                  % perform dataflow analysis
	getTextualFileName(Loc,Gfile),
	atom_concat('graph_',Gf,Gfile),
	atom_concats(['src/autogen/mhp_',Gf,'.pl'],MHPFile),
	exists_file(MHPFile),!,
	getTextualFileName(MHPFile,Module),
	use_module(MHPFile),
	findall((P,Q),(Module:mhp(P,Q),(node(P,class:exec,Id),node(Q,class:exec,Id))),M),
	abolish(main/1),
	abolish(node/3),
	abolish(mhp/2).
	