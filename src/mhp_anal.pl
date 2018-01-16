:- module(mhp_anal,[mhpOfGraphs/2,mhpOfFiles/2]).
:- use_module(src/helper).





% It creates a new graph and then perform dataflow analysis to generate mhp pair.

mhpOfGraphs(Gid1,Gid2):-
	mhpOfGraphsAux(Gid1,Gid2,GFile),

	%Perform dataflow analysis
	use_module('src/dataflowAnalysis.pl',[main/1]),
	main(GFile),
	abolish(main/1),!.

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


mhpOfGraphsAux(Gid1,Gid2,GFile):-
	absolute_file_name('src/autogen/graph.pl',FAbs1),
	use_module(FAbs1,[graphLoc/2,graphInfo/2]),
	graphLoc(Gid1,Loc1),
	graphLoc(Gid2,Loc2),
	
	nb_setval(barrier,1),
	% collect events connecting G1 and G2

	collectEvents(Gid1,Gid2,Events1,Events2),

	%get a new name
	graphInfo(Gid1,Name1),
	graphInfo(Gid2,Name2),
	atom_concat('gr_',AName1,Name1),
	atom_concat('gr_',AName2,Name2),
	atom_concats(['gr_',AName1,'_',AName2],GName),
	
	% get a new ID
	findall(ID,graphLoc(ID,_),IDList),
	max_list(IDList,Max),
	Gid is Max+1,

	%create a new graph
	atom_concats(['src/autogen/graph_',AName1,'_',AName2,'.pl'],GFile),
	open(GFile,write,GFileStream),
	atom_concats([':- module(graph_',AName1,'_',AName2,',[node/3,edge/3,graphs/1]',').'],GModule),
	write(GFileStream,GModule),
	nl(GFileStream),

	atom_concats([':- use_module("graphExtended_',AName1,'_',AName2,'").'],GExtendedModule),
	write(GFileStream,GExtendedModule),
	nl(GFileStream),

	write(GFileStream,':- discontiguous node/3.'),
	nl(GFileStream),

	write(GFileStream,':- discontiguous edge/3.'),
	nl(GFileStream),

	% write all nodes and edges of the two previous graph into the new graph
	use_module(Loc1,[node/3,edge/3]),
	findall((N,T),node(N,T,Gid1),Nodes),
	findall((N1,N2),edge(N1,N2,Gid1),Edges),
	getTextualFileName(Loc1,F1),
	getStartNode(Gid1,F1,Start1),
	
	(\+ Loc1=Loc2 -> (abolish(node/3),abolish(edge/3),use_module(Loc2,[node/3,edge/3]));true),
	findall((N,T),node(N,T,Gid2),Nodes2),
	findall((N1,N2),edge(N1,N2,Gid2),Edges2),
	getTextualFileName(Loc2,F2),
	getStartNode(Gid2,F2,Start2),

	forall(member((N,Type),Nodes),(
	    term_to_atom(Type,T),
	    atom_concats(['node(',N,',',T,',',Gid,').'],NodeText),
	    write(GFileStream,NodeText),
	    nl(GFileStream)
	)),
	
	forall(member((N,Type),Nodes2),(
	    term_to_atom(Type,T),
	    atom_concats(['node(',N,',',T,',',Gid,').'],NodeText),
	    write(GFileStream,NodeText),
	    nl(GFileStream)
	)),

	write(GFileStream,'node(A,B,C):- nd(A,B,C),'),
	nl(GFileStream),

	forall(member((N1,N2),Edges),(
	    atom_concats(['edge(',N1,',',N2,',',Gid,').'],EdgeText),
	    write(GFileStream,EdgeText),
	    nl(GFileStream)
	)),

	forall(member((N1,N2),Edges2),(
	    atom_concats(['edge(',N1,',',N2,',',Gid,').'],EdgeText),
	    write(GFileStream,EdgeText),
	    nl(GFileStream)
	)),

	write(GFileStream,'edge(A,B,C):- arc(A,B,C).'),
	nl(GFileStream),

	
	forall(member((T1,_E1,T2),Events1),(
	    createNewBarrierNodeName(Gid,Barrier),
	    atom_concats(['node(',Barrier,',class:barrier,',Gid,').'],NodeText),
	    write(GFileStream,NodeText),
	    nl(GFileStream),

	    atom_concats(['edge(',T1,',',Barrier,',',Gid,').'],EdgeTxt1),
	    write(GFileStream,EdgeTxt1),
	    nl(GFileStream),

	    atom_concats(['edge(',Barrier,',',T2,',',Gid,').'],EdgeTxt2),
	    write(GFileStream,EdgeTxt2),
	    nl(GFileStream)
	)),


	forall(member((T1,_E2,T2),Events2),(
	    createNewBarrierNodeName(Gid,Barrier),
	    atom_concats(['node(',Barrier,',class:barrier,',Gid,').'],NodeText),
	    write(GFileStream,NodeText),
	    nl(GFileStream),

	    atom_concats(['edge(',T1,',',Barrier,',',Gid,').'],EdgeTxt1),
	    write(GFileStream,EdgeTxt1),
	    nl(GFileStream),

	    atom_concats(['edge(',Barrier,',',T2,',',Gid,').'],EdgeTxt2),
	    write(GFileStream,EdgeTxt2),
	    nl(GFileStream)
	)),
	
	atom_concats(['graphs([',Gid,']).'],GIdInfo),
	write(GFileStream,GIdInfo),
	nl(GFileStream),

	abolish(node/3),
	abolish(edge/3),
	close(GFileStream),

	% write to the ExtendedGraph
	atom_concats(['src/autogen/graphExtended_',AName1,'_',AName2,'.pl'],GExtendedFile),
	open(GExtendedFile,write,GExtFileStream),
	atom_concats([':- module(graphExtended_',AName1,'_',AName2,',[nd/3,arc/3]',').'],GEModule),
	write(GExtFileStream,GEModule),
	nl(GExtFileStream),

	atom_concats(['nd(',AName1,'_',AName2,'_start_',Gid,',class:label,',Gid,').'],StartNodeText),
	write(GExtFileStream,StartNodeText),
	nl(GExtFileStream),

	createNewBarrierNodeName(Gid,Barrier),
	atom_concats(['nd(',Barrier,',class:barrier,',Gid,').'],NodeText),
	write(GExtFileStream,NodeText),
	nl(GExtFileStream),

	atom_concats([AName1,'_',AName2,'_start_',Gid],StartNode),
	atom_concats(['arc(',StartNode,',',Barrier,',',Gid,').'],SArc),
	write(GExtFileStream,SArc),
	nl(GExtFileStream),
	
	atom_concats(['arc(',Barrier,',',Start1,',',Gid,').'],EStart1),
	write(GExtFileStream,EStart1),
	nl(GExtFileStream),
	
	atom_concats(['arc(',Barrier,',',Start2,',',Gid,').'],EStart2),
	write(GExtFileStream,EStart2),
	nl(GExtFileStream),

	close(GExtFileStream),

	open('src/autogen/graph.pl',append,GStream),
	atom_concats(['graphInfo(',Gid,',',GName,').'],NewGInfo),
	atom_concats(['graphLoc(',Gid,',"',GFile,'").'],NewGLoc),
	write(GStream,NewGInfo),
	nl(GStream),
	write(GStream,NewGLoc),
	nl(GStream),
	close(GStream),

	abolish(graphInfo/2),
	abolish(graphLoc/2).


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




% sometimes a letter s before done is extra.
is_mutation(E,Ep):-
	atom_concat(X,'sdone',E),
	atom_concat(X,_,Ep).

is_mutation(E,Ep):-
	atom_concat(X,'done',E),
	atom_concat(X,_,Ep).


getMutatedMap([],[]).
getMutatedMap([(T,E)|Ts],[(T,E1)|Set]):-
	events(_,_G,_T,E1),
	is_mutation(E,E1),!,
	getMutatedMap(Ts,Set).

getMutatedMap([(T,E)|Ts],[(T,E)|Set]):-
	getMutatedMap(Ts,Set).

getAllEventMapWithPossibleMutation(EMap):-
	findall((T,E),eventMap(T,E),Map1),
	findall((T,E),(member((T,E),Map1),events(_,_G,_TT,E)),Map2),
	list_to_set(Map1,Set1),
	list_to_set(Map2,Set2),
	subtract(Set1,Set2,Set3),
	getMutatedMap(Set3,Set4),
	union(Set2,Set4,EMap).

collectEvents(Gid1,Gid2,Ev1,Ev2):-
	use_module('src/autogen/events.pl',[events/4]),
	events(F1,Gid1,_T1,_E2),
	events(F2,Gid2,_T2,_E1),

	use_module(F1,[eventMap/2,node/3]),

	getAllEventMapWithPossibleMutation(EMap),
	
	findall((Task,Event),events(_,Gid2,Task,Event),Events2),
	list_to_set(Events2,Evset2),

	findall((T,E,T1),(member((T,E),Evset2),member((T1,E),EMap),node(T1,_,Gid2)),Ev1),

	(\+ F1=F2 -> (abolish(eventMap/2),abolish(node/3),use_module(F2,[eventMap/2,node/3]),getAllEventMapWithPossibleMutation(EMap1));EMap1=EMap),

	findall((Task,Event),events(_,Gid1,Task,Event),Events1),
	list_to_set(Events1,Evset1),

	findall((T,E,T1),(member((T,E),Evset1),member((T1,E),EMap1),node(T1,_,Gid1)),Ev2),
	
	abolish(node/3),
	abolish(eventMap/2),
	abolish(events/4).


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
	


createNewBarrierNodeName(Gid,Barrier):-
	nb_getval(barrier,Bcounter),
	atom_concats(['barrier',Bcounter,'_',Gid],Barrier),
	Bcounterp is Bcounter+1,
	nb_setval(barrier,Bcounterp).


getStartNode(Gid,F,Start):-
	atom_concat('graph_',G,F),
	atom_concats([G,'_start_',Gid],Start).
	