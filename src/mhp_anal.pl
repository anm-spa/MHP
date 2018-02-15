:- module(mhp_anal,[mhpOfGraphs/3,mhpOfFiles/2]).
:- use_module(src/helper).



% It creates a new graph and then perform dataflow analysis to generate mhp pair.

mhpOfGraphs(G1,G2,GName):-
	valid_graph(G1,Gid1),
	valid_graph(G2,Gid),
	(Gid=Gid1->replicateGraph(Gid,Gid2);Gid2=Gid),
	mhpOfGraphsAux(Gid1,Gid2,GFile,GName),
	use_module('src/tools.pl',[draw_action_graph/1]),
	%draw_action_graph(GFile),
	abolish(draw_action_graph/1),
	%Perform dataflow analysis
	use_module('src/dataflowAnalysis.pl',[main/1]),
	main(GFile),
	abolish(main/1),!.

%% mhpOfGraphs(G1,G2):-
%% 	absolute_file_name('src/autogen/graph.pl',FAbs1),
%% 	use_module(FAbs1,[graphInfo/2,graphLoc/2]),
%% 	graphInfo(Id1,G1),
%% 	graphInfo(Id2,G2),
%% 	graphLoc(Id1,Loc1),
%% 	graphLoc(Id2,Loc2),
%% 	getMHPResults(Id1,Loc1,M1),
%% 	getMHPResults(Id2,Loc2,M2),
%% 	crossProduct(M1,M2,M3),
%% 	length(M3,L3),
%% 	write(L3),
%% 	abolish(graphInfo/2),
%% 	abolish(graphLoc/2).

mhpOfGraphs(_G1,_G2,_):-
	write("Graph not found").


mhpOfGraphsAux(Gid1,Gid2,GFile,GName):-
	%collectEvents(Gid1,Gid2,Events1,Events2),
	collectEvents(Gid1,Gid2,Events_0,RemEventNodes),

	absolute_file_name('src/autogen/graph.pl',FAbs1),
	consult(FAbs1),
	
	graphLoc(Gid1,Loc1),
	graphLoc(Gid2,Loc2),
	
	nb_setval(barrier,1),
	% collect events connecting G1 and G2
	
	% group Events
	findall(T,member((T,_Tp),Events_0),SourceT_0),
	list_to_set(SourceT_0,SourceT),
	findall((T,Dest),(member(T,SourceT),findall(DD,member((T,DD),Events_0),Dest)),Events),
	

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
	atom_concats([':- module(graph_',AName1,'_',AName2,',[node/3,edge/3,graphs/1,eventMap/2]',').'],GModule),
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
	use_module(Loc1,[node/3,edge/3,eventMap/2]),
	getTextualFileName(Loc1,F1),
	atom_concat('graph_',AbsF,F1),
	atom_concats(['src/autogen/graphExtended_',AbsF],ExtLoc1),
	use_module(ExtLoc1,[nd/3,arc/3]),
	
	findall((N,T),(node(N,T,Gid1),\+ nd(N,T,Gid1)),Nodes),
	findall((N1,N2),(edge(N1,N2,Gid1),\+ arc(N1,N2,Gid1)),Edges0_0),
	
	getStartNode(Gid1,Start1,E1Nodes),	
	findall((Start1,NE),(edge(Start1,NE,Gid1)),Edges0_1),
	union(Edges0_0,Edges0_1,Edges),
	

	findall((T,E),eventMap(T,E),EMap1),

	(\+ Loc1=Loc2 -> (abolish(node/3),abolish(edge/3),abolish(nd/3),abolish(arc/3),abolish(eventMap/2),use_module(Loc2,[node/3,edge/3,eventMap/2]),
	                  getTextualFileName(Loc2,F2),
			  atom_concat('graph_',AbsF2,F2),
			  atom_concats(['src/autogen/graphExtended_',AbsF2],ExtLoc2),
			  use_module(ExtLoc2,[nd/3,arc/3])
	                 );true
			  %getTextualFileName(Loc2,F2)
	),
	findall((N,T),(node(N,T,Gid2),\+ nd(N,T,Gid2)),Nodes2),
	findall((N1,N2),(edge(N1,N2,Gid2),\+ arc(N1,N2,Gid2)),Edges1_1),
	getStartNode(Gid2,Start2,E2Nodes),
	findall((Start2,NE),(edge(Start2,NE,Gid2)),Edges1_2),
	union(Edges1_1,Edges1_2,Edges2),
	
	findall((T,E),eventMap(T,E),EMap2),
	union(EMap1,EMap2,EMap1_2),
	list_to_set(EMap1_2,EMap),
	%findall(N,(node(N,_,Gid2),\+ edge(_Nd,N,Gid2)),E2Nodes),

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

	write(GFileStream,'node(A,B,C):- nd(A,B,C).'),
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

	% create nodes and edges connecting the two graph
	forall(member((T1,DestList),Events),(
	    createNewBarrierNodeName(Gid,Barrier),
	    atom_concats(['node(',Barrier,',class:barrier,',Gid,').'],NodeText),
	    write(GFileStream,NodeText),
	    nl(GFileStream),

	    atom_concats(['edge(',T1,',',Barrier,',',Gid,').'],EdgeTxt1),
	    write(GFileStream,EdgeTxt1),
	    nl(GFileStream),


	    forall(member(T2,DestList),(
	    	    atom_concats(['edge(',Barrier,',',T2,',',Gid,').'],EdgeTxt2),
		    write(GFileStream,EdgeTxt2),
		    nl(GFileStream)))
		)),



	%% forall(member((T1,_E1,T2),Events1),(
	%%     createNewBarrierNodeName(Gid,Barrier),
	%%     atom_concats(['node(',Barrier,',class:barrier,',Gid,').'],NodeText),
	%%     write(GFileStream,NodeText),
	%%     nl(GFileStream),

	%%     atom_concats(['edge(',T1,',',Barrier,',',Gid,').'],EdgeTxt1),
	%%     write(GFileStream,EdgeTxt1),
	%%     nl(GFileStream),

	%%     atom_concats(['edge(',Barrier,',',T2,',',Gid,').'],EdgeTxt2),
	%%     write(GFileStream,EdgeTxt2),
	%%     nl(GFileStream)
	%% )),


	%% forall(member((T1,_E2,T2),Events2),(
	%%     createNewBarrierNodeName(Gid,Barrier),
	%%     atom_concats(['node(',Barrier,',class:barrier,',Gid,').'],NodeText),
	%%     write(GFileStream,NodeText),
	%%     nl(GFileStream),

	%%     atom_concats(['edge(',T1,',',Barrier,',',Gid,').'],EdgeTxt1),
	%%     write(GFileStream,EdgeTxt1),
	%%     nl(GFileStream),

	%%     atom_concats(['edge(',Barrier,',',T2,',',Gid,').'],EdgeTxt2),
	%%     write(GFileStream,EdgeTxt2),
	%%     nl(GFileStream)
	%% )),

	write(GFileStream,"%Unique start node connecting the two graph"),
	nl(GFileStream),
	
	atom_concats(['node(start_',AName1,'_',AName2,'_',Gid,',class:label,',Gid,').'],StartNodeText),
	write(GFileStream,StartNodeText),
	nl(GFileStream),

	createNewBarrierNodeName(Gid,Barrier),
	atom_concats(['node(',Barrier,',class:barrier,',Gid,').'],NodeText),
	write(GFileStream,NodeText),
	nl(GFileStream),

	atom_concats(['start_',AName1,'_',AName2,'_',Gid],StartNode),
	atom_concats(['edge(',StartNode,',',Barrier,',',Gid,').'],SArc),
	write(GFileStream,SArc),
	nl(GFileStream),
	
	atom_concats(['edge(',Barrier,',',Start1,',',Gid,').'],EStart1),
	write(GFileStream,EStart1),
	nl(GFileStream),
	
	atom_concats(['edge(',Barrier,',',Start2,',',Gid,').'],EStart2),
	write(GFileStream,EStart2),
	nl(GFileStream),
	
	atom_concats(['graphs([',Gid,']).'],GIdInfo),
	write(GFileStream,GIdInfo),
	nl(GFileStream),


	forall(member((T,E),EMap),(
	  atom_concats(['eventMap(',T,',',E,').'],EventInfo),
	  write(GFileStream,EventInfo),
	  nl(GFileStream)
	)),

	
	%% findall(T2,member((_T1,_Ev,T2),Events1),G1Events),
	%% findall(T2,member((_Ts1,_Evs,T2),Events2),G2Events),
	%% union(G1Events,G2Events,GEvents),
	%% union(GEvents,[Start1,Start2],AllGEvents),
	union(E1Nodes,E2Nodes,_ENodes),
	%% subtract(ENodes,AllGEvents,RemEventNodes),
	
	abolish(node/3),
	abolish(edge/3),
	abolish(nd/3),
	abolish(arc/3),
	close(GFileStream),

	% write to the ExtendedGraph
	atom_concats(['src/autogen/graphExtended_',AName1,'_',AName2,'.pl'],GExtendedFile),
	open(GExtendedFile,write,GExtFileStream),
	atom_concats([':- module(graphExtended_',AName1,'_',AName2,',[nd/3,arc/3]',').'],GEModule),
	write(GExtFileStream,GEModule),
	nl(GExtFileStream),
	
	% create this dummy node so that the compiler does not complain
	% It does not have any effect on the mhp analysis
	write(GExtFileStream,'nd(dummyNode,class:label,0).'),
	nl(GExtFileStream),

	forall(member(EN,RemEventNodes),(
             	atom_concats(['arc(',Barrier,',',EN,',',Gid,').'],ArcText),
		write(GExtFileStream,ArcText),
		nl(GExtFileStream)
	)),
	

	close(GExtFileStream),

	use_module('src/autogen/graph.pl',[graphInfo/2,graphLoc/2,origin/2]),	
	findall((G,I),graphInfo(G,I),GNameInfo_0),
	findall((G,I),graphLoc(G,I),GLocInfo_0),
	findall((IdG,O),origin(IdG,O),OriginInfo_0),
	union(GNameInfo_0,[(Gid,GName)],GNameInfo_1),	
	union(GLocInfo_0,[(Gid,GFile)],GLocInfo_1),
	list_to_set(GNameInfo_1,GNameInfo),
	list_to_set(GLocInfo_1,GLocInfo),

	origin(Gid1,Origin1),
	origin(Gid2,Origin2),
	union(Origin1,Origin2,Origin),

	union(OriginInfo_0,[(Gid,Origin)],OriginInfo_1),
	list_to_set(OriginInfo_1,OriginInfo),

	open('src/autogen/graph.pl',write,GStream),

	write(GStream,':-module(graph,[graphInfo/2,graphLoc/2,origin/2]).'),
	nl(GStream),
	write(GStream,':- discontiguous graphInfo/2.'),
	nl(GStream),
	write(GStream,':- discontiguous graphLoc/2.'),
	nl(GStream),

	write(GStream,':- discontiguous origin/2.'),
	nl(GStream),

	forall(member((G,I),GNameInfo),(
	   atom_concats(['graphInfo(',G,',',I,').'],NewGInfo),
	   write(GStream,NewGInfo),
	   nl(GStream)
	)),

	forall(member((G,I),GLocInfo),(
	   atom_concats(['graphLoc(',G,',\'',I,'\').'],NewGLoc),
	   write(GStream,NewGLoc),
	   nl(GStream)
	)),

	forall(member((IdG,O),OriginInfo),(
	   write(GStream,'origin('), 
	   write(GStream,IdG),
	   write(GStream,','),
	   write(GStream,O),
	   write(GStream,').'),
	   nl(GStream)
	)),


	%union(Origin1,Origin2,Origin),
	%atom_concats(['origin(',Origin,').'],OriginInfo),
	%write(GFileStream,OriginInfo),
	%nl(GFileStream),


	close(GStream,[force(true)]),
	abolish(origin/2),
	abolish(graphInfo/2),
	abolish(graphLoc/2),!.


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



is_equal_or_mutated(E,Ep):-
	((E=Ep);is_mutation(E,Ep)).

% sometimes a letter s before done is extra.
is_mutation(E,Ep):-
	atom_concat(X,'sdone',E),
	atom_concat(X,_,Ep).

is_mutation(E,Ep):-
	atom_concat(X,'done',E),
	atom_concat(X,_,Ep).


%% getMutatedMap([],[]).
%% getMutatedMap([(T,E)|Ts],[(T,E1)|Set]):-
%% 	events(_,_G,_T,E1),
%% 	is_mutation(E,E1),!,
%% 	getMutatedMap(Ts,Set).

%% getMutatedMap([(T,E)|Ts],[(T,E)|Set]):-
%% 	getMutatedMap(Ts,Set).

%% getAllEventMapWithPossibleMutation(EMap):-
%% 	findall((T,E),eventMap(T,E),Map1),
%% 	findall((T,E),(member((T,E),Map1),events(_,_G,_TT,E)),Map2),
%% 	list_to_set(Map1,Set1),
%% 	list_to_set(Map2,Set2),
%% 	subtract(Set1,Set2,Set3),
%% 	getMutatedMap(Set3,Set4),
%% 	union(Set2,Set4,EMap).

%% collectEvents(Gid1,Gid2,Ev1,Ev2):-
%% 	use_module('src/autogen/events.pl',[events/4]),
%% 	events(F1,Gid1,_T1,_E2),
%% 	events(F2,Gid2,_T2,_E1),

%% 	use_module(F1,[eventMap/2,node/3]),

%% 	getAllEventMapWithPossibleMutation(EMap),
	
%% 	findall((Task,Event),events(_,Gid2,Task,Event),Events2),
%% 	list_to_set(Events2,Evset2),

%% 	findall((T,E,T1),(member((T,E),Evset2),member((T1,E),EMap),node(T1,_,Gid2)),Ev1),

%% 	(\+ F1=F2 -> (abolish(eventMap/2),abolish(node/3),use_module(F2,[eventMap/2,node/3]),getAllEventMapWithPossibleMutation(EMap1));EMap1=EMap),

%% 	findall((Task,Event),events(_,Gid1,Task,Event),Events1),
%% 	list_to_set(Events1,Evset1),

%% 	findall((T,E,T1),(member((T,E),Evset1),member((T1,E),EMap1),node(T1,_,Gid1)),Ev2),
	
%% 	abolish(node/3),
%% 	abolish(eventMap/2),
%% 	abolish(events/4).


collectEvents(Gid1,Gid2,Events,RET):-
	consult('src/autogen/events.pl'),
	
	consult('src/autogen/graph.pl'),
	graphLoc(Gid1,F1),
	graphLoc(Gid2,F2),
	abolish(graphLoc/2),
	abolish(graphInfo/2),
	abolish(origin/2),

	findall((T,E),(events(_,Gid1,T,E);events(_,Gid2,T,E)),AllEvents),
	
	abolish(events/4),
	
	use_module(F1,[eventMap/2,node/3]),
	findall((Tp,T),(member((Tp,Ep),AllEvents),eventMap(T,E),(node(T,_,Gid1);node(T,_,Gid2)),is_equal_or_mutated(E,Ep)),Ev1),

	findall(T,member((_,T),Ev1),TaskSet1),
	findall(T,eventMap(T,_E3),AllTE1),
	subtract(AllTE1,TaskSet1,RET1),
	
	abolish(node/3),
	abolish(eventMap/2),

	use_module(F2,[eventMap/2,node/3]),
	findall((Tp,T),(member((Tp,Ep),AllEvents),eventMap(T,E),(node(T,_,Gid1);node(T,_,Gid2)),is_equal_or_mutated(E,Ep)),Ev2),

	findall(T,member((_,T),Ev2),TaskSet2),
	findall(T,eventMap(T,_E),AllTE2),
	subtract(AllTE2,TaskSet2,RET2),

	union(RET1,RET2,RET3),
	list_to_set(RET3,RET),
	
	abolish(node/3),
	abolish(eventMap/2),
	union(Ev1,Ev2,Ev),
	list_to_set(Ev,Events).
	

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


getStartNode(Gid,Start,Nodes):-
%	findall(N,(node(N,_,Gid),\+ edge(_N,N,Gid)),Nodes),
	findall(N,(node(N,_,Gid),\+ nd(N,_,Gid),no_real_edge(N,Gid)),Nodes),
	member(Start,Nodes),
	atom_concat(rstartActivity,_,Start),!.

	%atom_concat('graph_',G,F),
	%atom_concats([G,'_start_',Gid],Start).
	
no_real_edge(N,Gid):-
	findall(E,edge(E,N,Gid),EList),
	forall(member(E,EList),nd(E,_,Gid)).


valid_graph(G,G):-
	number(G),!,
	use_module('src/autogen/graph.pl',[graphInfo/2]),
	graphInfo(G,_Gp),
	abolish(graphInfo/2).

valid_graph(G,Gid):-
	use_module('src/autogen/graph.pl',[graphInfo/2]),
	graph(G,Gp),!,
	graphInfo(Gid,Gp).
	
graph(G,G).
graph(G,Gp):- atom_concat('gr_',Gp,G).
graph(G,Gp):- atom_concat('graph_',Gp,G).



% A replicate already exists
replicateGraph(Gid,Gid2):-
	use_module('src/autogen/graph.pl',[graphInfo/2]),
	graphInfo(Gid,Gname),
	atom_concats([Gname,'_replica'],GnameReplica),
	graphInfo(Gid2,GnameReplica),
	abolish(graphInfo/2),!.



replicateGraph(Gid,Gid2):-
	use_module('src/autogen/graph.pl',[graphInfo/2,graphLoc/2,origin/2]),
	graphInfo(Gid,Gname),
	graphLoc(Gid,GLoc),
	origin(Gid,OList),

	% get a new ID
	findall(ID,graphLoc(ID,_),IDList),
	max_list(IDList,Max),
	Gid2 is Max+1,

	(length(OList,1)-> Origin=[Gid2];Origin=OList),
	%get a new name
	atom_concats([Gname,'_replica'],GnameReplica),
	atom_concat(GWithoutExt,'.pl',GLoc),
	atom_concats([GWithoutExt,'_replica.pl'],GReplicaLoc),
	
	%write to graph.pl file about the new replicated graph

	open('src/autogen/graph.pl',append,GStream),

        atom_concats(['graphInfo(',Gid2,',',GnameReplica,').'],NewGInfo),
	write(GStream,NewGInfo),
	nl(GStream),
    
        atom_concats(['graphLoc(',Gid2,',\'',GReplicaLoc,'\').'],NewGLoc),
	write(GStream,NewGLoc),
	nl(GStream),

        write(GStream,'origin('), 
	write(GStream,Gid2),
	write(GStream,','),
	write(GStream,Origin),
	write(GStream,').'),
	nl(GStream),
	close(GStream),
 
	%create a new graph
	getTextualFileName(GReplicaLoc,GreplicaWithoutExt),
	atom_concat('graph_',GTemp,GreplicaWithoutExt),

	% Info about the Extended Graph
	atom_concat('graphExtended_',GTemp,GExtendedModuleName),
	atom_concats(['src/autogen/graphExtended_',GTemp,'.pl'],GExtendedReplicaLoc),
		
	open(GReplicaLoc,write,GFileStream),

	atom_concats([':- module(',GreplicaWithoutExt,',[node/3,edge/3,graphs/1,eventMap/2]',').'],GModule),
	write(GFileStream,GModule),
	nl(GFileStream),
	
	write(GFileStream,':- discontiguous node/3.'),
	nl(GFileStream),

	write(GFileStream,':- discontiguous edge/3.'),
	nl(GFileStream),

	atom_concats([':- use_module("',GExtendedModuleName,'").'],GExtendedModuleText),
	write(GFileStream,GExtendedModuleText),
	nl(GFileStream),


	% write all nodes and edges of the graph into the replicated graph
	use_module(GLoc,[node/3,edge/3,eventMap/2,graphs/1]),
	getTextualFileName(GLoc,F1),
	atom_concat('graph_',AbsF,F1),
	atom_concats(['src/autogen/graphExtended_',AbsF,'.pl'],GExtLoc),
	use_module(GExtLoc,[nd/3,arc/3]),
	
	findall((N,T),(node(N,T,Gid),\+ nd(N,T,Gid)),Nodes),
	findall((N1,N2),(edge(N1,N2,Gid),\+ arc(N1,N2,Gid)),Edges),
	findall((N1,N2),(eventMap(N1,N2)),EMap),
	graphs(GList),

	% write nodes and edges info	
	forall(member((N,Type),Nodes),(
	    term_to_atom(Type,T),
	    %term_to_atom(N,Natom),
	    atom_concat(N,'_copy',NCopy),
	    atom_concats(['node(',NCopy,',',T,',',Gid2,').'],NodeText),
	    write(GFileStream,NodeText),
	    nl(GFileStream)
	)),
	
	forall(member((N1,N2),Edges),(
	    atom_concat(N1,'_copy',NCopy1),
	    atom_concat(N2,'_copy',NCopy2),
	    atom_concats(['edge(',NCopy1,',',NCopy2,',',Gid2,').'],EdgeText),
	    write(GFileStream,EdgeText),
	    nl(GFileStream)
	)),

	write(GFileStream,'node(A,B,C):- nd(A,B,C).'),
	nl(GFileStream),
	
	write(GFileStream,'edge(A,B,C):- arc(A,B,C).'),
	nl(GFileStream),

	forall(member((A,B),EMap),(
	    atom_concat(A,'_copy',ACopy),
	    atom_concats(['eventMap(',ACopy,',',B,').'],MapText),
	    write(GFileStream,MapText),
	    nl(GFileStream)
	)),

	write(GFileStream,'graphs('), 
	write(GFileStream,GList),
	write(GFileStream,').'),
	nl(GFileStream),
	close(GFileStream),


		% Info about the Extended Graph
	atom_concat('graphExtended_',GTemp,GExtModuleName),
	atom_concats(['src/autogen/graphExtended_',GTemp,'.pl'],GExtendedReplicaLoc),
		
	open(GExtendedReplicaLoc,write,GExtFileStream),

	atom_concats([':- module(',GExtModuleName,',[nd/3,arc/3]).'],GExtModule),
	write(GExtFileStream,GExtModule),
	nl(GExtFileStream),
	
	write(GExtFileStream,':- discontiguous nd/3.'),
	nl(GExtFileStream),

	write(GExtFileStream,':- discontiguous arc/3.'),
	nl(GExtFileStream),


	findall((N,T),nd(N,T,Gid),ExtNodes),
	findall((N1,N2),arc(N1,N2,Gid),ExtEdges),

	% write nd and arc info	
	forall(member((N,Type),ExtNodes),(
	    term_to_atom(Type,T),
	    %term_to_atom(N,Natom),
	    atom_concat(N,'_copy',NCopy),
	    atom_concats(['nd(',NCopy,',',T,',',Gid2,').'],NodeText),
	    write(GExtFileStream,NodeText),
	    nl(GExtFileStream)
	)),
	
	forall(member((N1,N2),ExtEdges),(
	    atom_concat(N1,'_copy',NCopy1),
	    atom_concat(N2,'_copy',NCopy2),
	    atom_concats(['arc(',NCopy1,',',NCopy2,',',Gid2,').'],EdgeText),
	    write(GExtFileStream,EdgeText),
	    nl(GExtFileStream)
	)),

	close(GExtFileStream),

	% writing to event File
	use_module('src/autogen/events.pl',[events/4]),
	
	open('src/autogen/events.pl',append,EStream),

	forall(events(_Gf,Gid,Tsk,TEvent),(
	        atom_concat(Tsk,'_copy',TskCopy),
		atom_concats(['events(\'',GReplicaLoc,'\',',Gid2,',',TskCopy,',',TEvent,').'],EText),
		write(EStream,EText),
		nl(EStream)
	)),
	close(EStream),
	abolish(graphLoc/2),
	abolish(origin/2),
	abolish(node/3),
	abolish(nd/3),
	abolish(edge/3),
	abolish(arc/3),
	abolish(graphs/1),
	abolish(eventMap/2),
	abolish(graphInfo/2).
