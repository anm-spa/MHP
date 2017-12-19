:- module(dataflowAnalysis,[mainDefault/0,main/1]).
:- use_module(helper).

:- dynamic sync/2.
:- dynamic atom/2.
:- dynamic df/2.
:- dynamic joinArity/2.
:- dynamic barriers/2.

:- use_module(config/config).
:- use_module(library(lists)).

:- style_check(-singleton).
:- style_check(-discontiguous).


debug_mode(false).

mainDefault:-
	use_module(autogen/graph),
	use_module(autogen/graphExtended),
	graphs(GidList),
	mhpDir(MhpDir),
	atom_concat(MhpDir,'src/autogen/mhp.pl',File),
	open(File,write,Os),
	write(Os,':-module(mhp,[mhp/2]).'),nl(Os),nl(Os),

	atom_concat(MhpDir,'src/autogen/cht.pl',Cht),
	open(Cht,write,ChtHandler),
	write(ChtHandler,':-module(cht,[cht_lessthan/2]).'),nl(ChtHandler),nl(ChtHandler),

	retractall(atom(_Node,_Atom)),
	retractall(df(_N,_DF)),
	retractall(joinArity(_Nd,_Arity)),	
	retractall(barriers(_NN,_BB)),	
	nb_setval(taskcounter,1),
	nb_setval(barriercounter,1),
	main_aux(GidList,Os,ChtHandler,0,_PairNo),
	close(Os),
	close(ChtHandler).


main(F):-
	absolute_file_name(F,Graph),
	use_module(Graph),
	getTextualFileName(F,GFile),
	atom_concat('graph_',GF,GFile),
	file_directory_name(Graph,D),
	atom_concats([D,'/','graphExtended_',GF,'.pl'],ExtGraph),
	use_module(ExtGraph),

	graphs(GidList),
	mhpDir(MhpDir),
	atom_concats([MhpDir,'src/autogen/mhp_',GF,'.pl'],MHPFile),
	open(MHPFile,write,Os),
	write(Os,':-module(mhp,[mhp/2]).'),nl(Os),nl(Os),

	atom_concats([MhpDir,'src/autogen/cht_',GF,'.pl'],ChtFile),
	open(ChtFile,write,ChtHandler),
	write(ChtHandler,':-module(cht,[cht_lessthan/2]).'),nl(ChtHandler),nl(ChtHandler),

	retractall(atom(_Node,_Atom)),
	retractall(df(_N,_DF)),
	retractall(joinArity(_Nd,_Arity)),	
	retractall(barriers(_NN,_BB)),	
	nb_setval(taskcounter,1),
	nb_setval(barriercounter,1),
	main_aux(GidList,Os,ChtHandler,0,_PairNo),
	close(Os),
	close(ChtHandler).
	%delete_import_module(dataflowAnalysis,Graph),
	%delete_import_module(dataflowAnalysis,ExtGraph),




main_aux([],_,_FileHandler,P,P).	
main_aux([G|Gs],Os,FileHandler,C,P):-
    edge(_N,_N1,G),!,   % check if an edge in graph G exists	
    mhpmain(G,Os,FileHandler,C,C1),
    main_aux(Gs,Os,FileHandler,C1,P).

main_aux([_G|Gs],Os,FileHandler,C,P):-
    main_aux(Gs,Os,FileHandler,C,P).


%*********************Initialization*************************************

selectGraph(N,Nodes,Edges):-
	findall((P,Q),edge(P,Q,N),MEdges),
	findall(P,node(P,_,N),MNodes),
	list_to_set(MEdges,Edges),
	list_to_set(MNodes,Nodes).

init_DataFlowSets([],[]).
init_DataFlowSets([N|Ns],[(N,[])|FlowSets]):-
    assertz(df(N,[])),
    init_DataFlowSets(Ns,FlowSets).

initWorklist(WL,N):-                 
    findall(P,(node(P,_,N),\+ edge(_X,P,N)),List),
    findall((P,Q),(member(P,List),edge(P,Q,N)),WL).

%************************************************************************


mhpmain(G,Os,FileHandler,Count,Countp):-

    % it creates a sync database for each node P, i.e. sync(P,B) such that P synchronizes with barrier B. B can be run_to_completion.	
    createRelevantBarrierListForEachNode(G),

    % creating atoms of all barrier and decision nodes	
    findall(B,((node(B,class:barrier,G);node(B,class:decision,G)),sync(_,B)),BList),
    findOrCreateBarrierAtom(BList,BAtomList),


    %% Discover the list of all barriers that a barrier node is associated	
    findall(B,node(B,class:barrier,G),BarrierList),
    forall(member(B,BarrierList),(		
	getAllBarriersWithIndirectSync([B], G,[],Barriers),
        assertz(barriers(B,Barriers)))),
 
    findall((P,Bset),(node(P,T,G),\+ T=class:barrier,searchImmediatePreviousBarrierNode(P,G,Bl),list_to_set(Bl,Bset)),NodePredBarrier),	
    forall(member((P,PBlist),NodePredBarrier),(findall(BB,(member(B,PBlist),barriers(B,BB)),BBList),flatten(BBList,Bdiff),list_to_set(Bdiff,PBset),assertz(barriers(P,PBset)))),	
      
    findall((N,L),((node(N,class:join,G);node(N,class:multijoin,G)),findall(E,(edge(E,N,G)),EList),length(EList,L),L>0),EffectiveJoinPred),   
    forall(member((N,L),EffectiveJoinPred),assertz(joinArity(N,L))),

    selectGraph(G,Nodes,Edges),
    length(Nodes,NodeCount),

    init_DataFlowSets(Nodes,FlowSets),      % FlowSets is the list of entry and exit sets of each node (n,Entry,Exit)
    initWorklist(WL,G),
    forwardDataFlowAnalysis(WL,G),
    findall((P,Entry),(df(P,Entry),node(P,class:exec,G)),FlowSetsUp),
 
    obtain_concurrentTaskList(FlowSetsUp,ConcurrentTaskList,G),
    selectMHPNodes(ConcurrentTaskList,G,MHPList),

    findall(P,(node(P,class:exec,G),member((P,PList),MHPList),PList=[]),LoneNodes),	
    findall((P,dummyTask),(member(P,LoneNodes)),MHPAux),	
    refineMHPPair(MHPList,[],G,MHPTasks),	
    union(MHPTasks,MHPAux,MHPUnion),	
    debugWriteInfo(MHPUnion,Os,true,Count,Countp),

    % Non-concurrency analysis
    cannotHappenTogether(G,PO),
    storeCHTInfoInFile(FileHandler,PO).



union_pairs([],PList,Acc,Rs):-
   union(PList,Acc,Rs). 

union_pairs([(P,Q)|Ns],PList,Acc,Rs):-
    \+ member((P,Q),PList),
    \+ member((Q,P),PList),!,
    union([(P,Q)],Acc,Accp),
    union_pairs(Ns,PList,Accp,Rs).

union_pairs([_|Ns],PList,Acc,Rs):-
    union_pairs(Ns,PList,Acc,Rs).	

refineMHPPair([],Rs,_G,Rs).
refineMHPPair([(P,L)|Ns],Acc,G,Rs):-
    findall((P,Q),(member(Q,L),P\==Q,node(Q,class:exec,G)),Pair), 
    union_pairs(Pair,Acc,[],Accp),
    refineMHPPair(Ns,Accp,G,Rs).

selectMHPNodes([],_G,[]).
selectMHPNodes([(P,L)|Ns],G,[(P,L)|Rs]):-
	node(P,class:exec,G),!,
	selectMHPNodes(Ns,G,Rs).

selectMHPNodes([_|Ns],G,Rs):-
	selectMHPNodes(Ns,G,Rs).
	
debugWriteInfo(_FlowSetsUp,_,false,C,C).
debugWriteInfo(FlowSetsUp,Os,true,Count,Countp):-
       writeMHPInfo(FlowSetsUp,Os,Count,Countp).  
	

writeMHPInfo([],_,C,C).
writeMHPInfo([(P,Q)|Ls],Os,Count,Countp):- 
	write(Os,'mhp('),
	write(Os,P),
	write(Os,','),
	write(Os,Q),
	write(Os,').'),
	nl(Os),
	(Q=dummyTask->C1=Count; C1 is Count+1),
	writeMHPInfo(Ls,Os,C1,Countp).

concurrentTaskList_aux([],CList,CList,_).
concurrentTaskList_aux([Atom|AtomRs],Acc,CList,G):-
    %(atom(P,Atom);(atom_concat(A,B,Atom),atom_concat('*',_L,B),atom(P,A))),
    extractTaskNode(Atom,P),	
    node(P,T,G),
    member(T,[class:exec,class:decision,class:label]),!,
    union(Acc,[P],Acc1),
    concurrentTaskList_aux(AtomRs,Acc1,CList,G).

concurrentTaskList_aux([Atom|AtomRs],Acc,CList,G):-
    concurrentTaskList_aux(AtomRs,Acc,CList,G).



concurrentTaskList([],[],_).
concurrentTaskList([(P,B)|Bs],[(P,BSet)|BSetRs],G):-
    concurrentTaskList_aux(B,[],BSet,G),
    concurrentTaskList(Bs,BSetRs,G).

obtain_concurrentTaskList(FlowSetsUp,ConcurrentTaskList,G):-
    %findall(Entry,(member((P,Entry),FlowSetsUp),node(P,_,G)),BarrierEntry),
    %debugWriteInfo(BarrierEntry,"DataFlow Info in Atoms",true),nl,
    BarrierEntry=FlowSetsUp,
    concurrentTaskList(BarrierEntry,ConcurrentTaskList,G).
    

cleanup:-
    retractall(atom(_Node,_Atom)),
    retractall(sync(_Node,_Barrier)).


% For graph with ID no G, this predicate finds all barrier, exec, and label nodes to be synchronized with a barrier node, 
% and store this info in a dynamic database called sync.

createRelevantBarrierListForEachNode(G):-
    findForkBarriersComb(G,NodeWithSync),
    % writeAllPair(NodeWithSync),
    findall((B,run_to_completion),((node(B,class:exec,G);node(B,class:label,G);node(B,class:decision,G)),
				   \+ member((B,_),NodeWithSync)),NodeWithoutSync),
    retractall(sync(_Node,_Barrier)),
    writeToSyncDatabase(NodeWithSync),
    writeToSyncDatabase(NodeWithoutSync).





blockingJoinNode(Q,GraphNo):-
	(node(Q,class:join,GraphNo);node(Q,class:multijoin,GraphNo)),
	joinArity(Q,QDeg),
	QDeg>=1.

nonblockingJoinNode(Q,GraphNo):-
	(node(Q,class:join,GraphNo);node(Q,class:multijoin,GraphNo)),
	joinArity(Q,QDeg),
	QDeg=0.

joinNode(Q,GraphNo):-
	node(Q,class:join,GraphNo).


go_checkpoint.

db_checkpoint(P,Q):-
	%(P=rbarrier2;Q=rbarrier2),
	(P=rbarrier33;Q=rbarrier33),
%	((node(P,class:join,_);node(P,class:multijoin,_));
%	(node(Q,class:join,_);node(Q,class:multijoin,_))),    
	go_checkpoint.

db_checkpoint(_P,_Q).

forwardDataFlowAnalysis([],_GraphNo):-!.
forwardDataFlowAnalysis(WL,GraphNo):-
	WL=[(P,Q)|Ws],
	db_checkpoint(P,Q),
	df(Q,QEntry),
	applyTransferFunction(Q,GraphNo,QEntry,QExit),

	retractall(df(Q,QEntry)),
	assertz(df(Q,QExit)),

	debugWriteInfo(P,Q,Q,QEntry,QExit),

	(blockingJoinNode(Q,GraphNo)->
	(
	    joinArity(Q,QDeg),
	    retractall(joinArity(Q,QDeg)),
	    QDegAux is QDeg-1,
	    assertz(joinArity(Q,QDegAux))
	);true),

	(\+ blockingJoinNode(Q,GraphNo)->
	(
	    findall((Q,R),(edge(Q,R,GraphNo)),EdgeList),
	    (nonblockingJoinNode(Q,GraphNo)->
		(findall(X,(member(X,QExit),atom_concat('p',Y,X),member(Y,QExit)),RedundantAtoms),
		subtract(QExit,RedundantAtoms,QExitReduced)
	    );QExitReduced=QExit
	    ),
	    updateWorklistAndDataflowSet(P,EdgeList,QExitReduced,Ws,WsAux,GraphNo)
	);WsAux=Ws),    

	
  
						%Algorithm 1: line 10-15
	list_to_set(QEntry,QEntrySet),
	list_to_set(QExit,QExitSet),	
	set_diff(QExitSet,QEntrySet,ResSet),	
	length(ResSet,Len),	
	(
    	    (node(P,class:barrier,GraphNo),Len>0)->
    		(
    		    findall((P,R),(edge(P,R,GraphNo),R\=Q),Successors),
		    genFactsforParallelBranch(ResSet,ParResSet),
		   % debugWriteInfoParBranch(Successors, ParResSet),
    		    updateWorklistAndDataflowSet(P,Successors,ParResSet,WsAux, WsUp,GraphNo)
    		);
    		(WsUp=WsAux)	
	    ),
	    
						%Algorithm 1: line 16-24

	    (\+joinNode(Q,GraphNo)->
		
		(
		    extractParAndSelfAtoms(QExit,ParAtoms,SelfAtoms),
		    getDistantSymmetricParallelNodesAndDF(ParAtoms,SelfAtoms,P,Q,SymBranch),
		    updateWLAndDF(P,Q,SymBranch,GraphNo,WsUp,WMod)
		);
		WMod=WsUp
	    ),
	    WUpdate=WMod,
	    forwardDataFlowAnalysis(WUpdate,GraphNo).   

get_predAtomList([],_PAtoms,_GraphNo,[]).
get_predAtomList([R|Rs],PAtoms,GraphNo,[(R,PSelfMod)|CopySelf]):-
	getAllBarriersWithIndirectSync([R],GraphNo,[],BarList),
	%findall(B,(member(B,REntry),atom(N,B),node(N,class:barrier,_)),BarList),
	removeTasksHavingBarriers(PAtoms,BarList,PSelfMod),
	get_predAtomList(Rs,PAtoms,GraphNo,CopySelf).


extractParAndSelfAtoms([],[],[]).
extractParAndSelfAtoms([A|As],ParAtoms,SelfAtoms):-
	atom(N,A),
	node(N,class:barrier,_),!,
	extractParAndSelfAtoms(As,ParAtoms,SelfAtoms).

extractParAndSelfAtoms([A|As],[A|PA],SelfAtoms):-
	atom_concat('p',_B,A),!,
	extractParAndSelfAtoms(As,PA,SelfAtoms).

extractParAndSelfAtoms([A|As],PA,[Par|SA]):-
	atom_concat('p',A,Par),
	extractParAndSelfAtoms(As,PA,SA).


genFactsforParallelBranch([],[]).
genFactsforParallelBranch([A|As],Bs):-
	atom(N,A),
	node(N,class:barrier,_),!,
	genFactsforParallelBranch(As,Bs).


% an atom synchronized with decision node will not be copied to parallel branch unless the decision node is replaced by appropriate barrier node.
genFactsforParallelBranch([A|As],Bs):-
	atom_concat(_,D,A),
	atom(N,D),
	node(N,class:decision,_),!,
	genFactsforParallelBranch(As,Bs).

genFactsforParallelBranch([A|As],[B|Bs]):-
	atom_concat('p',A,B),
	genFactsforParallelBranch(As,Bs).


getDistantSymmetricParallelNodesAndDF([],_S,_P,_Q,[]).
getDistantSymmetricParallelNodesAndDF([X|Xs],S,P,Q,[(Mp,Sp)|Rs]):-
	extractTaskNode(X,Mp),
	Mp\=P,Mp\=Q, 
	df(Mp,MEntry),
	findall(B,(member(B,MEntry),atom(N,B),node(N,class:barrier,_)),BarrierList),

	% it removes all atoms having barriers from the BarrierList at the end; add a p-tag to the native atom 
	removeTasksHavingBarriers(S,BarrierList,Sp),
	
        \+ subset(Sp,MEntry),!,
	%debugWrite(Mp,Sp,P,Q),
	getDistantSymmetricParallelNodesAndDF(Xs,S,P,Q,Rs).

getDistantSymmetricParallelNodesAndDF([_X|Xs],S,P,Q,SymBranch):-
	getDistantSymmetricParallelNodesAndDF(Xs,S,P,Q,SymBranch).



atoms_nodes([]).
atoms_nodes([S|Ss]):-
	atom_concat('p',_SS,S),
	extractTaskNode(S,N),
	write(N), write(' '),!,
	atoms_nodes(Ss).

atoms_nodes([S|Ss]):-
	atom_concat('p',S,Sp),
	extractTaskNode(Sp,N),
	write(N), write(' '),!,
	atoms_nodes(Ss).

atoms_nodes([_S|Ss]):-
	atoms_nodes(Ss).


removeTasksHavingBarriers([],_B,[]).

% atom S is synchronized with one of the barriers from BList, it is thus discarded
removeTasksHavingBarriers([S|Ss],BList,Sp):-
	extractBarrierAtom(S,Bp),
	(member(Bp,BList);(atom(M,S),node(M,Type,_),Type=class:barrier)),!,
	removeTasksHavingBarriers(Ss,BList,Sp).

% atom S is synchronized with one of the decision nodes, it is thus discarded until it is synchronized to some barriers
removeTasksHavingBarriers([S|Ss],BList,Sp):-
	atom_concat(_,D,S),
	atom(N,D),
	node(N,class:decision,_),!,
	removeTasksHavingBarriers(Ss,BList,Sp).

removeTasksHavingBarriers([S|Ss],B,[S|Sp]):-
	removeTasksHavingBarriers(Ss,B,Sp).


updateWLAndDF(_P,_Q,[],_GraphNo,W,W).
updateWLAndDF(P,Q,[(Mp,A)|Res],GraphNo,W,WR):-
	df(Mp,MEntry),
	union(MEntry,A,NewMEntry),

	barriers(Mp,Barriers),
	getAllKillSet(Barriers,NewMEntry,KillSet),
	set_diff(NewMEntry,KillSet,RMod),

	((\+ subset(RMod,MEntry))),!,          %%% check if it works or not

	retractall(df(Mp,MEntry)),
	assertz(df(Mp,RMod)),

	debugWriteInfo(P,Q,Mp,MEntry,RMod),

	findall((R,Mp),edge(R,Mp,GraphNo),Edges),
	union(W,Edges,WAux),
	updateWLAndDF(P,Q,Res,GraphNo,WAux,WR).

updateWLAndDF(P,Q,[_|Res],GraphNo,W,WR):-	
	updateWLAndDF(P,Q,Res,GraphNo,W,WR).

%Lazy evaluation of atoms representing a decision node
update_atoms_for_decision_node(P,Q,PExit,QEntry):-
	node(P,class:decision,_),
	atom(P,Patom),
        sync(Q,B),
	atom(B,Batom),!,
	findall(Z,(member(X,PExit),((atom_concat(Y,Patom,X),atom_concat(Y,Batom,Z));(Z=X,\+ atom_concat(_,Patom,X)))),QEntry).

update_atoms_for_decision_node(P,Q,PExit,PExit).

updateWorklistAndDataflowSet(_S,[],_ResSet,W,W,_).

updateWorklistAndDataflowSet(S,[(P,Q)|Es],ResSet,W,WsUp,GraphNo):-
	df(Q,QEntry),
	update_atoms_for_decision_node(P,Q,ResSet,ResSetMod),
	barriers(Q,Barriers),
	getAllKillSet(Barriers,ResSetMod,KillSet),
	set_diff(ResSetMod,KillSet,RMod),
	((\+ subset(RMod,QEntry));node(P,class:join,GraphNo)),!,
%	((\+ subset(RMod,QEntry));(QEntry=[],RMod=[])),!,
	union(QEntry,ResSetMod,NewQEntry),
	retractall(df(Q,QEntry)),
	assertz(df(Q,NewQEntry)),
	
	debugWriteInfo(S,P,Q,QEntry,NewQEntry),

	union(W,[(P,Q)],WAux),
	updateWorklistAndDataflowSet(S,Es,ResSet,WAux,WsUp,GraphNo).

updateWorklistAndDataflowSet(S,[_|Es],ResSet,W,WsUp,GraphNo):-
	updateWorklistAndDataflowSet(S,Es,ResSet,W,WsUp,GraphNo).


extractBarrierAtomTaskNode(X,B,N):-
	extractTaskNode(X,N),
	extractBarrierAtom(X,B).

extractBarrierAtom(X,B):-
	atom_concat(_A,Bs,X),
	atom_concat('*',B,Bs),
	atom(N,B),
	node(N,Type,_),
	Type=class:barrier.

extractBarrierAtom(X,B):-
	atom(M,X),
	node(M,Type,_),
	Type=class:barrier.
	

extractTaskNode(X,N):-
	atom_concat(A,B,X),
	atom_concat('*',_,B),
	atom_concat('p',T,A),
	atom(N,T),
	node(N,Type,_),
	\+ Type=class:barrier.

extractTaskNode(X,N):-
	atom_concat('p',T,X),
	atom(N,T),
	node(N,Type,_),
	\+ Type=class:barrier.

extractSelfTaskNode(X,N):-
	atom_concat(A,B,X),
	atom_concat('*',_,B),
	atom(N,A),
	node(N,Type,_),
	\+ Type=class:barrier.

extractSelfTaskNode(X,N):-
	atom(N,X),
	node(N,Type,_),
	\+ Type=class:barrier.


extractAllTaskNodes([],[]).
extractAllTaskNodes([T|Ts],[N|Rs]):-
	extractTaskNode(T,N),
	node(N,class:exec,_),!,
	extractAllTaskNodes(Ts,Rs).

extractAllTaskNodes([T|Ts],[N|Rs]):-
	extractSelfTaskNode(T,N),
	node(N,class:exec,_),!,
	extractAllTaskNodes(Ts,Rs).

extractAllTaskNodes([T|Ts],Rs):-
	extractAllTaskNodes(Ts,Rs).


applyTransferFunction(P,GraphNo,Entry,Exit):-
    gen(P,GraphNo,GenSet),
    kill(P,Entry,KillSet),
    subtract(Entry,KillSet,EntryAux),
    union(EntryAux,GenSet,Exit).


combineTaskBarrierAtom(_Atom,[],GenSet,GenSet).
combineTaskBarrierAtom(Atom,[B|Bs],Acc,GenSet):-
    atom_concat(Atom,'*',Atom1),
    atom_concat(Atom1,B,Atom2),
    union(Acc,[Atom2],Accp),
    combineTaskBarrierAtom(Atom,Bs,Accp,GenSet). 

generateAtoms(P,GenSet):-
    findOrCreateTaskAtom(P,Atom),
    findall(B,(sync(P,B),\+ B=run_to_completion),Barrier),
    findOrCreateBarrierAtom(Barrier,BAtom),       
    (BAtom=[]-> GenSet=[Atom]; combineTaskBarrierAtom(Atom,BAtom,[],GenSet)).
    

findOrCreateBarrierAtom([],[]).
findOrCreateBarrierAtom([B|Bs],[Batom|BAtomRs]):-
    (\+ atom(B,At)->
	 (
	    nb_getval(barriercounter,C),
	    (node(B,class:barrier,_)-> atom_concat('b',C,Batom);
		(
		    node(B,class:decision,_G),atom_concat('d',C,Batom)
		)
	    ),
	    Cp is C+1,
	    nb_setval(barriercounter,Cp),
	    assertz(atom(B,Batom))
	 );
       atom(B,Batom)
    ),
    findOrCreateBarrierAtom(Bs,BAtomRs).
    
findOrCreateTaskAtom(P,Atom):-
    (\+ atom(P,At)->
	 (
	    nb_getval(taskcounter,C), 
            atom_concat('t',C,Atom),
	    Cp is C+1,
	    nb_setval(taskcounter,Cp),
	    assertz(atom(P,Atom))
	 );
       atom(P,Atom)
    ).


searchImmediatePreviousBarrierNodeAux([],GraphNo,[]).
searchImmediatePreviousBarrierNodeAux([(P,Q)|Ws],GraphNo,[P|Ps]):-
	 node(P,class:barrier,GraphNo),!,
	 searchImmediatePreviousBarrierNodeAux(Ws,GraphNo,Ps).
 
searchImmediatePreviousBarrierNodeAux([(P,_Q)|Ws],GraphNo,Ps):-
	findall((R,P),(edge(R,P,GraphNo)),WList),
	union(WList,Ws,WsAux),
	searchImmediatePreviousBarrierNodeAux(WsAux,GraphNo,Ps).

searchImmediatePreviousBarrierNode(P,GraphNo,BList):-
	findall((Q,P),(edge(Q,P,GraphNo)),WList),
	searchImmediatePreviousBarrierNodeAux(WList,GraphNo,BList).

findForkBarriersComb(GraphNo,ForkBarrierList):-
    findall(B,(node(B,class:barrier,GraphNo);node(B,class:decision,GraphNo)),BList),
    findall((P,Q),(member(Q,BList),edge(P,Q,GraphNo)),WList),
    searchBackward(WList,[],ForkBarrierList,GraphNo).


searchBackward([],ForkBarrierList,ForkBarrierList,GraphNo).
searchBackward([(P,Q)|Rs],Acc,ForkBarrierList,GraphNo):-
    node(P,T,GraphNo),
    %(T=class:fork; T=class:multifork),!,
    (T=class:exec;T=class:label;T=class:fork;T=class:multifork),!,
    union(Acc,[(P,Q)],AccAux),
    findall((R,Q),edge(R,P,GraphNo),BList),
    append(BList,Rs,RsAux),                                    % A change is being made here     
    searchBackward(RsAux,AccAux,ForkBarrierList,GraphNo).

searchBackward([(P,Q)|Rs],Acc,ForkBarrierList,GraphNo):-
    node(P,T,GraphNo),
    (T=class:barrier;T=class:decision),!,
    union(Acc,[(P,Q)],AccAux),                       % Barrier synchronizes with another barrier
    searchBackward(Rs,AccAux,ForkBarrierList,GraphNo).


searchBackward([(P,Q)|Rs],Acc,ForkBarrierList,GraphNo):-
    findall((R,Q),edge(R,P,GraphNo),BList),
    union(BList,Rs,RsAux),
    searchBackward(RsAux,Acc,ForkBarrierList,GraphNo).


kill_aux([],_B,KillSet,KillSet).
kill_aux([P|Rs],B,Acc,KillSet):-
    atom_concat(A,B,P),
    atom_concat(Ap,'*',A),!,
    union(Acc,[P],Accp),                      %%% remove B, now a barrier atom will never be killed
    kill_aux(Rs,B,Accp,KillSet).

kill_aux([P|Rs],B,Acc,KillSet):-
    kill_aux(Rs,B,Acc,KillSet).

%kill(P,Entry,[]):-
%    node(P,A,GraphNo),
%    member(A,[class:exec,class:decision,class:label,class:fork,class:multifork,class:join,class:multijoin,class:nop,class:return]),!.


kill(P,Entry,KillSet):-
 %   node(P,class:barrier,GraphNo),
    barriers(P,Barriers),!,
    getAllKillSet(Barriers,Entry,KillSet).
	
kill(_P,_Entry,[]).
    
getAllKillSet([],_Entry,[]).
getAllKillSet([P|Ps],Entry,KillSet):-		
	atom(P,B),
	kill_aux(Entry,B,[],Kill1),
	getAllKillSet(Ps,Entry,Kill2),
	union(Kill1,Kill2,KillSet).
	


getAllBarriersWithIndirectSync([], GraphNo, B,Barriers):- list_to_set(B,Barriers).
getAllBarriersWithIndirectSync([P|Ps], GraphNo, Acc,Barriers):-
	findall(B,(sync(B,P),node(B, class:barrier,GraphNo)),BList),
	union(Acc,[P],Accp),
	set_diff(BList,Accp,Bdiff),
	union(Bdiff,Ps,PUp),
	getAllBarriersWithIndirectSync(PUp, GraphNo, Accp,Barriers).


gen_aux([],Graphno,GenSet,GenSet).
gen_aux([P|Rs],Graphno,Acc,GenSet):-
    node(P,A,GraphNo),
    member(A,[class:exec,class:decision,class:label]),!,
    generateAtoms(P,GenSet1),
    union(GenSet1,Acc,Acc1),
    findall(Q,(edge(P,Q,GraphNo)),Succ),
    union(Rs,Succ,Succ1),
    gen_aux(Succ1,Graphno,Acc1,GenSet).

gen_aux([P|Rs],Graphno,Acc,GenSet):-
    node(P,class:barrier,Graphno),!,
    % Recent Modification  (Barrier sync with another barrier)
    findOrCreateBarrierAtom([P],BList),
    union(Acc,BList,Acc1), 
    % Until here   
    gen_aux(Rs,Graphno,Acc1,GenSet).

gen_aux([P|Rs],Graphno,Acc,GenSet):-
    findall(Q,(edge(P,Q,GraphNo)),Succ),
    union(Rs,Succ,Succ1),
    gen_aux(Succ1,Graphno,Acc,GenSet).




gen(P,GraphNo,GenSet):-                      
    node(P,A,GraphNo),
    member(A,[class:label]),!,
    generateAtoms(P,GenSet).

gen(P,GraphNo,GenSet):-
    node(P,A,GraphNo),
    member(A,[class:barrier]),!,
    findOrCreateBarrierAtom([P],GenSet).

gen(P,GraphNo,GenSet):-
	node(P,A,GraphNo),
	member(A,[class:exec,class:fork,class:multifork]),!,
	generateAtoms(P,GenSet).
    
gen(P,GraphNo,[]):-                      
    node(P,A,GraphNo).


   

writeToSyncDatabase([]).
writeToSyncDatabase([(P,B)|Ns]):-
    assertz(sync(P,B)),
    %write('Node '),write(P), write('Synced '), write(B),nl,	
    writeToSyncDatabase(Ns).

writeAllPair([]).
writeAllPair([(P,B)|Ns]):-
    write(P),write(" "), write(B),nl,
    writeAllPair(Ns).	


set_diff([],_S,[]).
set_diff([P|Ps],S,[P|Rs]):-
	\+ member(P,S),!,
	set_diff(Ps,S,Rs).

set_diff([_P|Ps],S,Rs):-
	set_diff(Ps,S,Rs).


find_and_remove(_,[],[]).

find_and_remove((N,E1),[(N,_E2)|Ns],Rs):-
	find_and_remove((N,E1),Ns,Rs),!.

find_and_remove((N,E1),[(N2,E2)|Ns],[(N2,E2)|Rs]):-
	N\=N2,
	find_and_remove((N,E1),Ns,Rs).


%***************************Non-concurrency analysis*************************************
cannotHappenTogether(G,PO):-
	findall(N,node(N,class:barrier,G),Nodes),
	findall(N,nd(N,class:barrier,G),Nds),
	subtract(Nodes,Nds,NodeList),
        findall((N,FSet,[]),(member(N,NodeList),edge(Q,N,G),df(Q,QExit),kill(N,QExit,KillSet),extractAllTaskNodes(KillSet,F),list_to_set(F,FSet)),WL),
	collectAllPO(WL,G,[],PO).
 	

collectAllPO([],_G,PO,PO).
collectAllPO([(_P,[],_V)|Ws],G,Acc,PO):-
	!,
	collectAllPO(Ws,G,Acc,PO).

collectAllPO([(P,F,V)|Ws],G,Acc,PO):-
	findall(Q,edge(P,Q,G),Succ),
	subtract(Succ,V,NotVisitedSucc),
	union(V,[P],Vp),
	findall((Q,F,Vp),member(Q,NotVisitedSucc),WNext),
	union(WNext,Ws,WUp),
	collectPO_ifany(NotVisitedSucc,G,F,[],POAux),
	union(Acc,POAux,AccM),
	collectAllPO(WUp,G,AccM,PO).


collectPO_ifany([],_G,_F,PO,PO).
collectPO_ifany([M|Ms],G,F,Acc,PO):-
	node(M,class:exec,G),!,
	gen(M,G,Genset),
	extractAllTaskNodes(Genset,Gen),
	findall(cht(T,Tp),(member(T,F),member(Tp,Gen)),POAux),
	union(Acc,POAux,AccM),
	collectPO_ifany(Ms,G,F,AccM,PO).

collectPO_ifany([M|Ms],G,F,Acc,PO):-
	collectPO_ifany(Ms,G,F,Acc,PO).






%*************************Debug Write Info***********************************************

debugWrite(Mp,Sp,P,Q):-
	debug_mode(true),
	nl,write('Distant Branch: '), write(Mp),nl,
	write('Atoms: '),write(Sp),nl,
	atoms_nodes(Sp),nl,
	write('...........................'),nl,!.

debugWrite(_Mp,_Sp,_P,_Q).


debugWriteInfoParBranch(Successors, ParResSet):-
	debug_mode(true),
	write('Sucessors: '),
	findall(X,(member((_A,X),Successors),write(X),write(' ')),_LX),
	nl,
	write('Parallel Atoms :'), write(ParResSet),nl,
	atoms_nodes(ParResSet),nl,nl,!.

debugWriteInfoParBranch(_Successors, _ParResSet).

debugWriteInfo(P,Q,R,QEntry,QExit):-
	debug_mode(true),
	(R=rULMACCE_SELFO_start;R=rULMACCE_SELFO_cleanup),
	nl,write('...........................'),nl,
	write(P), write('->'), write(Q),
	write(' | Node: '),write(R),
	write('   | Entry :'),write(QEntry),
	%atoms_nodes(QEntry),nl,
	write(' | Exit :'),write(QExit),nl,
	atoms_nodes(QExit),nl,!.

debugWriteInfo(_P,_Q,_R,_QEntry,_QExit).	

storeCHTInfoInFile(FileHandler,PO):-
	forall(member(cht(T,Tp),PO),(
	write(FileHandler,'cht_lessthan('),write(FileHandler,T),write(FileHandler,','),write(FileHandler,Tp),write(FileHandler,').'),
	nl(FileHandler)
	)).
