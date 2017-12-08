:- module(dataflowAnalysis,[main/0]).
:- use_module(autogen/xmltreelogic).
:- use_module(config/config).
:- use_module(library(lists)).

:- dynamic sync/2.
:- dynamic atom/2.
:- dynamic df/2.
:- dynamic joinArity/2.
:- dynamic barriers/2.

:- style_check(-singleton).
:- style_check(-discontiguous).


debug_mode(true).

main:-
	graphs(GidList),
	mhpDir(MhpDir),
	atom_concat(MhpDir,'src/autogen/mhp.pl',File),
	open(File,write,Os),
	write(Os,':-module(mhp,[mhp/2]).'),nl(Os),nl(Os),
	retractall(atom(_Node,_Atom)),
	retractall(df(_N,_DF)),
	retractall(joinArity(_Nd,_Arity)),	
	retractall(barriers(_NN,_BB)),	
	nb_setval(taskcounter,1),
	nb_setval(barriercounter,1),

	main_aux(GidList,Os,0,PairNo),
	(PairNo=0->
	(write(Os,'mhp('),
	write(Os,'Dummy1'),
	write(Os,','),
	write(Os,'Dummy2'),
	write(Os,').')
	);true),
	close(Os).

main_aux([],_,P,P).	
main_aux([G|Gs],Os,C,P):-
    edge(_N,_N1,G),!,   % check if an edge in graph G exists	
    mhpmain(G,Os,C,C1),
    main_aux(Gs,Os,C1,P).

main_aux([_G|Gs],Os,C,P):-
    main_aux(Gs,Os,C,P).

mhpmain(G,Os,Count,Countp):-

    createRelevantBarrierListForEachNode(G),
    findall(B,((node(B,class:barrier,G);node(B,class:decision,G)),sync(_,B)),BList),
    findOrCreateBarrierAtom(BList,BAtomList),

    forall(sync(N1,B1),(write(N1),write('-->'),write(B1),nl)),	

    findall((N,L),((node(N,class:join,G);node(N,class:multijoin,G)),findall(E,(edge(E,N,G),edge(_,E,G)),EList),length(EList,L),L>0),EffectiveJoinPred),   
    forall(member((N,L),EffectiveJoinPred),assertz(joinArity(N,L))),

    selectGraph(G,Nodes,Edges),
    length(Nodes,NodeCount),

    init_DataFlowSets(Nodes,FlowSets),      % FlowSets is the list of entry and exit sets of each node (n,Entry,Exit)
    initWorklist(WL,G),
    forwardDataFlowAnalysis(WL,G),
    findall((P,Entry),(df(P,Entry),node(P,class:exec,G)),FlowSetsUp),
 
    obtain_concurrentTaskList(FlowSetsUp,ConcurrentTaskList,G),
    selectMHPNodes(ConcurrentTaskList,G,MHPList),
    refineMHPPair(MHPList,[],G,MHPTasks),	
    debugWriteInfo(MHPTasks,Os,true,Count,Countp).


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
	C1 is Count+1,
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
	(node(Q,class:join,GraphNo);node(Q,class:multijoin,GraphNo)).


go_checkpoint.

db_checkpoint(P,Q):-
	(P=rbarrier2;Q=rbarrier2),
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
	    updateWorklistAndDataflowSet(EdgeList,QExitReduced,Ws,WsAux,GraphNo)
	);WsAux=Ws),    

	debugWriteInfo(P,Q,QEntry,QExit),
  
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
		    debugWriteInfoParBranch(Successors, ParResSet),
    		    updateWorklistAndDataflowSet(Successors,ParResSet,WsAux, WsUp,GraphNo)
    		);
    		(WsUp=WsAux)	
	    ),
	    
						%Algorithm 1: line 16-24

	    (\+joinNode(Q,GraphNo)->
		
		(
		    extractParAndSelfAtoms(QExit,ParAtoms,SelfAtoms),
		    getDistantSymmetricParallelNodesAndDF(ParAtoms,SelfAtoms,P,Q,SymBranch),
		    updateWLAndDF(SymBranch,GraphNo,WsUp,WMod)
		);
		WMod=WsUp
	    ),
	    forwardDataFlowAnalysis(WMod,GraphNo).   



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
	debugWrite(Mp,Sp,P,Q),
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

debugWriteInfo(P,Q,QEntry,QExit):-
	debug_mode(true),
	nl,write('...........................'),nl,
	write('Branch '),write(P), write(' to '), write(Q),nl,
	write('Entry :'),write(QEntry),nl,
	atoms_nodes(QEntry),nl,
	write('Exit :'),write(QExit),nl,
	atoms_nodes(QExit),nl,!.

debugWriteInfo(_P,_Q,_QEntry,_QExit).	

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


updateWLAndDF([],_GraphNo,W,W).
updateWLAndDF([(Mp,A)|Res],GraphNo,W,WR):-
	df(Mp,MEntry),
	union(MEntry,A,NewMEntry),
	
	retractall(df(Mp,MEntry)),
	assertz(df(Mp,NewMEntry)),
	findall((R,Mp),edge(R,Mp,GraphNo),Edges),
	union(W,Edges,WAux),
	updateWLAndDF(Res,GraphNo,WAux,WR).
	


update_atoms_for_decision_node(P,Q,PExit,QEntry):-
	node(P,class:decision,_),
	atom(P,Patom),
        sync(Q,B),
	atom(B,Batom),!,
	findall(Z,(member(X,PExit),((atom_concat(Y,Patom,X),atom_concat(Y,Batom,Z));(Z=X,\+ atom_concat(_,Patom,X)))),QEntry).

update_atoms_for_decision_node(P,Q,PExit,PExit).

updateWorklistAndDataflowSet([],_ResSet,W,W,_).

updateWorklistAndDataflowSet([(P,Q)|Es],ResSet,W,WsUp,GraphNo):-
	df(Q,QEntry),
	update_atoms_for_decision_node(P,Q,ResSet,ResSetMod),
	\+ subset(ResSetMod,QEntry),!,
	union(QEntry,ResSetMod,NewQEntry),
%	findall(X,(member(X,NewQEntryAux),atom_concat('p',Y,X),member(Y,NewQEntryAux)),RedundantAtoms),
%	subtract(NewQEntryAux,RedundantAtoms,NewQEntry),	
	retractall(df(Q,QEntry)),
	assertz(df(Q,NewQEntry)),
	union(W,[(P,Q)],WAux),
	updateWorklistAndDataflowSet(Es,ResSet,WAux,WsUp,GraphNo).

updateWorklistAndDataflowSet([_|Es],ResSet,W,WsUp,GraphNo):-
	updateWorklistAndDataflowSet(Es,ResSet,W,WsUp,GraphNo).


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


applyTransferFunction(P,GraphNo,Entry,Exit):-
    gen(P,GraphNo,GenSet),
    kill(P,Entry,KillSet),
    subtract(Entry,KillSet,EntryAux),
    union(EntryAux,GenSet,Exit).
%    findall(X,(member(X,ExitAux),atom_concat('p',Y,X),member(Y,ExitAux)),RedundantAtoms),
%    subtract(ExitAux,RedundantAtoms,Exit).	


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

%% findForkBarriersComb(GraphNo,ForkBarrierList):-
%%     findall(B,node(B,class:barrier,GraphNo),BList),
%%     findall((P,Q),(edge(P,Q,GraphNo),member(Q,BList)),WList),
%%     searchBackward(WList,[],ForkBarrierList,GraphNo).

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

%% searchBackward([(P,Q)|Rs],Acc,ForkBarrierList,GraphNo):-
%%     node(P,T,GraphNo),
%%     T=class:decision,!,
%%     append(Acc,[(P,Q)],AccAux),                       % Decision node usually have two branches, and may synchronize with different barriers in different branches
%%     findall((R,P),edge(R,P,GraphNo),BList),             % its treated as a placeholder for sucessive barrier
%%     append(BList,Rs,RsAux),                                    % A change is being made here     
%%     searchBackward(RsAux,AccAux,ForkBarrierList,GraphNo).


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

kill(P,Entry,[]):-
    node(P,A,GraphNo),
    member(A,[class:exec,class:decision,class:label,class:fork,class:multifork,class:join,class:multijoin,class:nop,class:return]),!.


kill(P,Entry,KillSet):-
    node(P,class:barrier,GraphNo),
    (barriers(P,BList)-> Barriers=BList;	
    (
	getAllBarriersWithIndirectSync([P], GraphNo,[],Barriers),
        assertz(barriers(P,Barriers))
     )),
    getAllKillSet(Barriers,Entry,KillSet).	

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


selectGraph(N,Nodes,Edges):-
	findall((P,Q),edge(P,Q,N),MEdges),
	findall(P,node(P,_,N),MNodes),
	list_to_set(MEdges,Edges),
	list_to_set(MNodes,Nodes).

init_DataFlowSets([],[]).
init_DataFlowSets([N|Ns],[(N,[])|FlowSets]):-
    assertz(df(N,[])),
    init_DataFlowSets(Ns,FlowSets).

initWorklist(WL,N):-                    %%Need to modify the initialization of the worklist
    findall(P,(node(P,_,N),\+ edge(_X,P,N),P=rstartActivity),List),
    findall((P,Q),(member(P,List),edge(P,Q,N)),WL).
   

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
