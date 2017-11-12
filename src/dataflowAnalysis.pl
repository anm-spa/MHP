:- module(dataflowAnalysis,[main/0]).
:- use_module(autogen/xmltreelogic).
:- use_module(config/config).
:- use_module(library(lists)).

:- dynamic
       sync/2.
:- dynamic atom/2.

:- style_check(-singleton).
:- style_check(-discontiguous).


main:-
	graphs(GidList),
	mhpDir(MhpDir),
	atom_concat(MhpDir,'src/autogen/mhp.pl',File),
	open(File,write,Os),
	write(Os,':-module(mhp,[mhp/2]).'),nl(Os),nl(Os),	
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
    addSyncInfo(G),
    retractall(atom(_Node,_Atom)),
    nb_setval(taskcounter,1),
    nb_setval(barriercounter,1),
    
    selectGraph(G,Nodes,Edges),
    length(Nodes,NodeCount),
    %write("Nodes "), write(NodeCount),nl,
    %length(Edges,EdgeCount),
    %write("Edges "), write(EdgeCount),nl,	
    init_DataFlowSets(Nodes,FlowSets),      % FlowSets is the list of entry and exit sets of each node (n,Entry,Exit)
    initWorklist(WL,G),
    forwardDataFlowAnalysis(WL,FlowSets,FlowSetsUp,G),
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
%    write(Pair),nl,nl,
    union_pairs(Pair,Acc,[],Accp),
    refineMHPPair(Ns,Accp,G,Rs).

%% refineMHPNodes([],[]).
%% refineMHPNodes([(P,L)|Ns],[(P,Ls)|Rs]):-
%%      select(P,L,Ls),
%%      refineMHPNodes(Ns,Rs).

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
    (atom(P,Atom);(atom_concat(A,B,Atom),atom_concat('*',_L,B),atom(P,A))),
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

addSyncInfo(G):-
    findForkBarriersComb(G,NodeWithSync),
    % writeAllPair(NodeWithSync),
    findall((B,run_to_completion),((node(B,class:exec,G);node(B,class:label,G);node(B,class:decision,G)),
				   \+ member((B,_),NodeWithSync)),NodeWithoutSync),
    retractall(sync(_Node,_Barrier)),
    writeToSyncDatabase(NodeWithSync),
    writeToSyncDatabase(NodeWithoutSync).


forwardDataFlowAnalysis([],Fs,Fs,_GraphNo):-!.
forwardDataFlowAnalysis(WL,FlowSets,FlowSetsUp,GraphNo):-
    WL=[(P,Q)|Ws],
    member((P,Entry),FlowSets),      % make this efficient
    member((Q,QEntry),FlowSets),
    applyTransferFunction(P,GraphNo,Entry,Exit),
    (
	\+subset(Exit,QEntry)->
	(
            select((Q,QEntry),FlowSets,FlowSetsp),
	    union(QEntry,Exit,NewQEntry),
	    union(FlowSetsp,[(Q,NewQEntry)],FlowSetsAux),
	    findall((Q,R),edge(Q,R,GraphNo),Edges),
	    union(Ws,Edges,WsAux)
	);(WsAux=Ws,FlowSetsAux=FlowSets)
    ),
    forwardDataFlowAnalysis(WsAux,FlowSetsAux,FlowSetsUp,GraphNo).   

applyTransferFunction(P,GraphNo,Entry,Exit):-
    gen(P,GraphNo,GenSet),
    kill(P,Entry,KillSet),
    subtract(Entry,KillSet,EntryAux),
    union(EntryAux,GenSet,Exit).


combineTaskBarrierAtom(_Atom,[],GenSet,GenSet).
combineTaskBarrierAtom(Atom,[B|Bs],Acc,GenSet):-
    atom_concat(Atom,'*',Atom1),
    atom_concat(Atom1,B,Atom2),
    union(Acc,[Atom2,B],Accp),
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
            atom_concat('b',C,Batom),
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

findForkBarriersComb(GraphNo,ForkBarrierList):-
    findall(B,node(B,class:barrier,GraphNo),BList),
    findall((P,Q),(edge(P,Q,GraphNo),member(Q,BList)),WList),
    searchBackward(WList,[],ForkBarrierList,GraphNo).


searchBackward([],ForkBarrierList,ForkBarrierList,GraphNo).
searchBackward([(P,Q)|Rs],Acc,ForkBarrierList,GraphNo):-
    node(P,T,GraphNo),
    %(T=class:fork; T=class:multifork),!,
    (T=class:exec;T=class:label),!,
    append(Acc,[(P,Q)],AccAux),
    findall((R,Q),edge(R,P,GraphNo),BList),
    append(BList,Rs,RsAux),                                    % A change is being made here     
    searchBackward(RsAux,AccAux,ForkBarrierList,GraphNo).

searchBackward([(P,Q)|Rs],Acc,ForkBarrierList,GraphNo):-
    node(P,T,GraphNo),
    T=class:barrier,!,
    append(Acc,[(P,Q)],AccAux),                       % Barrier synchronizes with another barrier
    searchBackward(Rs,AccAux,ForkBarrierList,GraphNo).

searchBackward([(P,Q)|Rs],Acc,ForkBarrierList,GraphNo):-
    findall((R,Q),edge(R,P,GraphNo),BList),
    append(BList,Rs,RsAux),
    searchBackward(RsAux,Acc,ForkBarrierList,GraphNo).


kill_aux([],_B,KillSet,KillSet).
kill_aux([P|Rs],B,Acc,KillSet):-
    atom_concat(A,B,P),
    atom_concat(Ap,'*',A),!,
    union(Acc,[P,B],Accp),
    kill_aux(Rs,B,Accp,KillSet).

kill_aux([P|Rs],B,Acc,KillSet):-
    kill_aux(Rs,B,Acc,KillSet).

kill(P,Entry,[]):-
    node(P,A,GraphNo),
    member(A,[class:exec,class:decision,class:label,class:fork,class:multifork,class:join,class:multijoin]),!.


kill(P,Entry,KillSet):-
    node(P,class:barrier,GraphNo),
    atom(P,B),
    kill_aux(Entry,B,[],KillSet).


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
    findall(Q,(edge(P,Q,GraphNo)),Succ),  %(node(Q,class:fork,GraphNo);node(Q,class:multifork,GraphNo))
    gen_aux(Succ,GraphNo,[],GenSet).	    
    
gen(P,GraphNo,[]):-                      
    node(P,A,GraphNo),
    member(A,[class:fork,class:multifork,class:join,class:multijoin,class:exec,class:decision]),!.


selectGraph(N,Nodes,Edges):-
	findall((P,Q),edge(P,Q,N),Edges),
	findall(P,node(P,_,N),Nodes).

init_DataFlowSets([],[]).
init_DataFlowSets([N|Ns],[(N,[])|FlowSets]):-
    init_DataFlowSets(Ns,FlowSets).

initWorklist(WL,N):-                    %%Need to modify the initialization of the worklist
    findall(P,(node(P,_,N),\+ edge(_X,P,N)),List),
    findall((P,Q),(member(P,List),edge(P,Q,N)),WL).
   

writeToSyncDatabase([]).
writeToSyncDatabase([(P,B)|Ns]):-
    assertz(sync(P,B)),
    writeToSyncDatabase(Ns).

writeAllPair([]).
writeAllPair([(P,B)|Ns]):-
    write(P),write(" "), write(B),nl,
    writeAllPair(Ns).	
