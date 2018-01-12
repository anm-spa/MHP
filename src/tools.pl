:- module(tools,[drawGraphFromFile/1,mhpQuery/2,showStatistics/0,saveStatistics/1,drawMarkedMHPGraph/1,drawMarkedCHTGraph/1]).
:- use_module(autogen/buildpath).
:- use_module(config/config).
%:- use_module(autogen/mhp).
:- use_module(src/helper).


% +F is the name of the graph like F='/path-to-dive-file' if something.dive is parsed successfully   
drawGraphFromFile(F):-
	getTextualFileName(F,FNameWithoutExt),	
	mhpDir(MhpDir),
	atom_concats([MhpDir,'src/autogen/graph_',FNameWithoutExt,'.pl'],File1),	
	exists_file(File1),!,
	drawGraphFromFileAux(File1),
	write('graph is already written as dot file'),nl,
	atom_concats(['dot -Tpdf outputs/graph_',FNameWithoutExt,'.dot',' -o outputs/graph_',FNameWithoutExt,'.pdf'],Command1),
	atom_concats(['xdg-open outputs/graph_',FNameWithoutExt,'.pdf'],Command2),
        shell(Command1,_S1),
	shell(Command2,_S2),!.


% +F is the absolute graph filename in prolog format 
drawGraphFromFile(F):-
	exists_file(F),!,
	drawGraphFromFileAux(F).

% +F is the name of the graph like F='something' if something.dive is parsed successfully   
drawGraphFromFile(F):-
	mhpDir(MhpDir),
	atom_concats([MhpDir,'src/autogen/graph_',F,'.pl'],File1),	
	exists_file(File1),!,
	drawGraphFromFileAux(File1).

% +F is the name of the graph like F='graph_graphName.pl' that exists in src/autogen directory  
drawGraphFromFile(F):-
	mhpDir(MhpDir),
	atom_concats([MhpDir,'src/autogen/',F],File1),	
	exists_file(File1),!,
	drawGraphFromFileAux(File1).

% +F is the name of the graph like F='src/autogen/graph_graphName.pl' that exists  
drawGraphFromFile(F):-
	mhpDir(MhpDir),
	atom_concats([MhpDir,F],File1),	
	exists_file(File1),!,
	drawGraphFromFileAux(File1).

drawGraphFromFileAux(F):-
    mhpDir(MhpDir),
    getTextualFileName(F,FNameWithoutExt),	
    atom_concat(MhpDir,'outputs/',Dir),	
    atom_concats([Dir,FNameWithoutExt,'.dot'],DotFile),	
    atom_concat('graph_',OwnName,FNameWithoutExt),	
    atom_concats([MhpDir,'src/autogen/graphExtended_',OwnName,'.pl'],ExtendedGFile),	
    write("Generating dot file: "),write(DotFile),nl,	
    check_or_create_dir(Dir),	

    open(DotFile,write,Os),
    write(Os,'digraph G {'),
    nl(Os),	
    write(Os,'compound=true;'),	
    nl(Os),

    use_module(F,[node/3,edge/3,graphs/1]),	
    use_module(ExtendedGFile,[arc/3]),	
    use_module('src/autogen/graph.pl',[graphInfo/2]),	
    graphs(Gs),	
    forall(member(G,Gs),	
    (drawGraph(Os,G),nl(Os))
    ),
    nl(Os),
    write(Os,'}'),
    abolish(node/3),	
    abolish(edge/3),
    abolish(arc/3),	
    abolish(graphs/1),	
    abolish(graphInfo/2),	
    close(Os).	


drawMarkedMHPGraph(N):-
	mhpDir(MhpDir),
	atom_concat(MhpDir,'test/outputs/',Temp1),	
    	atom_concat(Temp1,'mhpGraph',Temp2),
	atom_concat(Temp2,'.dot',File),
	open(File,write,Os),
	write(Os,'digraph G {'),
	node(N,_,G),
	mhpQuery(N,MHPList),
	drawGraph(Os,G,N,MHPList),
	nl(Os),
	write(Os,'}'),
	close(Os).


drawMarkedCHTGraph(N):-
	use_module(src/autogen/cht),
	mhpDir(MhpDir),
	atom_concat(MhpDir,'test/outputs/',Temp1),	
    	atom_concat(Temp1,'chtGraph',Temp2),
	atom_concat(Temp2,'.dot',File),
	open(File,write,Os),
	write(Os,'digraph G {'),
	node(N,_,G),
	chtQuery(N,CHTList),
	drawCHTGraph(Os,G,N,CHTList),
	nl(Os),
	write(Os,'}'),
	close(Os).

  
drawGraph(Os,G,N,MHP):-
    drawNodes(Os,G,N,MHP),
    findall((P,Q),edge(P,Q,G),L),	
    drawMarkedMHPGraphDetails(L,Os).    


drawCHTGraph(Os,G,N,CHT):-
    drawCHTNodes(Os,G,N,CHT),
    findall((P,Q),edge(P,Q,G),L),	
    drawMarkedMHPGraphDetails(L,Os).    



drawGraph(Os,G):-
    findall((P,Q),edge(P,Q,G),L),
    graphInfo(G,GName),
    atom_concat('gr_',GInfo,GName),	
    atom_concats(['subgraph cluster_',GInfo,'{'],SubGraph),	
    nl(Os),	
    write(Os, SubGraph),nl(Os),	
    drawGraphDetails(L,Os),
    atom_concats(['label="',GInfo,'";'],Label),
    nl(Os),	
    write(Os,Label),nl(Os), write(Os,'}'),nl(Os).	
    
printTree([],_,_Os,_G).
printTree([P|Ps],Pro,Os,G):-
    findall(A,edge(P,A,G),L),
    printTreeOutput(P,L,Os),
    filterNodes(L,[P|Pro],Ls,ProUpdate),  
    union(Ps,Ls,PL),                        %union
    printTree(PL,ProUpdate,Os,G).


filterNodes(L,Pro,Ls,ProUpdate):-
    subtract(L,Pro,Ls),
    subtract(L,Ls,Lp),
    union(Pro,Lp,ProUpdate).      %union


drawNodes(Os,G,N,MHP):-
	findall(Node,(node(Node,_,G)),Nodes),
	findall(Nd,nd(Nd,_,G),Nds),
	subtract(Nodes,Nds,ActiveNodes),
	forall((member(P,ActiveNodes)),(
	%nl(Os),    
	(
	    (node(P,class:barrier,_))->
		(
		    write(Os,P),
		    write(Os,'[shape=rectangle,style=filled,color=green];'),
		    nl(Os)    
		);true
	),

       (
	   (node(P,class:join,_);node(P,class:multijoin,_))->
	       (
		   write(Os,P),
		   write(Os,'[shape=rectangle,style=filled,color=gray];'),
		   nl(Os)    
	       );true
        ),
	(
	    node(P,class:decision,_)->
	    (
		write(Os,P),
		write(Os,'[shape=diamond];'),
		nl(Os)    
	    );true
        ),
	(
	    member(P,MHP)->
	    (write(Os,P),
	    write(Os,'[style=filled,color=blue];'),
	    nl(Os)
	);true
    ),

	(
	    (P=N)->
		(write(Os,P),
		write(Os,'[style=filled,color=royalblue,fontcolor=red];'),
		nl(Os)
	    );true
    )
	)).



drawCHTNodes(Os,G,N,CHT):-
	findall(Node,(node(Node,_,G)),Nodes),
	findall(Nd,nd(Nd,_,G),Nds),
	subtract(Nodes,Nds,ActiveNodes),
	forall((member(P,ActiveNodes)),(
	%nl(Os),    
	(
	    (node(P,class:barrier,_))->
		(
		    write(Os,P),
		    write(Os,'[shape=rectangle,style=filled,color=green];'),
		    nl(Os)    
		);true
	),

       (
	   (node(P,class:join,_);node(P,class:multijoin,_))->
	       (
		   write(Os,P),
		   write(Os,'[shape=rectangle,style=filled,color=gray];'),
		   nl(Os)    
	       );true
        ),
	(
	    node(P,class:decision,_)->
	    (
		write(Os,P),
		write(Os,'[shape=diamond];'),
		nl(Os)    
	    );true
        ),
	(
	    member(P,CHT)->
	    (   
		write(Os,P),
	    write(Os,'[style=filled,color=red];'),
	    nl(Os)
	);true
    ),

	(
	    (P=N)->
		(write(Os,P),
		write(Os,'[style=filled,color=royalblue,fontcolor=red];'),
		nl(Os)
	    );true
    )
	)).



drawMarkedMHPGraphDetails([],_Os).
drawMarkedMHPGraphDetails([(P,Q)|Rs],Os):-
    \+ arc(P,Q,_),!,		
    write(Os,P),
    write(Os,' -> '),
    write(Os,Q),
    write(Os,';'),
    nl(Os),	
    drawMarkedMHPGraphDetails(Rs,Os).

drawMarkedMHPGraphDetails([(_P,_Q)|Rs],Os):-
	drawMarkedMHPGraphDetails(Rs,Os).

drawGraphDetails([],_Os).
drawGraphDetails([(P,Q)|Rs],Os):-
    cond_include_nodes(P,Q),!,			
    nl(Os),    
    (
	(node(P,class:barrier,_))->
	(
            write(Os,P),
	    write(Os,'[shape=rectangle,style=filled,color=green];'),
	    nl(Os)    
	);true

    ),

    (
	(node(P,class:join,_);node(P,class:multijoin,_))->
	(
            write(Os,P),
	    write(Os,'[shape=rectangle,style=filled,color=cyan];'),
	    nl(Os)    
	);true

    ),

    (
	node(P,class:decision,_)->
	(
            write(Os,P),
	    write(Os,'[shape=diamond,style=filled,color=yellow];'),
	    nl(Os)    
	);true

    ),

    (
	\+ edge(_,P,_)->
	(
            write(Os,P),
	    write(Os,'[shape=rectangle,style=filled,color=red];'),
	    nl(Os)    
	);true

    ),
	
    write(Os,P),
    write(Os,' -> '),
    write(Os,Q),
    write(Os,';'),
    drawGraphDetails(Rs,Os).

drawGraphDetails([(_P,_Q)|Rs],Os):-
	drawGraphDetails(Rs,Os).
    
printTreeOutput(_P,[],_Os).
printTreeOutput(P,[C|Cs],Os):-
    nl(Os),
    write(Os,P),
    write(Os,' -> '),
    write(Os,C),
    write(Os,';'),
    printTreeOutput(P,Cs,Os).

cond_include_nodes(P,Q):-
	debug_mode(false),
	\+ arc(P,Q,_).

cond_include_nodes(_P,_Q):-
	debug_mode(true).



validateTreeLogic:-
    findall(A,(edge(A,_,N),\+node(A,_T1,N)),L1),
    findall(B,(edge(_A,B,N),\+node(B,_T,N)),L2),
  %  write(L1),
  %  write(L2),
    L1=[],
    L2=[].

mhpQuery(Task,MHP):-
	findall(Q,((mhp(Task,Q);mhp(Q,Task))),MHPList),
	subtract(MHPList,[dummyTask],MHP).

chtQuery(Task,CHT):-
	findall(Q,(cht_lessthan(Q,Task)),CHT).

mhpQuery(Task):-
	findall(Q,(mhp(Task,Q);mhp(Q,Task)),MHPList),
	subtract(MHPList,[dummyTask],MHP),
	length(MHP,L),
	write('Task '),write(Task), write(' runs in parallel with '),write(L), write(' other tasks'),nl,
	forall(member(M,MHP),(write(M), write(' '))),nl.
        
save_mhpQuery(Task,Os):-
	findall(Q,(mhp(Task,Q);mhp(Q,Task)),MHPList),
	subtract(MHPList,[dummyTask],MHP),
	write(Os,'mhpOf_latest('), write(Os,Task), write(Os,','),write(Os,MHP),write(Os,').'),nl(Os).
	

showStatistics:-
	write('**********************************MHP Statistics************************************'),nl,
	findall(N,node(N,class:exec,_),NodeList),
	forall(member(N,NodeList),mhpQuery(N)).

saveStatistics(F):-
	mhpDir(MhpDir),
	atom_concat(MhpDir,'test/outputs/',Dir),
	atom_concat(Dir,F,FM1),
	atom_concat(FM1,'.pl',FM),
	open(FM,write,Os),
	write(Os,':-module(statistics1,[mhpOf_latest/2]).'),	
	nl(Os),
	findall(N,node(N,class:exec,_),NodeList),
	forall(member(N,NodeList),save_mhpQuery(N,Os)),
	close(Os).


compareTaskListAux([],_T2,[]).
compareTaskListAux([T|Ts],T2,TD1):-
	member(T,T2),!,
	compareTaskListAux(Ts,T2,TD1).

compareTaskListAux([T|Ts],T2,[T|TD]):-
	compareTaskListAux(Ts,T2,TD).

compareTaskList(T1,T2,TD1,TD2):-
	compareTaskListAux(T1,T2,TD1),
	compareTaskListAux(T2,T1,TD2),
	((\+ TD1=[]);(\+ TD2=[])).


saveRegression(TDiffList):-
	open('test/outputs/regression.pl',write,Os),
	write(Os,':-module(regression,[mhpOf/2]).'),	
	nl(Os),
	saveRegressionAux(TDiffList,Os),
	close(Os).

saveRegressionAux([],_Os).
saveRegressionAux([(N,Td1,Td2)|DList],Os):-
	write(Os,'regression('), write(Os,N), write(Os,','),write(Os,Td1), write(Os,','),write(Os,Td2),
	write(Os,').'),nl(Os),
        saveRegressionAux(DList,Os).


compareStatistics(F1,F2):-
	atom_concat('test/outputs/',F1,FM1),
	atom_concat('test/outputs/',F2,FM2),
	use_module(FM1),
	use_module(FM2),
	findall((N,TDiff1,TDiff2),(F1:mhpOf(N,TL),F2:mhpOf_latest(N,TL1),compareTaskList(TL,TL1,TDiff1,TDiff2)),TDiffList),
	saveRegression(TDiffList).