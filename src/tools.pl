:- module(tools,[drawGraphFromFile/1,
	drawGraphWithID/1,
	mhpQuery/2,
	mhpQuery/1,
	showStatistics/0,
	saveStatistics/1,
	drawMarkedMHPGraph/2,
	drawMarkedCHTGraph/1]).

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
	exists_file(F),
	getFileExtension(F,Ext),	
	member(Ext,['.pl']),!,
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


drawGraphWithID(ID):-
	use_module('src/autogen/graph.pl',[graphLoc/2,graphInfo/2]),	
	findall((L,ID),graphLoc(ID,L),Graphs),
	forall(member((L,ID),Graphs),(
	   mhpDir(MhpDir),
	   getTextualFileName(L,FNameWithoutExt),
	   atom_concat(MhpDir,'outputs/',Dir),	
	   atom_concats([Dir,FNameWithoutExt,'_',ID,'.dot'],DotFile),	
	   atom_concat('graph_',OwnName,FNameWithoutExt),	
	   atom_concats([MhpDir,'src/autogen/graphExtended_',OwnName,'.pl'],ExtendedGFile),	
	   write("Generating dot file: "),write(DotFile),nl,	
	   check_or_create_dir(Dir),	
	   open(DotFile,write,Os),
	   write(Os,'digraph G {'),
	   nl(Os),	
	   write(Os,'compound=true;'),	
	   nl(Os),
	   use_module(L,[node/3,edge/3,graphs/1]),	
	   use_module(ExtendedGFile,[arc/3]),	
	   drawGraph(Os,ID),nl(Os),
	   nl(Os),
	   write(Os,'}'),
	   close(Os),
	   abolish(node/3),	
	   abolish(edge/3),
	   abolish(arc/3),	
	   abolish(graphs/1),
	   
	   atom_concats(['dot -Tpdf outputs/graph_',OwnName,'_',ID,'.dot',' -o outputs/graph_',OwnName,'_',ID,'.pdf'],Command1),
	   atom_concats(['xdg-open outputs/graph_',OwnName,'_',ID,'.pdf'],Command2),
           shell(Command1,_S1),
	   shell(Command2,_S2)
	)),
	abolish(graphInfo/2),	
	abolish(graphLoc/2).	
	

drawMarkedMHPGraph(N,F):-
	getTextualFileName(F,FNameWithoutExt),	
	mhpDir(MhpDir),
	atom_concats([MhpDir,'outputs/mhp_',FNameWithoutExt,'.dot'],File),
	open(File,write,Os),
	write(Os,'digraph G {'),	


	mhpDir(MhpDir),
	atom_concats([MhpDir,'src/autogen/graph_',FNameWithoutExt,'.pl'],File1),	
	atom_concats([MhpDir,'src/autogen/graphExtended_',FNameWithoutExt,'.pl'],File2),	
	exists_file(File1),!,
	use_module(File1,[node/3,edge/3]),
	use_module(File2,[nd/3,arc/3]),
	node(N,_,G),

	mhpQuery(N,F,MHPList),
	drawGraph(Os,G,N,MHPList),
	nl(Os),
	write(Os,'}'),
	
	abolish(node/3),
	abolish(nd/3),
	abolish(edge/3),
	abolish(arc/3),
	close(Os),

	atom_concats(['dot -Tpdf outputs/mhp_',FNameWithoutExt,'.dot',' -o outputs/mhp_',FNameWithoutExt,'.pdf'],Command1),
	atom_concats(['xdg-open outputs/mhp_',FNameWithoutExt,'.pdf'],Command2),
        shell(Command1,_S1),
	shell(Command2,_S2),!.




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


mhpQuery(Task,F):-
	getTextualFileName(F,FNameWithoutExt),	
	mhpDir(MhpDir),
	atom_concats([MhpDir,'src/autogen/mhp_',FNameWithoutExt,'.pl'],File1),	
	exists_file(File1),!,
	use_module(File1,[mhp/2]),
	findall(Q,((mhp(Task,Q);mhp(Q,Task))),MHPList),
	subtract(MHPList,[dummyTask],MHP),
	length(MHP,L),
	% print result
	write('The following taska are running in parallel with '),write(Task),write(':'),nl,nl,
	forall(member(M,MHP),(write(M), nl)),nl,
        write('Total number of tasks running in parallel with '), write(Task),write(': '),write(L),nl,
	abolish(mhp/2).


mhpQuery(Task,F,MHP):-
	getTextualFileName(F,FNameWithoutExt),	
	mhpDir(MhpDir),
	atom_concats([MhpDir,'src/autogen/mhp_',FNameWithoutExt,'.pl'],File1),	
	exists_file(File1),!,
	use_module(File1,[mhp/2]),
	findall(Q,((mhp(Task,Q);mhp(Q,Task))),MHPList),
	subtract(MHPList,[dummyTask],MHP),
	abolish(mhp/2).



mhpQuery(F):-
	getTextualFileName(F,FNameWithoutExt),	
	mhpDir(MhpDir),
	atom_concats([MhpDir,'src/autogen/mhp_',FNameWithoutExt,'.pl'],File1),	
	exists_file(File1),!,
	use_module(File1,[mhp/2]),
	findall((P,Q),mhp(P,Q),MHPList),
	subtract(MHPList,[dummyTask],MHP),
	abolish(mhp/2),
	remove_duplicates(MHP,[],MHP_noDuplicate),
	length(MHP_noDuplicate,L),
	write("Total no of mhp-pairs: "),write(L),nl,

	atom_concats([MhpDir,'src/autogen/graph_',FNameWithoutExt,'.pl'],File2),	
	atom_concats([MhpDir,'src/autogen/graphExtended_',FNameWithoutExt,'.pl'],File3),	
	exists_file(File2),
	exists_file(File3),
	use_module(File2,[node/3]),
	use_module(File3,[nd/3]),
	findall(N,(node(N,_Type,_G),\+ nd(N,_,_Gp)),Nodes),
	abolish(node/3),
	abolish(nd/3),
	length(Nodes,L1),
	write("Total no of tasks: "),write(L1),nl,nl.






remove_duplicates([],Res,Res).
remove_duplicates([(P,Q)|Mhp],Acc,Res):-
	(\+ member((P,Q),Acc); \+ member((Q,P),Acc)),
	union(Acc,[(P,Q)],Accp),!,
	remove_duplicates(Mhp,Accp,Res).

remove_duplicates([_|Mhp],Acc,Res):-
	remove_duplicates(Mhp,Acc,Res).
	

chtQuery(Task,CHT):-
	findall(Q,(cht_lessthan(Q,Task)),CHT).

        
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