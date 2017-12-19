:- module(tools,[validateall/0,printTreeinDot/0, file_contains_func/2,build_all_task_spec/0,mhpQuery/2,showStatistics/0,saveStatistics/1,drawMarkedMHPGraph/1,drawMarkedCHTGraph/1]).
:- use_module(autogen/graph).
:- use_module(autogen/graphExtended).
:- use_module(autogen/buildpath).
:- use_module(config/config).
:- use_module(autogen/mhp).
:- use_module(library(http/js_write)).

validateall:-
    validateTreeLogic.
    %validateNodePath.
    
build_all_task_spec:-
      mhpDir(MhpDir),
      atom_concat(MhpDir,'src/autogen/taskSpec.pl',File),	
      open(File,write,OS),
      write(OS,':-module(taskSpec,[taskspec/3]).'),	
      nl(OS),
      write_task_spec(OS),	
      close(OS).	



write_task_spec(OS):-
	findall((Node,Func,Path),(func(Node,PNode,Func),path(PNode,Path)),TaskInfo),
      	build_task_info_and_write(OS,TaskInfo).

build_task_info_and_write(_OS,[]).

build_task_info_and_write(OS,[(Node,Func,Path)|Ts]):-
	directory_files(Path,FileList),
	filter_only_c_file(FileList,CFileList),
	find_file_containing_func(CFileList,Path,Func,File),
	write(OS,'taskspec('),	
	term_to_atom(File,FileA),
	write(OS,Node), write(OS,','),
	write(OS,FileA), write(OS,',"'),
  	write(OS,Func), write(OS,'").'), nl(OS),
	build_task_info_and_write(OS,Ts).


filter_only_c_file([],[]).
filter_only_c_file([F|Fs],[F|Rs]):-
      atom_concat(_Pre,'.c',F),!,
      filter_only_c_file(Fs,Rs).	

filter_only_c_file([_F|Fs],Rs):-	
      filter_only_c_file(Fs,Rs).

find_file_containing_func([],_P,_Func,'file_not_found').
find_file_containing_func([F|_Fs],Path,Func,CompleteFileLoc):-
	atom_concat(Path,F,CompleteFileLoc),
      	file_contains_func(CompleteFileLoc,Func),!.

find_file_containing_func([_F|Fs],Path,Func,R):-
      	find_file_containing_func(Fs,Path,Func,R).

% file_contains_func(+FileDesc, +Func)
% Checks if FileDesc contains Func

file_contains_func(File, Func) :-
   open(File, read, _, [alias(input)]),
   atom_length(Func,L),	
   read_each_line(Func,L),!, close(input).

file_contains_func(_File, _Func) :-close(input),fail.


read_each_line(Func,L):-
    read_line_to_string(input, String), 
    String \= end_of_file,
    atom_string(Line,String),
    sub_atom(Line,_B,L,_X,Func),!.	

read_each_line(Func,L):-
    read_line_to_string(input, String), 
    String \= end_of_file,
    !,read_each_line(Func,L).


printTreeinDot:-
    %P=rstartActivity,
    mhpDir(MhpDir),
    atom_concat(MhpDir,'test/outputs/treeoutput1.dot',File1),	
    atom_concat(MhpDir,'test/outputs/treeoutput2.dot',File2),	
    open(File1,write,Os1),
    open(File2,write,Os2),
    write(Os1,'digraph G {'),
    write(Os2,'digraph G {'),
    %printTree([P],[],Os1,1),
    %printTree([P],[],Os2,2),
    drawGraph(Os1,1),
    drawGraph(Os2,2),	
    nl(Os1),
    nl(Os2),
    write(Os1,'}'),
    write(Os2,'}'),
    close(Os1),
    close(Os2).


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
    drawGraphDetails(L,Os).    
    
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

drawMarkedMHPGraphDetails([(P,Q)|Rs],Os):-
	drawMarkedMHPGraphDetails(Rs,Os).


drawGraphDetails([],_Os).
drawGraphDetails([(P,Q)|Rs],Os):-
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
    
printTreeOutput(_P,[],_Os).
printTreeOutput(P,[C|Cs],Os):-
    nl(Os),
    write(Os,P),
    write(Os,' -> '),
    write(Os,C),
    write(Os,';'),
    printTreeOutput(P,Cs,Os).

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