:- module(tools,[validateall/0,printTreeinDot/0, file_contains_func/2,build_all_task_spec/0,mhpQuery/2]).
:- use_module(autogen/xmltreelogic).
:- use_module(autogen/buildpath).
:- use_module(config/config).
:- use_module(autogen/mhp).

validateall:-
    validateTreeLogic.
    %validateNodePath.
    


%% test1:-
%% 	findall(Node,(node(Node,class:exec,_),\+ func(Node,_,_M)),L),
%% 	length(L,N),
%% 	findall(Node,(func(Node,_,_M1),\+ taskspec(Node,_L,_F)),L1),
%% 	length(L1,N1),
%% 	write(N),
%% 	write(L),nl,
%% 	write(N1),
%% 	write(L1).

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
    printTreeAlternative(Os1,1),
    printTreeAlternative(Os2,2),	
    nl(Os1),
    nl(Os2),
    write(Os1,'}'),
    write(Os2,'}'),
    close(Os1),
    close(Os2).

printTreeAlternative(Os,G):-
    findall((P,Q),edge(P,Q,G),L),
    printTreeOutputAlternative(L,Os).    
    
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

printTreeOutputAlternative([],_Os).
printTreeOutputAlternative([(P,Q)|Rs],Os):-
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
    write(Os,P),
    write(Os,' -> '),
    write(Os,Q),
    write(Os,';'),
    printTreeOutputAlternative(Rs,Os).
    
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

%validateNodePath:-
%    findall(A, node(A,_B,_C),L),
%    findall(X, edge(X,_B1,_C1),E).	
 %   length(L,N),length(E,EN),
  %  write("Nodes: "), write(N), write(" Edges: "), write(EN),nl, 		
  %  findall(A, path(A,_B),P),length(P,PN),	
  %  write("Path: "),write(PN),nl,	
  %  findall(A,(node(A,_T,_G),\+ path(A,_)),L1),
  %  findall(A,(node(A,_T1,_G1), path(A,_)),L2),
  %  length(L1,N1),length(L2,N2),	
 %   write("The paths for the following tasks are known: "), write(N2),nl,
 %   write(L2),nl,	
 %   write("The paths for the following tasks are unknown: "),write(N1),nl,
 %   write(L1).	


mhpQuery(Task,MHPList):-
	write('MHP list of '),
	write(Task),nl,
	findall(Q,((mhp(Task,Q);mhp(Q,Task)),write(Q),nl),MHPList),
	length(MHPList,L),
	write('No of Parallel Tasks: '),
	write(L).

mhpAnomalyCheck.
	