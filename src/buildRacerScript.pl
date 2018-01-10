:- module(buildRacerScript,[build_script/0,build_compile_commands/0,file_contains_func/2,build_all_task_spec/0,build_all_events/0,getEventMap/0,map_events_to_tasks/0]).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(config/config).
:- use_module(autogen/taskSpec).
:- use_module(autogen/mhp).
:- use_module(compile_options).
:- use_module(src/helper).


% It creates a task info containing the source files of each tasks 
% Prerequisite: buildpath contains the buildpath of each parsed graph

build_all_task_spec:-
	use_module(src/autogen/buildpath,[func/3,path/2]),
	mhpDir(MhpDir),
	atom_concat(MhpDir,'src/autogen/taskSpec.pl',File),	
	open(File,write,OS),
	write(OS,':-module(taskSpec,[taskspec/3]).'),	
	nl(OS),
	write_task_spec(OS),	
	abolish(func/3),
	abolish(path/2),
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



build_all_events:-
	use_module(src/autogen/buildpath,[path/2,func/3]),
	
	llvm_bin(LLVM),
	absolute_file_name('src/autogen/eventMap.pl',EventFile),
	open(EventFile,write,EventHandler),

	write(EventHandler,":-module(eventMap,[triggerEvent/2,joinConfigEvent/2,activatorEvent/2,threadEvent/2])."),
	nl(EventHandler),
	close(EventHandler),
	atom_concats([LLVM,'/racer -e ',EventFile,' '],Comm),

	findall((Dirs),(path(_T,Dirs)),DirList),
	list_to_set(DirList,DList),

	findall((D,FList), (member(D,DList),findall(F,(func(T,L,F),(path(T,D);path(L,D))),FList)),DFList),

	run_event_command(Comm,DFList),
	map_events_to_tasks,
	abolish(path/2),
	abolish(func/3).


run_event_command(_,[]).
run_event_command(Comm,[(D,Flist)|DFList]):-
	   directory_files(D,FileList),
	   filter_only_c_file(FileList,CFileList),
	   findall(AbsLoc,(member(C,CFileList),atom_concat(D,C,AbsLoc)),AllCFiles),
	   		    
	   findall(F,(member(X,Flist),atom_concat(' -ef ',X,F)),FuncList),
	   foldl(atom_concat,FuncList,' ',Comm1),

	   findall(F,(member(X,AllCFiles),atom_concat(X,' ',F)),Files),
	   foldl(atom_concat,Files,' ',SourceFiles),
	   atom_concats([Comm,Comm1,SourceFiles,' 2>>errorlog 1>>outlog'],Command),
	   
	   atom_concats(['echo ',Command,' 1>>outlog'],Command1),
	   shell(Command1,_S1),
	   shell(Command,_S),
	   run_event_command(Comm,DFList).
	

build_all_events_Bak:-
	use_module(src/autogen/buildpath,[path/2,func/3]),
	
	findall((Dirs),(path(_T,Dirs)),DirList),
	list_to_set(DirList,DList),

	findall(AllCFiles,( member(D,DList),
	                directory_files(D,FileList),
			filter_only_c_file(FileList,CFileList),
			findall(AbsLoc,(member(C,CFileList),atom_concat(D,C,AbsLoc)),AllCFiles)
		    ),CFiles),

        flatten(CFiles,AllFiles),		    
	llvm_bin(LLVM),
	absolute_file_name('src/autogen/eventMap.pl',EventFile),
	open(EventFile,write,EventHandler),

	write(EventHandler,":-module(eventMap,[triggerEvent/2,joinConfigEvent/2,activatorEvent/2,threadEvent/2])."),
	nl(EventHandler),
	close(EventHandler),
	atom_concats([LLVM,'/racer -e ',EventFile,' '],Comm),
	forall(member(F,AllFiles),
	(
	    atom_concat(Comm,F,Command),
	    atom_concat(Command,' 2>>makelog',Command1),
	    shell(Command1,_S)
	)
	),
	%use_module(src/autogen/eventMap,[triggerEvent/2,joinConfigEvent/2,activatorEvent/2,threadEvent/2]),

	%findall((TFunc,E,G),(triggerEvent(TFunc,E),joinConfigEvent(E,G)),TriggerEvents),

	%findrelevantThreadOrActivatorEvent(TriggerEvents,EventMap),

	%abolish(triggerEvent/2),
	%abolish(joinConfigEvent/2),
	%abolish(activatorEvent/2),
	%abolish(threadEvent/2),
	abolish(path/2),
	abolish(func/3).


map_events_to_tasks:-
	use_module('src/autogen/eventMap',[triggerEvent/2]),
	use_module('src/autogen/buildpath',[func/3]),
	findall((T,E),(triggerEvent(TFunc,E),func(T,_,TFunc)),EventTasksFound),
	findall((TFunc,E),(triggerEvent(TFunc,E),\+ func(_T,_,TFunc)),EventTaskNotFound),
	
	absolute_file_name('src/autogen/events.pl',EventFname),
	open(EventFname,write,EventFdesc),

	write(EventFdesc,":-module(events,[eventFound/4])."),
	nl(EventFdesc),
	

	findall(T,member((T,_E),EventTasksFound),TaskList),
	list_to_set(TaskList,TaskSet),
	use_module('src/autogen/graph',[graphLoc/2]),
	findall(G,graphLoc(_,G),GList),
	list_to_set(GList,Gset),

	getGraphInfoForEachTask(Gset,TaskSet,[],TG,TNG),

	
	forall(member((T,E),EventTasksFound),(
	  convertAppropriateCase(E,Ep),
	  member((T,G,Id),TG),
	  atom_concats(['events(',G,',',Id,',',T,',',Ep,').'],Ev),
	  write(EventFdesc,Ev),
	  nl(EventFdesc)
	)),
	close(EventFdesc),

	forall(member((F,E),EventTaskNotFound),
	(
	    write('Events not found: '),write(E), write(' Func: '),write(F),nl
	)),
	abolish(graphLoc/2),
	abolish(func/3),
	abolish(triggerEvent/2).


getGraphInfoForEachTask([],TaskSet,Acc,Acc,TNG):-
	findall(T,(member(T,TaskSet),\+ member((T,_G,_Id),Acc)),TNG).

getGraphInfoForEachTask([G|Gs],TaskSet,Acc,TG,TNG):-
	use_module(G,[node/3]),
	findall((T,G,Id),(node(T,_,Id),member(T,TaskSet)),TGAux),
	union(Acc,TGAux,TGAcc),
	abolish(node/3),
	getGraphInfoForEachTask(Gs,TaskSet,TGAcc,TG,TNG).



getEventMap:-
	use_module(src/autogen/eventMap,[triggerEvent/2,joinConfigEvent/2,activatorEvent/2,threadEvent/2]),
	use_module(src/autogen/buildpath,[func/3,path/2]),
		
	findall((TFunc,E,G),(triggerEvent(TFunc,E),joinConfigEvent(E,G)),TriggerEvents),
	
	findrelevantThreadOrActivatorEvent(TriggerEvents,EventMap),
	
	abolish(triggerEvent/2),
	abolish(joinConfigEvent/2),
	abolish(activatorEvent/2),
	abolish(threadEvent/2),
	
	findall((Dirs),(path(_T,Dirs)),DirList),
	list_to_set(DirList,DList),
	
	findall((D,TList), (member(D,DList),findall(T,(func(T,L,_),(path(T,D);path(L,D))),TList)),DTList),

	write(DTList),

	forall(member((F,E,G1,G2),EventMap),(
	    convertAppropriateCase(E,Ep),write('Event: '),write(Ep),write('\t\t'),write('Task: '),
	    (func(T,_,F)-> write(T); (write(F),write('(not found)'))),
	    write('\t\t'),write(G1),write('-->'),write(G2),nl)),
	    
	abolish(func/3),
	abolish(path/2).
    


convertAppropriateCase(E,Ep):-
	%term_to_atom(E,A),
	convertCaseAux(E,'',Ep).

convertCaseAux(A,Accu,Ep):-
	(atom_concat("ULMACCE",X,A);atom_concat("DLMACCE",X,A)),!,
	convertCaseAux(X,Accu,Ep).

convertCaseAux(A,Accu,Ep):-
	atom_prefix_pattern(A,'_X_',X,Y),

	atom_length(X,N),
	%sub_atom(X,0,1,_,First),
	%N1 is N-1,
	sub_atom(X,0,N,_K,Rem),
	downcase_atom(Rem,Remlower),
	atom_concats([Accu,Remlower],Accup),!,
	convertCaseAux(Y,Accup,Ep).

convertCaseAux(_X,E,E).

%% convertCaseAux(X,Accu,Ep):-
%% 	atom_length(X,N),
%% 	sub_atom(X,1,1,_,First),
%% 	N1 is N-2,
%% 	sub_atom(X,2,N1,_K,Rem),
%% 	downcase_atom(Rem,Remlower),
%% 	atom_concats([Accu,First,Remlower],Ep).


findrelevantThreadOrActivatorEvent([],[]).

%findrelevantThreadOrActivatorEvent([(F,E,G1)|Rs],EventMap):-
%	(activatorEvent(E,G2);threadEvent(E,G2)),
%	(G1="item_not_found";G2="item_not_found"),!,
%	findrelevantThreadOrActivatorEvent(Rs,EventMap).

findrelevantThreadOrActivatorEvent([(F,E,G1)|Rs],[(F,E,G1,G2)|EventMap]):-
	(activatorEvent(E,G2);threadEvent(E,G2)),!,
	findrelevantThreadOrActivatorEvent(Rs,EventMap).

findrelevantThreadOrActivatorEvent([(F,E,G1)|Rs],[(F,E,G1,'not_found')|EventMap]):-
	findrelevantThreadOrActivatorEvent(Rs,EventMap).

% generate compile_commands.json file including compile commands of all tasks in a activity graph parsed by the MHP tool.
% compile_commands.json file is located in the BB directory.
%Prerequisite: all task specification have to be build 

build_compile_commands:-
	use_module(src/autogen/buildpath,[path/2,func/3]),
	groupPaths,
	abolish(path/2),
	abolish(func/3).

groupPaths:-
	findall((Dirs),(path(_T,Dirs)),DirList),
	list_to_set(DirList,SList),
	group_tasks(SList,DirTaskList,[],_TList),
	get_clang_command(Comm),
	bbDir(BB),
	atom_concat(BB,'/compile_commands.json',JFile),
	open(JFile,write,Fs),
	write(Fs,'['),
        nl(Fs),
	gen_compile_commands_json(DirTaskList,Comm,Fs),
	write(Fs,']'),
	close(Fs).


get_clang_command(Comm):-
	bbDir(BB),
	findall(Dirs,(includeDir(Dir), atom_concat(BB,'/',D),atom_concat(D,Dir,BBDir),atom_concat('-I ',BBDir,Dirs)),DirList),
	flatten_dir_list(DirList,'',IncludeDirs),
	
	findall(Dirs,(includeSelfDir(Dir), atom_concat('-I ',Dir,Dirs)),SelfDirList),
	flatten_dir_list(SelfDirList,'',IncludeDirs1),
	atom_concat(IncludeDirs,IncludeDirs1,IncludeDirs2),
	findall(Dirs,(isystem(Dir), atom_concat('-isystem ',Dir,Dirs)),SysList),
	flatten_dir_list(SysList,'',SysDirs),
	initCommandOP(Init),
	finalCommandOP(Final),
	atom_concat(IncludeDirs2,SysDirs,Comm1),
	atom_concat(Init,Comm1,Comm2),
	atom_concat(Comm2,Final,Comm3),
	clang(Clang),
	atom_concat(Clang,' ',Cl),
	atom_concat(Cl,Comm3,Comm).

flatten_dir_list([],C,C).
flatten_dir_list([D|Ds],C,IncludeDirs):-
	atom_concat(C,D,C1),
	atom_concat(C1,' ',C2),
	flatten_dir_list(Ds,C2,IncludeDirs).
	


check_exists([],_L,[]).
check_exists([M|Ms],L,Rs):-
	member(M,L),!,
	check_exists(Ms,L,Rs).

check_exists([M|Ms],L,[M|Rs]):-
	check_exists(Ms,L,Rs).


group_tasks([],[],R,R).
group_tasks([D|Ds],[(D,SList)|DT],Acc,Rs):-
	findall(T,((path(T,D),taskspec(T,F,_),atom_concat(D,_,F));(func(T,P,_),path(P,D),taskspec(T,F,_),atom_concat(D,_,F))),TList),
	list_to_set(TList,SList),
	append(Acc,SList,Accp),
	group_tasks(Ds,DT,Accp,Rs).

printDirList([]).
printDirList([(D,T)|DT]):-
	write(D),nl,
	write(T),nl,nl,
	printDirList(DT).

gen_compile_commands_json([],_,_Fs).
gen_compile_commands_json([(D,T)|DT],Comm,Fs):-
	write("Creating compilation database for diretory: "),
	write(D),nl,
	
	getTaskFileInfo(T,[],TF),

	% if you want to include all c files in the directory D to be added to the compile_commands.json 
	directory_files(D,FileList),
	filter_only_c_file(FileList,CFileList),
	findall(AbsLoc,(member(C,CFileList),atom_concat(D,C,AbsLoc)),AllCFiles),

	union(TF,AllCFiles,TAll),
	write_commands(Fs,D,Comm,TAll),
	(DT\= [] -> (write(Fs,','));true),
	nl(Fs),
	gen_compile_commands_json(DT,Comm,Fs).

getTaskFileInfo([],Rs,Rs).
getTaskFileInfo([T|Ts],Acc,Rs):-
	taskspec(T,TFile,_),
	\+ member(TFile,Acc),
	append(Acc,[TFile],Accp),!,
	getTaskFileInfo(Ts,Accp,Rs).

getTaskFileInfo([_T|Ts],Acc,Rs):-
	getTaskFileInfo(Ts,Acc,Rs).

write_commands(_Fs,_D,_,[]).
write_commands(Fs,D,Comm,[TFile|Ts]):-
	write(Fs,'{ '),
	write(Fs, '"directory": '),
	write(Fs,'"'),
	write(Fs,D),
	write(Fs,'",'),
	nl(Fs),
	atom_concat(Comm,TFile,Command),
	write(Fs, '"command": '),
	write(Fs,'"'),
	write(Fs,Command),
	write(Fs,'",'),
	nl(Fs),
	write(Fs, '"file": '),
	write(Fs,'"'),
	write(Fs,TFile),
	write(Fs,'"'),
	nl(Fs),
	write(Fs,'}'),
	(Ts\= [] -> (write(Fs,','),nl(Fs));true),
	write_commands(Fs,D,Comm,Ts).


build_script:-
	 mhpDir(MhpDir),
	 atom_concat(MhpDir,'bbTest',Mhp),
	 check_or_create_dir(Mhp),
	 atom_concat(MhpDir,'bbTest/checkRace',RaceChecker),	
	 open(RaceChecker,write,OS),
	 write(OS,'#!/bin/sh'),
	 nl(OS),nl(OS),
	 findall((P,Q),mhp(P,Q),MhpList),
	 llvm_bin(LLVM),
	 write_test_command(MhpList,LLVM,OS,1),
	 atom_concat('chmod +x ',RaceChecker,Command),
	 shell(Command).
     

write_test_command([],_LLVM,_OS,_).
write_test_command([(P,Q)|Ms],LLVM,OS,N):-
	taskspec(P,F1,M1),
	taskspec(Q,F2,M2),
	F1\=file_not_found,
	F2\=file_not_found,!,
	write_command(LLVM,OS,F1,M1,F2,M2,P,Q,N),
	N1 is N+1,
	write_test_command(Ms,LLVM,OS,N1).

write_test_command([(_P,_Q)|Ms],LLVM,OS,N):-
	write_test_command(Ms,LLVM,OS,N).

write_command(LLVM,OS,F1,M1,F2,M2,P,Q,N):-
	write(OS, 'echo "Test '),
	write(OS,N),
	write(OS, ': Race analysis between'),
	write(OS,F1),
	write(OS,' and '),
	write(OS,F2),
	write(OS,'"'),
	nl(OS),
	
	write(OS,'echo "Task1: '),
	write(OS,P),
	write(OS,' Task2: '),
	write(OS,Q),
	write(OS,'"'),	
	nl(OS),

	atom_concat(LLVM,'/racer -ra ',Race),
	atom_concat('',Race,Racer),
	write(OS,Racer),
	write(OS,F1),
	write(OS, ' '),
	write(OS,F2),
	write(OS, ' -m1 '),
	write(OS,M1),
	write(OS, ' -m2 '),
	write(OS,M2),
	bbDir(BB),
	write(OS,' -p '),
	write(OS,BB),
%	write(OS, ' 2>&1'),
	write(OS, ' 2>/dev/null'),
	nl(OS),nl(OS).


%% check_or_create_dir(D):-
%% 	exists_directory(D),!.

%% check_or_create_dir(D):- make_directory(D).




