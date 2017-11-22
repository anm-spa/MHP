:- module(buildRacerScript,[build_script/0,build_compile_commands/0]).
:- use_module(config/config).
:- use_module(autogen/taskSpec).
:- use_module(autogen/mhp).
:- use_module(autogen/buildpath).
:- use_module(compile_options).


build_compile_commands:-
	groupPaths.


groupPaths:-
	findall((Dirs),(path(T,Dirs)),DirList),
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
	write_commands(Fs,D,Comm,TF),
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


check_or_create_dir(D):-
	exists_directory(D),!.

check_or_create_dir(D):- make_directory(D).




