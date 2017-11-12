:- module(buildRacerScript,[build_script/0]).
:- use_module(config/config).
:- use_module(autogen/taskSpec).
:- use_module(autogen/mhp).



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
	 write_test_command(MhpList,LLVM,OS),
	 atom_concat('chmod +x ',RaceChecker,Command),
	 shell(Command).
     

write_test_command([],_LLVM,_OS).
write_test_command([(P,Q)|Ms],LLVM,OS):-
	taskspec(P,F1,M1),
	taskspec(Q,F2,M2),
	F1\=file_not_found,
	F2\=file_not_found,!,
	write_command(LLVM,OS,F1,M1,F2,M2),
	write_test_command(Ms,LLVM,OS).

write_test_command([(_P,_Q)|Ms],LLVM,OS):-
	write_test_command(Ms,LLVM,OS).

write_command(LLVM,OS,F1,M1,F2,M2):-
	write(OS, '#echo "Race analysis between'),
	write(OS,F1),
	write(OS,' and '),
	write(OS,F2),
	write(OS,'"'),
	nl(OS),
	atom_concat(LLVM,'/racer -ra ',Race),
	atom_concat('#',Race,Racer),
	write(OS,Racer),
	write(OS,F1),
	write(OS, ' '),
	write(OS,F2),
	write(OS, ' -m1 '),
	write(OS,M1),
	write(OS, ' -m2 '),
	write(OS,M2),
	write(OS, ' 2>/dev/null'),
	nl(OS),nl(OS).


check_or_create_dir(D):-
	exists_directory(D),!.

check_or_create_dir(D):- make_directory(D).