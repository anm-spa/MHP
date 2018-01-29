:- module(createConfig,[configure_mhp_tool/3,configure_mhp_tool_test_mode/3]).

configure_mhp_tool(BB,LLVM,DMode):-
	open('config/config.pl',write,OS),
        write(OS,':-module(config,[bbDir/1,mhpDir/1,llvm_bin/1,debug_mode/1,force_analysis/1]).'),	
        nl(OS),nl(OS),

	write(OS,':-dynamic debug_mode/1.'),
	nl(OS),
	write(OS,':-dynamic force_analysis/1.'),
	nl(OS),
	
	% BB Dir
	write(OS,'bbDir('),
	term_to_atom(BB,BAtom),
	write(OS,BAtom),
	write(OS,').'),
	nl(OS),
	working_directory(W,W),
	
	%MHP Tool Dir
	write(OS,'mhpDir('),
	term_to_atom(W,WAtom),
	write(OS,WAtom),
	write(OS,').'),
	nl(OS),

	%debug mode
	write(OS,'debug_mode('),
	write(OS,DMode),
	write(OS,').'),
	nl(OS),

	%force_analysis
	write(OS,'force_analysis('),
	write(OS,false),
	write(OS,').'),
	nl(OS),
	
	%LLVM Binary Dir where the racer tool resides
	write(OS,'llvm_bin('),
	term_to_atom(LLVM,LLVMBina),
	write(OS,LLVMBina),
	write(OS,').'),
	nl(OS),
	close(OS).


configure_mhp_tool_test_mode(BB,LLVM,DMode):-
	working_directory(W,W),
	term_to_atom(W,WAtom),
	open('config/config.pl',write,OS),
        write(OS,':-module(config,[bbDir/1,mhpDir/1,llvm_bin/1,debug_mode/1,force_analysis/1]).'),	
        nl(OS),nl(OS),

	write(OS,':-dynamic debug_mode/1.'),
	nl(OS),
	write(OS,':-dynamic force_analysis/1.'),
	nl(OS),
	
	% BB Dir
	write(OS,'bbDir('),
	term_to_atom(BB,BAtom),
	atom_concat(WAtom,BAtom,BBp),
	write(OS,BBp),
	write(OS,').'),
	nl(OS),
	
	%MHP Tool Dir
	write(OS,'mhpDir('),
	write(OS,WAtom),
	write(OS,').'),
	nl(OS),

	%debug mode
	write(OS,'debug_mode('),
	write(OS,DMode),
	write(OS,').'),
	nl(OS),

	%force_analysis
	write(OS,'force_analysis('),
	write(OS,false),
	write(OS,').'),
	nl(OS),
	
	%LLVM Binary Dir where the racer tool resides
	write(OS,'llvm_bin('),
	term_to_atom(LLVM,LLVMBina),
	atom_concat(WAtom,LLVMBina,LLVMBin),
	write(OS,LLVMBin),
	write(OS,').'),
	nl(OS),
	close(OS).
