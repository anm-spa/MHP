:- module(helper,[check_or_create_dir/1,getTextualFileName/2,atom_concats/2,filter_only_c_file/2,filterFilesWithExtensions/3,atom_prefix_pattern/4,getFileExtension/2]).

check_or_create_dir(D):-
	exists_directory(D),!.

check_or_create_dir(D):- make_directory(D).



% get the name of the file without extension and directory path
getTextualFileName(InputXML,FileName):-
	absolute_file_name(InputXML,L), 
	file_directory_name(L,D), 
	atom_concat(D,'/',Dir),
	atom_concat(Dir,F,L),
	member(Ext,['.pl','.dive','']),
	atom_concat(FileName,Ext,F),!.

getFileExtension(File,Ext):-
	absolute_file_name(File,L), 
	file_directory_name(L,D), 
	atom_concat(D,'/',Dir),
	atom_concat(Dir,F,L),
	member(Ext,['.pl','.dive','']),
	atom_concat(_FileName,Ext,F),!.

atom_concats([],'').
atom_concats([U],U).
atom_concats([A,B|Rs],UAtom):-
	atom_concat(A,B,U),
	atom_concats([U|Rs],UAtom).



filterFilesWithExtensions([],_Ext,[]).
filterFilesWithExtensions([F|Fs],Ext,[F|Rs]):-
	atom_concat('.',Ext,DotExt),
	atom_concat(_Pre,DotExt,F),!,
	filterFilesWithExtensions(Fs,Ext,Rs).

filterFilesWithExtensions([_F|Fs],Ext,Rs):-	
      filterFilesWithExtensions(Fs,Ext,Rs).


filter_only_c_file(Fs,Rs):-filterFilesWithExtensions(Fs,'c',Rs). 

%% filter_only_c_file([],[]).
%% filter_only_c_file([F|Fs],[F|Rs]):-
%%       atom_concat(_Pre,'.c',F),!,
%%       filter_only_c_file(Fs,Rs).	

%% filter_only_c_file([_F|Fs],Rs):-	
%%       filter_only_c_file(Fs,Rs).





%%% + A: An atom to be splitted, +P: pattern is 'cXd' where c and d are valid characters and X is a Var, -M: first part of A, -R: last part of A 
atom_prefix_pattern(A,P,M,R):-
	sub_atom(P,0,1,N,First),
	atom_concat(First,Rest,A),
	sub_atom(P,N,1,_,Last),
	
	atom_chars(Rest,RList),
	split_atom(RList,Last,'',M,R).

split_atom([],_,_Acc,'','').

split_atom([C|Cs],D,M,M,R):-
	C=D,!,
	atom_chars(R1,Cs),
	atom_concat(C,R1,R).
	
split_atom([C|Cs],D,Acc,M,R):-
	\+ C=D,
	atom_concat(Acc,C,Mp),
	split_atom(Cs,D,Mp,M,R).