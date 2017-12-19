:- module(helper,[check_or_create_dir/1,getTextualFileName/2,atom_concats/2]).

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
	atom_concat(FileName,Ext,F).


atom_concats([],'').
atom_concats([U],U).
atom_concats([A,B|Rs],UAtom):-
	atom_concat(A,B,U),
	atom_concats([U|Rs],UAtom).