:- module(analXMLStructure,[compareXMLs/2,parseXMLStruct/1]).
:- use_module(library(sgml)).





parseXMLStruct(File1):-
     load_xml_file(File1,XML1),
     processXML(XML1),
     write("Parsing finished successfully"),nl.	


processXML([]).
processXML([element(class:class,A,B)]):-
	processXML(A),
	processXML(B),!.

processXML([element(class:include,A,B)|As]):-
	processXML(A),
	processXML(B),
	processXML(As),!.
    
processXML([element(class:interface,A,B)|As]):-
	processXML(A),
	processXML(B),
	processXML(As),!.

processXML([_|As]):-
	processXML(As),!.


compareXMLs(File1,File2):-
      load_xml_file(File1,XML1),
      load_xml_file(File2,XML2),	
      open('src/autogen/comparexmls.pl',write,OS),
      write("Compare two XMLs..."),nl,	
      compareXs(XML1,XML2,OS),	
      close(OS).

compareXs([],[],_).
compareXs([],Y,OS):-
	write(OS,"XML: 1------->"), nl(OS),
	write(OS,"XML: 2------->"), nl(OS),
	write(OS,Y),nl(OS),!.

compareXs(X,[],OS):-
	write(OS,"XML: 1------->"), nl(OS),
	write(OS,X),nl(OS),
	write(OS,"XML: 2------->"), nl(OS),!.

compareXs([X|Xs],[Y|Ys],OS):-
	write(X),nl,write(Y),nl,
	write("-------------------------------"),nl,
	compareAux(X,Y,OS),
	write(OS,"XML: 1------->"), nl(OS),
	write(OS,X),nl(OS),
	write(OS,"XML: 2------->"), nl(OS),
	write(OS,Y),nl(OS),
	compareXs(Xs,Ys,OS).


compareAux(X,Y,OS):-
	X=element(A1,B1,C1),
	Y=element(A2,B2,C2),
	compareXs(B1,B2,OS),
	compareXs(C1,C2,OS).

compareAux(_X,_Y,_).