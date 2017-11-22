:- module(xml2tree,[convertXML/1,parseXML/1]).
:- use_module(library(sgml)).
:- use_module(config/config).

:- dynamic nodepath/2.

%convertXML(+InputXML)
%InputXML is the input dive file in xml format describing high-level design of a parallel application. It generates the following prolog files.
%"xmltreelogic.pl" describes possibly disjoint graphs having the node and edge predicates. 
% Predicate node have the following specification: node(Nodename,NodeClass in XML, GraphId in which Nodename belongs).
% Predicate edge have the following specification: edge(Node1, Node2, GraphId in which this edge belongs).

convertXML(InputXML):-
      load_xml(InputXML,XML,[dialect(xml)]),
      mhpDir(MhpDir),
      atom_concat(MhpDir,'src/autogen',AutoGen),
      check_or_create_dir(AutoGen),
      atom_concat(MhpDir,'src/autogen/buildpath.pl',BuildPath),	
      atom_concat(MhpDir,'src/autogen/xmltreelogic.pl',TreeLogic),	
      atom_concat(MhpDir,'src/autogen/xmltreeInfo.pl',TreeInfo),	
      open(BuildPath,write,OS),
      open(TreeLogic,write,Tr),
      open(TreeInfo,write,Tr1),
      write(Tr,':-module(xmltreelogic,[node/3,edge/3,graphs/1]).'),
      write(OS,':-module(buildpath,[path/2,func/3]).'),
      nl(OS),	
      write(OS,':- discontiguous path/2.'), nl(OS),	 	
      write(OS,':- discontiguous func/3.'),
      nl(Tr),	
      write(Tr,':- discontiguous node/3.'),nl(Tr),	 	
      write(Tr,':- discontiguous edge/3.'),	
      nl(Tr),
      nl(Tr),
      nl(OS),		
      visitMain(XML,GraphIdList,OS,Tr,Tr1),
      write(Tr,'graphs('),
      write(Tr,GraphIdList),
      write(Tr,').'),
      retractall(nodepath(_N,_P)),	
      close(Tr),
      close(Tr1),
      close(OS).

visitMain([element(A,B,C)],GraphIdList,Os,Tr,Tr1):-	
    writeInfo(Tr1,A,B),
    filterNonElement(C,[],Cp),
    nb_setval(barrier,1),
    nb_setval(executableNode,1),
    processMain(Cp,Tr1,Os,1,[],GraphIdList,Tr).

processMain([],_Tr1,_Os,_N,G,G,_Tr).
processMain([X|Xs],Tr1,Os,N,Accum,G,Tr):-
    %write('Processing starts'),nl,
    processInfo(X,Tr1,Xp),
    nb_setval(graph,N),
    %write(Xp),nl,	
    retractall(nodepath(_N,_P)),	
    visitGraph(Xp,Os,Tr,Tr1),
    N1 is N+1,
    append(Accum,[N],Accump),	
    processMain(Xs,Tr1,Os,N1,Accump,G,Tr).

processInfo(element('class:class',B,C),Tr1,Xp):-
    writeInfo(Tr1,'class:class',B),
    filterNonElement(C,[],Cp),
    filterAuxInfo(Cp,Tr1,Xp).

processInfo(element('structure:structure',_B,C),Tr1,Xp):-
    filterNonElement(C,[],Cp),
    filterAuxInfo(Cp,Tr1,Xp).

% visitGraph(+XMLGraph,+FileDesc1,+FileDesc2,+FileDesc3)
% XMLGraph contains list of items: element(A,B,C)
% FileDesc1 is the descriptor of buildpath.pl
% FileDesc2 is the descriptor of xmltreelogic.pl
% FileDesc3 is the descriptor of xmltreeinfo.pl

visitGraph([],_Os,_Tr,_Tr1).
visitGraph([element('class:script',L,C)|Xs],Os,Tr,Tr1):-
    %member(beginAtLabel=A,L),
    writeInfo(Tr1,class:script,L),
    %write(Tr1,C),             %Debug Info
    filterNonElement(C,[],Cp),
    visitList(Cp,[],[],Os,Tr),
    visitGraph(Xs,Os,Tr,Tr1).

visitGraph([X|Xs],_Os,_Tr,_Tr1):-
	write('The following item is not considered'),nl,
	write(X), fail.

visitGraph([X|Xs],Os,Tr,Tr1):-
    visitGraph(Xs,Os,Tr,Tr1).

filterAuxInfo([],_Tr1,[]).

filterAuxInfo([element('class:include',_,C)|Xs],Tr1,Xp):-
	writeInfo(Tr1,'class:include',C),
	filterAuxInfo(Xs,Tr1,Xp),!.

filterAuxInfo([element('class:headerInclude',B,_C)|Xs],Tr1,Xp):-
	writeInfo(Tr1,'class:headerInclude',B),
	filterAuxInfo(Xs,Tr1,Xp),!.

filterAuxInfo([element('class:interface',B,C)|Xs],Tr1,Xp):-
	 filterNonElement(C,[],Cp),
	 append(Cp,Xs,Xupdate),
	 filterAuxInfo(Xupdate,Tr1,Xp),!.

filterAuxInfo([element('class:map',B,C)|Xs],Tr1,Xp):-
	writeInfo(Tr1,'class:map',B),
	 filterAuxInfo(Xs,Tr1,Xp),!.

filterAuxInfo([element('class:port',B,C)|Xs],Tr1,Xp):-
	writeInfo(Tr1,'class:port',B),
	filterAuxInfo(Xs,Tr1,Xp),!.

filterAuxInfo([element('structure:protocol',_B,_C)|Xs],Tr1,Ys):-!,
	filterAuxInfo(Xs,Tr1,Ys).

filterAuxInfo([element('structure:component',_B,_C)|Xs],Tr1,Ys):-
	!,
	filterAuxInfo(Xs,Tr1,Ys).

filterAuxInfo([X|Xs],Tr1,[X|Ys]):-
	 filterAuxInfo(Xs,Tr1,Ys).

filterNonElement([],Acc,Acc).
filterNonElement([X|Xs],Acc,Cp):-
    X=element(A,B,C),!,
    append(Acc,[element(A,B,C)],Accp),
    filterNonElement(Xs,Accp,Cp).

filterNonElement([_X|Xs],Acc,Cp):-
    filterNonElement(Xs,Acc,Cp).    


%visitList(+ElemList,+[],-Accum,+FileDesc1,+FileDesc2)
% ElemList contains part of xml graph
% []: Don't know why I added earlier. Possibly can be removed.
% Accum is the accumulator of different graph elements such as name of nodes
% FileDesc1 is the descriptor of buildpath.pl in which path/2 and func/2 are written 
% FileDesc2 is the descriptor of xmltreelogic.pl in which node/3 and edge/3 are written


visitList([],[],_L,_OS,_Tr).
visitList([X|Xs],L,NList,OS,Tr):-
    X=element('class:label',[_=B],C),
    (
       \+ member(B,NList)-> (writeNodeInfo(Tr,B,'class:label'),append(NList,[B],NListAux));NListAux=NList

    ),
    %writeOutput(OS,'class:label',B),
    filterNonElement(C,[],Cp),

    (  ((has_forknode(Cp),\+ Cp=[element('class:exec',_LL,_)|_XX]))->       % ;has_multipleJoinNode(Cp), Need to think later
       (
         nb_getval(barrier,Bcounter),
         atom_concat('barrier',Bcounter,BarrierNode),
         writeNodeInfo(Tr,BarrierNode,'class:barrier'),               % Create a dummy barrier node
         Bcounterp is Bcounter+1,
         nb_setval(barrier,Bcounterp),
         writeEdgeInfo(Tr,B,BarrierNode),
	 Parent=BarrierNode,
	 append(NListAux,[BarrierNode],NListp)
       ); (Parent=B,NListp=NListAux)

    ),
   visitElementList(Parent,Cp,NListp,NListUp,OS,Tr),!,
  %  visitElementList(B,Cp,NListAux,NListUp,OS,Tr),
    visitList(Xs,L,NListUp,OS,Tr).

has_multipleJoinNode(Cp):-
    findall(A,member(element('class:join',A,_B),Cp),L),
    length(L,N),
    N>1.

has_forknode(Cp):-
    (member(element('class:fork',A,_B),Cp);member(element('class:multifork',A,_Y),Cp)).

has_joinnode(Cp):-
    (member(element('class:join',A,_B),Cp);member(element('class:multijoin',A,_Y),Cp)).



%visitElementList(+Parent,+ElemList,+ElemListGen,-Accum,+FileDesc1,+FileDesc2)
% Parent is the parent node
% ElemList contains part of xml graph elements to be explored which are sucessor of Parent
% ElemListGen contains part of xml graph elements that are already generated
% Accum is the accumulator of different graph elements such as name of nodes
% FileDesc1 is the descriptor of buildpath.pl in which path/2 and func/2 are written 
% FileDesc2 is the descriptor of xmltreelogic.pl in which node/3 and edge/3 are written

visitElementList(_B,[],N,N,_OS,_Tr).
 
visitElementList(B,[X,Y|Xs],N,Np,OS,Tr):-
    X=element('class:exec',L,_),
   % member(func=P,L),
    visitElement(B,X,N,N1,OS,Tr),
    append(N,[P],N1),
    (
      has_forknode([Y|Xs])->
       (
         nb_getval(barrier,Bcounter),
         atom_concat('barrier',Bcounter,BarrierNode),
         writeNodeInfo(Tr,BarrierNode,'class:barrier'),               % Create a dummy barrier node
         Bcounterp is Bcounter+1,
         nb_setval(barrier,Bcounterp),
         writeEdgeInfo(Tr,P,BarrierNode),
	 Parent=BarrierNode,
	 atom_concat('r',BarrierNode,BNode),
	 atom_concat('r',B,ParBNode),
	 (nodepath(ParBNode,Path)->(bbDir(Base),atom_concat(Base,BPath,Path),writePathOutput(OS,BarrierNode,BPath),assertz(nodepath(BNode,Path)));true),
	 append(N1,[BarrierNode],N2)
       ); 
	( Parent=P,N2=N1,
	   %%% temporary 
	  atom_concat('r',B,ParBNode),atom_concat('r',P,PNode),
	  (has_joinnode([Y|Xs]),nodepath(ParBNode,Path)->
	      ( bbDir(Base),atom_concat(Base,BPath,Path),writePathOutput(OS,P,BPath),assertz(nodepath(PNode,Path)));true
           )
	)),
    
    visitElement(Parent,Y,N2,N3,OS,Tr),!,
    visitElementList(Parent,Xs,N3,Np,OS,Tr).

visitElementList(B,[X|Xs],N,Np,OS,Tr):-
    visitElement(B,X,N,N1,OS,Tr),
    visitElementList(B,Xs,N1,Np,OS,Tr).



% visitElement(+Parent,+Elem,+ElemListGen,-Accum,+FileDesc1,+FileDesc2)
% Parent is the parent node
% Elem is a graph element to check the relation with the Parent
% ElemListGen contains part of xml graph elements that are already generated
% Accum is the accumulator of different graph elements such as name of nodes
% FileDesc1 is the descriptor of buildpath.pl in which path/2 and func/2 are written 
% FileDesc2 is the descriptor of xmltreelogic.pl in which node/3 and edge/3 are written

visitElement(P,X,N,Np,OS,Tr):-
    X=element(A,L,_C),
    member(A,['class:multifork','class:fork','class:join','class:multijoin','class:enqueue']),
    member(branchLabel=B,L),!,
    (
	\+ member(B,N)->
	(

	    ((member(A,['class:join']), member(instance=IName,L), atom_concat('Barrier',_,IName))->
               writeNodeInfo(Tr,B,'class:barrier');writeNodeInfo(Tr,B,A)),
            append(N,[B],Np)
	);
	Np=N
    ),     
    writeEdgeInfo(Tr,P,B),!,
    (
      (member(A,['class:multifork','class:fork']), member(actionPackageFile=FPath,L))->
      (extract_dir_path(FPath,DPath), writePathOutput(OS,B,DPath)); true
    ).

visitElement(P,X,N,Np,OS,Tr):-
    X=element(A,L,_C),                    % Later update node info
    member(A,['class:exec']),!,
    member(func=B,L),!,
    (
	\+ member(B,N)->
	(
            writeNodeInfo(Tr,B,A),
	    writeFuncOutput(OS,P,B,B),
	    append(N,[B],Np),
	    BNode=B	   
	);
        (nb_getval(executableNode,Counter),
	atom_concat(B,Counter,BB),
	Counterp is Counter+1,
        nb_setval(executableNode,Counterp),
	writeNodeInfo(Tr,BB,A),
	writeFuncOutput(OS,P,BB,BB),
	BNode=BB,
	append(N,[BB],Np))
   ),
   % B belongs to the path DPath. But in order to have the consistency(like in other nodes), P also belongs to Dpath
   % and we include it for consistent inference
   (member(actionPackageFile=FPath,L)->(extract_dir_path(FPath,DPath), writePathOutput(OS,P,DPath)); true),
	
    writeEdgeInfo(Tr,P,BNode),!.
%    writeOutput(OS,A,L).


visitElement(P,X,N,Np,OS,Tr):-
    X=element(A,L,_C),
    member(A,['class:decision']),
    member(func=B,L),!,
    (
	\+ member(B,N)->
	(
            writeNodeInfo(Tr,B,A),
	    append(N,[B],Np)
	);
	Np=N
    ),
    writeEdgeInfo(Tr,P,B),
%
    member(branchFalseLabel=BF,L),
    member(branchTrueLabel=BT,L),
 %   writeNodeInfo(Tr,BT),
 %   writeNodeInfo(Tr,BF),
    writeEdgeInfo(Tr,B,BF),
    writeEdgeInfo(Tr,B,BT).
%  
   % writeOutput(OS,A,L).
%    visitList(B,C,[],OS,Tr).
   

visitElement(P,X,N,Np,OS,Tr):-
    X=element(A,L,_C),
    member(A,['class:return','class:nop', 'class:dequeue']),!,
    atom_concat('class:',B,A),
    atom_concat(B,P,BB),
    writeNodeInfo(Tr,BB,A),
    append(N,[BB],Np),
    writeEdgeInfo(Tr,P,BB),!.
%    writeOutput(OS,A,L).
%   visitList(B,C,[],OS,Tr).


extract_dir_path(FPath,DPath):-
	atom_concat(DPath,XML,FPath),
	atom_concat(A,'.xml',XML), 
	atom_chars(A,L), 
	\+ member('/',L),
	atom_concat(_,'/',DPath).

writePathOutput(OS,A,B):-
    write(OS,'path('),
    atom_concat('r',A,AA),	
    write(OS,AA),
    write(OS,',"'),
    bbDir(Base),
    atom_concat(Base,B,AbsolutePath),
    write(OS,AbsolutePath),
    write(OS,'").'),
    nl(OS),
    assertz(nodepath(AA,AbsolutePath)).

% writeFuncOutput(+FileDesc,+Class:Label, +Class:exec, + Func)

writeFuncOutput(OS,ParentNode,Node,FuncName):-
    write(OS,'func('),
    atom_concat('r',Node,NodeA),	
    write(OS,NodeA),
    write(OS,','),
    atom_concat('r',ParentNode,PNode),	
    write(OS,PNode),    
    write(OS,',"'),	
    write(OS,FuncName),
    write(OS,'").'),
    nl(OS).

    
writeInfo(Tr,A,B):-
     write(Tr,'info('),
     write(Tr,A),
     write(Tr,','),
     write(Tr,B),
     write(Tr,').'),
     nl(Tr).

writeNodeInfo(Tr,N,T):-
     nb_getval(graph,I),
     atom_concat('r',N,NN),
     write(Tr,'node('),
     write(Tr,NN),
     write(Tr,','),
     write(Tr,T),
     write(Tr,','),
     write(Tr,I),
     write(Tr,').'),
     nl(Tr).
    
writeEdgeInfo(Tr,P,A):-
    nb_getval(graph,I),
    atom_concat('r',P,PP),
    atom_concat('r',A,AA),
     write(Tr,'edge('),
     write(Tr,PP),
     write(Tr,','),
     write(Tr,AA),
     write(Tr,','),
     write(Tr,I),
     write(Tr,').'),
     nl(Tr).

parseXML(InputXML):-
	load_xml_file(InputXML,XML),
	mhpDir(MhpDir),
        atom_concat(MhpDir,'src/autogen/dumpxml.pl',DumpXml),
	open(DumpXml,write,OS),
	XML=[element(_A,_B,C)],
	filterNonElement(C,[],Cp),
	write(OS,Cp), 
	close(OS).



check_or_create_dir(D):-
	exists_directory(D),!.

check_or_create_dir(D):- make_directory(D).
