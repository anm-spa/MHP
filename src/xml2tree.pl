:- module(xml2tree,[convertXML/1,parseXML/1]).
:- use_module(library(sgml)).
:- use_module(config/config).
:- use_module(helper).

:- dynamic nodepath/2.

%convertXML(+InputXML)
%InputXML is the input dive file in xml format describing high-level design of a parallel application. It generates the following prolog files.
%"xmltreelogic.pl" describes possibly disjoint graphs having the node and edge predicates. 
% Predicate node have the following specification: node(Nodename,NodeClass in XML, GraphId in which Nodename belongs).
% Predicate edge have the following specification: edge(Node1, Node2, GraphId in which this edge belongs).

%% convertXMLdefault(InputXML):-
%%       load_xml(InputXML,XML,[dialect(xml)]),
%%       mhpDir(MhpDir),
%%       atom_concat(MhpDir,'src/autogen',AutoGen),
%%       check_or_create_dir(AutoGen),
%%       atom_concat(MhpDir,'src/autogen/buildpath.pl',BuildPath),	
%%       atom_concat(MhpDir,'src/autogen/graph.pl',Graph),
%%       atom_concat(MhpDir,'src/autogen/graphExtended.pl',ExtendedGraph),	
%%       atom_concat(MhpDir,'src/autogen/xmltreeInfo.pl',TreeInfo),	
%%       open(BuildPath,write,OS),
%%       open(Graph,write,Tr),
%%       open(TreeInfo,write,Tr1),
%%       write(Tr,':-module(graph,[node/3,edge/3,graphs/1,graphName/2]).'),
%%       write(OS,':-module(buildpath,[path/2,func/3]).'),
%%       nl(OS),	
%%       write(OS,':- discontiguous path/2.'), nl(OS),	 	
%%       write(OS,':- discontiguous func/3.'),
%%       nl(Tr),	
%%       write(Tr,':- discontiguous node/3.'),nl(Tr),	 	
%%       write(Tr,':- discontiguous edge/3.'),nl(Tr),	
%%       write(Tr,':- discontiguous graphName/3.'),	

%%       nl(Tr),
%%       nl(Tr),

%%       write(Tr,':- use_module("'),write(Tr,ExtendedGraph),write(Tr,'").'),nl(Tr),	 		
%%       nl(OS),	
%% %      retractall(node(_)),	
%%       visitMain(XML,GraphIdList,OS,Tr,Tr1),
      	
%%       write(Tr,'graphs('),
%%       write(Tr,GraphIdList),
%%       write(Tr,').'), nl(Tr),
%%       close(Tr),	
%%       use_module(Graph),	
%%       extendGraph(ExtendedGraph,Graph,'graph'),	
%%       retractall(nodepath(_N,_P)),	
%%       close(Tr1),
%%       close(OS).
 

convertXML(XMLs):-
	mhpDir(MhpDir),
	atom_concat(MhpDir,'src/autogen',AutoGen),
	check_or_create_dir(AutoGen),
	atom_concat(MhpDir,'src/autogen/buildpath.pl',BuildPath),
	atom_concat(MhpDir,'src/autogen/xmltreeInfo.pl',TreeInfo),
	atom_concat(MhpDir,'src/autogen/graph.pl',GraphInfo),
	
	open(TreeInfo,write,Tr1),
	(exists_file(GraphInfo) ->(use_module(GraphInfo),open(GraphInfo,append,GrInfoStream));
	    (
		open(GraphInfo,write,GrInfoStream),
		write(GrInfoStream,':-module(graph,[graphInfo/2,graphLoc/2]).'),
		nl(GrInfoStream),
		write(GrInfoStream,':- discontiguous graphInfo/2.'), nl(GrInfoStream),	 	
		write(GrInfoStream,':- discontiguous graphLoc/2.'),
		nl(GrInfoStream)
	    )
        ),

	(exists_file(BuildPath) ->(use_module(BuildPath),open(BuildPath,append,PathStream));
	    (
		open(BuildPath,write,PathStream),
		write(PathStream,':-module(buildpath,[path/2,func/3]).'),
		nl(PathStream),	
		write(PathStream,':- discontiguous path/2.'), nl(PathStream),	 	
		write(PathStream,':- discontiguous func/3.'),
		nl(PathStream)
		    )
        ),
	convertXMLAux(XMLs,AutoGen,PathStream,Tr1,GrInfoStream),
	retractall(nodepath(_N,_P)),	
	close(Tr1),
	close(GrInfoStream),
	close(PathStream).

convertXMLAux(InputXML,AutoGen,PathStream,Tr1,GrInfoStream):-
	load_xml(InputXML,XML,[dialect(xml)]),
	
	getTextualFileName(InputXML,FileName),

	atom_concats(['/graph_',FileName,'.pl'],GraphName),
	atom_concats(['/graphExtended_',FileName,'.pl'],GraphExtended),	
	atom_concat(AutoGen,GraphName,Graph),
	atom_concat(AutoGen,GraphExtended,ExtendedGraph),
    
	open(Graph,write,Tr),
	atom_concats([':-module(graph_',FileName,',[node/3,edge/3,graphs/1]).'], Module),
	write(Tr,Module),
        nl(Tr),	
	write(Tr,':- discontiguous node/3.'),nl(Tr),	 	
	write(Tr,':- discontiguous edge/3.'), nl(Tr),	
	%write(Tr,':- discontiguous graphName/2.'),	
	%nl(Tr),
	nl(Tr),

	atom_concats(['graphExtended_',FileName],ExtGraph),
	write(Tr,':- use_module("'),	
	write(Tr,ExtGraph),write(Tr,'").'),nl(Tr),	 			
	visitMain(XML,GraphIdList,PathStream,Tr,Tr1,GrInfoStream),
      	
	forall(member(Id,GraphIdList),(
	  catch(((graphLoc(Id,Graph),Status=exists);Status=notExists),_,Status=notExists),
	  (Status=notExists -> (
	      atom_concats(['graphLoc(',Id,',\'',Graph,'\').'],GLoc),
	      write(GrInfoStream,GLoc),nl(GrInfoStream)
	  );true)
	)),
	
	write(Tr,'graphs('),
	write(Tr,GraphIdList),
	write(Tr,').'), nl(Tr),
	close(Tr),	
	use_module(Graph),	
	extendGraph(ExtendedGraph,Graph,FileName).      


% This predicate creates a unique start node and a barrier node for each graph, connecting start node to the barrier node and barrier node to all node that have no predecessors. Thus, it connects all disjoint graphs. 

extendGraph(F,Graph,FileName):-
	open(F,write,Os),
	(FileName='graph' -> 	atom_concats([':-module(graphExtended',',[nd/3,arc/3]).'], Module);
	    atom_concats([':-module(graphExtended_',FileName,',[nd/3,arc/3]).'], Module)),
	write(Os,Module),	
	nl(Os),
	write(Os,':- discontiguous nd/3.'),nl(Os),	 	
	write(Os,':- discontiguous arc/3.'),nl(Os),	

	graphs(GIdList),
	forall(member(G,GIdList),(
	    atom_concats([FileName,'_start_',G],Start),
	    atom_concats([FileName,'_rbarrier0_',G],StartBarrier),	
	    N1=nd(Start,class:label,G),
            N2=nd(StartBarrier,class:barrier,G),
            E1=arc(Start,StartBarrier,G),
	    write(Os,N1),write(Os,'.'),nl(Os),
	    write(Os,N2),write(Os,'.'),nl(Os),
	    write(Os,E1),write(Os,'.'),nl(Os),
	    findall(P,(node(P,_,G),\+ edge(_X,P,G)),List),
	    findall(arc(StartBarrier,P,G),member(P,List),EList),
	    forall(member(E,EList),(write(Os,E),write(Os,'.'),nl(Os)))
	)),
   	close(Os),
	 open(Graph,append,Tr),
	 write(Tr,'node(A,B,C):-nd(A,B,C).'),nl(Tr),	 		
	 write(Tr,'edge(A,B,C):-arc(A,B,C).'),nl(Tr),
	 close(Tr).




visitMain([element(A,B,C)],GraphIdList,PathStream,Tr,Tr1,GrInfoStream):-	
    writeInfo(Tr1,A,B),
    filterNonElement(C,[],Cp),
    nb_setval(barrier,1),
    nb_setval(executableNode,1),
    nb_setval(graph,0),
    processMain(Cp,Tr1,PathStream,[],GraphIdList,Tr,GrInfoStream).

processMain([],_Tr1,_PathStream,G,G,_Tr,_GrInfoStream).
processMain([X|Xs],Tr1,PathStream,Accum,G,Tr,GrInfoStream):-
    processInfo(X,Tr1,GrInfoStream,Xp),
    (\+ (Xp=[])->	
	(retractall(nodepath(_N,_P)),	
	visitGraph(Xp,PathStream,Tr,Tr1),
	nb_getval(graph,N),
	append(Accum,[N],Accump)
        );
	Accump=Accum      
    ),
    processMain(Xs,Tr1,PathStream,Accump,G,Tr,GrInfoStream).

processInfo(element('class:class',B,C),Tr1,Os,Xp):-
	writeInfo(Tr1,'class:class',B),
	filterNonElement(C,[],Cp),
	filterAuxInfo(Cp,Tr1,Xp),
	(\+ Xp=[] -> (
	    member(name=Name,B),
	    atom_concat('gr_',Name,TName),
	    find_or_createGraphInfo(TName,Os,Id),
	    nb_setval(graph,Id)
	);true).

processInfo(element('structure:structure',_B,C),Tr1,_Os,Xp):-
    filterNonElement(C,[],Cp),
    filterAuxInfo(Cp,Tr1,Xp).


find_or_createGraphInfo(Name,Os,Id):-
	catch((graphInfo(Id,Name),!),_E,fail).

find_or_createGraphInfo(Name,Os,Id):-
	catch((findall(I,graphInfo(I,_),Ilist),nb_getval(graph,II),union(Ilist,[II],IDList)),_,(nb_getval(graph,Imax),IDList=[Imax])),
	max_list(IDList,Max),
	Id is Max+1,
	atom_concats(['graphInfo(',Id,',',Name,').'],GInfo),
	write(Os,GInfo),nl(Os).


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


if_unique(N,Np):-
 	N=startActivity,
	nb_getval(graph,I),!,
	atom_concat(N,I,Np).

if_unique(N,N).

visitList([],[],_L,_OS,_Tr).
visitList([X|Xs],L,NList,OS,Tr):-
    X=element('class:label',[_=B],C),
    (
       \+ member(B,NList)-> (if_unique(B,B_uni),writeNodeInfo(Tr,B_uni,'class:label'),append(NList,[B_uni],NListAux));(NListAux=NList,B_uni=B)

    ),
    %writeOutput(OS,'class:label',B),
    filterNonElement(C,[],Cp),

    (  ((has_forknode(Cp),\+ Cp=[element('class:exec',_LL,_)|_XX]);(has_joinnode(Cp),atom_concat('Join',_,B)))->       % ;has_multipleJoinNode(Cp), Need to think later
       (
         nb_getval(barrier,Bcounter),
         atom_concat('barrier',Bcounter,BarrierNode),
         writeNodeInfo(Tr,BarrierNode,'class:barrier'),               % Create a dummy barrier node
         Bcounterp is Bcounter+1,
         nb_setval(barrier,Bcounterp),
         writeEdgeInfo(Tr,B_uni,BarrierNode),
	 Parent=BarrierNode,
	 append(NListAux,[BarrierNode],NListp)
       ); (Parent=B_uni,NListp=NListAux)

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
    (member(element('class:join',A,_B),Cp)).

ifJoinFollowedByOtherNode([X,_Y|Ys]):-
	X=element('class:join',_A,_B).

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
      (has_forknode([Y|Xs]);(has_multipleJoinNode([Y|Xs]));(ifJoinFollowedByOtherNode([Y|Xs])))->
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
	 
	 (nodepath(ParBNode,Path)->(bbDir(Base),atom_concat(Base,BPath,Path),writePathOutput(OS,BarrierNode,BPath),writePathOutput(OS,P,BPath),assertz(nodepath(BNode,Path)));true),
	 append(N1,[BarrierNode],N2)
       ); 
	( Parent=P,N2=N1,
	   %%% temporary 
	  atom_concat('r',B,ParBNode),
	  (has_joinnode([Y|Xs]),nodepath(ParBNode,Path)->
	      ( bbDir(Base),atom_concat(Base,BPath,Path),writePathOutput(OS,P,BPath),assertz(nodepath(P,Path)));true
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
	writeFuncOutput(OS,P,BB,B),
	BNode=BB,
	append(N,[BB],Np))
   ),
   
   % B belongs to the path DPath. But in order to have the consistency(like in other nodes), P also belongs to Dpath
   % and we include it for consistent inference
   (member(actionPackageFile=FPath,L)->(extract_dir_path(FPath,DPath), writePathOutput(OS,P,DPath),writePathOutput(OS,BNode,DPath)); true),
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
     assertz(node(NN)),	
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



