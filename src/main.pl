:-module(main,[main/7,list_graph/0]).
:-use_module(helper).

main(GList,Force,Race,PTask,ETask,DebugMode,ShowGraph):-
	% initialize environment
	working_directory(W,W),
	atom_concat(TDir,'bin/',W),
	chdir(TDir),
	use_module('config/config.pl'),
	retractall(force_analysis(_)),
	retractall(debug_mode(_)),
	(Force=yes->assertz(force_analysis(true));assertz(force_analysis(false))),
	(DebugMode=yes->assertz(debug_mode(true));assertz(debug_mode(false))),
	% run necessary analysis
	run_parser(GList,ShowGraph,GListMod),
	(\+ GListMod =[] -> (
	    collect_all_mhp_list(PTask,GListMod,MHP),
	    show_mhp_graph(PTask,GListMod),
	    show_cht_graph(ETask,GListMod),
	    perform_essential_analysis(MHP,Race)
	    );
	    (write("***Info: Use -f to force essential analysis which may take some time"),nl)
	),
	chdir(W).


list_graph:-
	working_directory(W,W),
	atom_concat(TDir,'bin/',W),
	chdir(TDir),
	consult('src/autogen/graph.pl'),
	findall((Id, Name,Loc),(graphInfo(Id,Name),graphLoc(Id,Loc)),GList),
	(\+ GList = [] -> (write("*****************List of all graphs parsed before***************"),nl,
	                   write("----------------------------------------------------------------"),nl);true),
	forall(member((Id, Name,Loc),GList),(
	atom_concats(['Graph: ',Name,' (Id: ',Id,'InternalRep: ',Loc,')'],ToWrite),
	write(ToWrite),nl
	)),
	abolish(graphInfo/2),
	abolish(graphLoc/2),
	abolish(origin/2),
	chdir(W).


% if +GList is the list of dive files, then they are parsed and dataflow analysis are possibly performed on these graphs.
% However, if the input are name of graphs already parsed, then further parallelism shall be detected from the parallely running graphs

run_parser(GList,ShowGraph,GListChecked):-
	graphs_in_dive_format(GList,GListChecked),
	subtract(GList,GListChecked,Rem),
	(GListChecked = [] -> fail;true),
	run_parser_for_dive_file(GListChecked),!,
	show_all_graphs(GListChecked,ShowGraph),
	forall(member(G,Rem),
	( write(G),write(" does not exists (being ignored)!!! "),nl)).


% check if GMList contains everything
run_parser(GList,ShowGraph,ResultFile):-
	graphs_in_prolog_format(GList,GMList),	
	use_module('src/mhp_anal.pl',[mhpOfGraphs/3]),
	analyze_graph_file(GMList,ResultFile),!,
	show_all_graphs([ResultFile],ShowGraph),
	abolish(mhpOfGraphs/3).

run_parser(_,_ShowGraph,_G):-
	write('***FAIL: all required names are either (i) action graph files, or (ii) action graph names parsed before***'),nl,fail.



run_parser_for_dive_file(GList):-
	mhpDir(MhpDir),
	atom_concat(MhpDir,'src/parse_graph.pl',Parser),
	atom_concat(MhpDir,'src/dataflowAnalysis.pl',DataFlowAnalyzer),
	use_module(Parser,[parse_activity_graph/1]),
	use_module(DataFlowAnalyzer,[main/1]),

	run_parser_for_dive_file_aux(GList),

	abolish(main/1),
	abolish(parse_activity_graph/1).

run_parser_for_dive_file_aux([]).
run_parser_for_dive_file_aux([G|Gs]):-
	absolute_file_name(G,AbsG),
	parse_activity_graph(AbsG),
	mhpDir(Mhp_dir),
	getTextualFileName(AbsG,GF),
	atom_concats([Mhp_dir,'src/autogen/graph_',GF,'.pl'],GraphName),

	(force_analysis(true)->main(GraphName);write("***Info: parallelism detected for this file in previous analysis"),nl),
	run_parser_for_dive_file_aux(Gs).


graphs_in_dive_format([],[]).
graphs_in_dive_format([G|Gs],[G|Rs]):-
	atom_concat(_,'.dive',G),
	exists_file(G),!,
	graphs_in_dive_format(Gs,Rs).

graphs_in_dive_format([G|Gs],[G|Rs]):-
	exists_file(G),!,
	graphs_in_dive_format(Gs,Rs).

graphs_in_dive_format([_G|Gs],Rs):-
        graphs_in_dive_format(Gs,Rs).

graphs_in_prolog_format(GList,GListMod):-
	catch((
	       use_module('src/autogen/graph.pl',[graphInfo/2]),
	       graphs_in_prolog_format_aux(GList,GListMod),
               abolish(graphInfo/2)
	       ),_,fail).

graphs_in_prolog_format_aux([],[]).
graphs_in_prolog_format_aux([G|Gs],[GName|Rs]):-
	(graphInfo(G,GName);(atom(G),atom_number(G,GN),graphInfo(GN,GName))),!,
	graphs_in_prolog_format_aux(Gs,Rs).

graphs_in_prolog_format_aux([G|Gs],[G|Rs]):-
	graphInfo(_Gid,G),!,
	graphs_in_prolog_format_aux(Gs,Rs).

graphs_in_prolog_format_aux([G|Gs],[Gp|Rs]):-
	\+ number(G),
	atom_concat('gr_',G,Gp),
	graphInfo(_Gid,Gp),!,
	graphs_in_prolog_format_aux(Gs,Rs).

graphs_in_prolog_format_aux([G|Gs],Rs):-
	write(G),write(" does not exists (being ignored)!!! "),nl,
	graphs_in_prolog_format_aux(Gs,Rs).


analyze_graph_file([G],G).
analyze_graph_file([G1,G2|Gs],R):-
	mhpOfGraphs(G1,G2,GFile),
	analyze_graph_file([GFile|Gs],R).


% Perform all analysis such as (i) build task specification, (ii) build compile_commands.json file to be used by racer, 
% (iii) call racer to collect all message send events, and (iv) build racer script to test race condition.

perform_essential_analysis(MHP,Race):-
	force_analysis(true),!,
	use_module('src/buildRacerScript.pl',[build_all_task_spec/0,build_compile_commands/0,build_all_events/0,build_race_checker_script/1]),

	write("***Info: building all task specification - "),
	build_all_task_spec,
	write("finished"),nl,
	write("***Info: building compile commands (compile_commands.json) - "),nl, write("***Info: "),
	build_compile_commands,
	write("***Info: writing to compile_commands.json file - finished"),nl,

	write("***Info: finding all message send events (calling racer may take some time, please wait...) - "),nl,
	
	% check if events are generated before
	build_all_events,
	write("***Info: searching task specific DSP C files for message send events - finished"),nl,
	
	(Race=yes -> (
	    write("***Info: creating script to run racer -"),
	    build_race_checker_script(MHP),
	    write("finished"),nl,
            write("***Info: Running racer to check data race (this may take some time)"),nl,
	    shell('bin/check_race',_),
	    write("***Info: racer has finished execution"),nl
	    );true),

	abolish(build_all_task_spec/0),
	abolish(build_all_events/0),
	abolish(build_race_checker_script/1),
	abolish(build_compile_commands/0).

perform_essential_analysis(_MHP,_Race).


collect_all_mhp_list(none,GList,MHP):-
	collect_all_mhp_list_aux(GList,[],MHP),!.

collect_all_mhp_list(T,GList,MHP):-
	collect_all_mhp_list_aux(GList,[],MHP_aux),
	findall((P,Q),(member((P,Q),MHP_aux),(P=T;Q=T)),MHP).


collect_all_mhp_list_aux([],MHP,MHP).
collect_all_mhp_list_aux([G|Gs],Acc,MHP):-
	absolute_file_name(G,AbsG),
	getTextualFileName(AbsG,GF),
	mhpDir(MhpDir),
	atom_concats([MhpDir,'src/autogen/mhp_',GF,'.pl'],MHPGraphName),
	exists_file(MHPGraphName),
	consult(MHPGraphName),
	findall((P,Q),(mhp(P,Q),(\+ P=dummyTask;\+ Q=dummyTask)),MHPList),
	abolish(mhp/2),
	remove_duplicates(MHPList,[],MHP_noDuplicate),
	union(Acc,MHP_noDuplicate,Accp),
	collect_all_mhp_list_aux(Gs,Accp,MHP).

	

remove_duplicates([],Res,Res).
remove_duplicates([(P,Q)|Mhp],Acc,Res):-
	(\+ member((P,Q),Acc); \+ member((Q,P),Acc)),
	union(Acc,[(P,Q)],Accp),!,
	remove_duplicates(Mhp,Accp,Res).

remove_duplicates([_|Mhp],Acc,Res):-
	remove_duplicates(Mhp,Acc,Res).



show_all_graphs(_GList,no):-!.

show_all_graphs(GList,yes):-
	use_module('src/tools.pl',[draw_action_graph/1]),
	show_all_graphs_aux(GList),
	abolish(draw_action_graph/1).

show_all_graphs_aux([]).
show_all_graphs_aux([G|Gs]):-
	getTextualFileName(G,FileName),
	mhpDir(MhpDir),
	((atom_concats([MhpDir,'src/autogen/graph_',FileName,'.pl'],GraphName),exists_file(GraphName));
	 (exists_file(G),GraphName=G)),!,
	draw_action_graph(GraphName),
	show_all_graphs_aux(Gs).


show_mhp_graph(none,_GListMod):-!.

show_mhp_graph(_ETask,[]).
show_mhp_graph(ETask,[G|Gs]):-
	absolute_file_name(G,AbsG),
	use_module('src/tools.pl',[draw_mhp_graph/2]),
	draw_mhp_graph(ETask,AbsG),
	abolish(draw_mhp_graph/2),
	show_mhp_graph(ETask,Gs).


show_cht_graph(none,_GListMod):-!.

show_cht_graph(_ETask,[]).
show_cht_graph(ETask,[G|Gs]):-
	absolute_file_name(G,AbsG),
	use_module('src/tools.pl',[draw_cht_graph/2]),
	draw_cht_graph(ETask,AbsG),
	abolish(draw_cht_graph/2),
	show_cht_graph(ETask,Gs).