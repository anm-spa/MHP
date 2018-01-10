BBDIR=/repo/emasabu/bb
LLVMBIN=/repo/emasabu/tools/llvm/build-all/bin
DIVE=experiments/dlMacCe.dive
DIVE1=experiments/graph1Bak
COMPWITH=experiments/example1

build:
#	swipl --quiet -s 'config/createConfig.pl' -t "configureBB('${BBDIR}','${LLVMBIN}')"
	swipl --quiet -s 'src/xml2tree.pl' -t "convertXMLs(['${DIVE}','${DIVE1}'])"
#	swipl --quiet -s 'src/xml2tree.pl' -t "convertXML('${DIVE}')"
#	swipl --quiet -s 'src/tools.pl' -t validateall --
#	swipl --quiet -s 'src/dataflowAnalysis.pl' -t main --
#	swipl --quiet -s 'src/dataflowAnalysis.pl' -t "main(['src/autogen/graph_dlMacCe.pl','src/autogen/graph.pl'])" --  
#	swipl --quiet -s 'src/tools.pl' -t build_all_task_spec --
#	swipl --quiet -s 'src/buildRacerScript.pl' -t build_script --
#	swipl --quiet -s 'src/buildRacerScript.pl' -t build_compile_commands --



compare:
	swipl --quiet -s 'src/analXMLStructure.pl' -t "compareXMLs('${COMPWITH}','${DIVE}')"

parseStruct:
	swipl --quiet -s 'src/analXMLStructure.pl' -t "parseXMLStruct('${DIVE}')"



tasks:
	swipl --quiet -s 'src/tools.pl' -t test1 --

setconfig:
	swipl --quiet -s 'config/createConfig.pl' -t "configureBB('${BBDIR}','${LLVMBIN}')"


parseXML:
	swipl --quiet -s 'src/xml2tree.pl' -t "parseXML('${COMPWITH}')"

dotG:
	swipl --quiet -s 'src/tools.pl' -t printTreeinDot --

mhpdotG:
	swipl --quiet -s 'src/tools.pl' -t "drawMarkedMHPGraph('${task}')" --
	dot -Tpdf test/outputs/mhpGraph.dot -o test/outputs/mhpGraph.pdf
	xdg-open test/outputs/mhpGraph.pdf

chtdotG:
	swipl --quiet -s 'src/tools.pl' -t "drawMarkedCHTGraph('${task}')" --
	dot -Tpdf test/outputs/chtGraph.dot -o test/outputs/chtGraph.pdf
	xdg-open test/outputs/chtGraph.pdf


clean:
	rm -f *~ src/*~ config/*~ src/autogen/* config/config.pl bbTest/* test/outputs/*

