BBDIR=/repo/emasabu/bb
LLVMBIN=/repo/emasabu/tools/llvm/build-all/bin
DIVE=experiments/graph1
COMPWITH=experiments/example1

build:
	swipl --quiet -s 'config/createConfig.pl' -t "configureBB('${BBDIR}','${LLVMBIN}')"
	swipl --quiet -s 'src/xml2tree.pl' -t "convertXML('${DIVE}')"
	swipl --quiet -s 'src/tools.pl' -t validateall --
	swipl --quiet -s 'src/dataflowAnalysis.pl' -t main --  
	swipl --quiet -s 'src/tools.pl' -t build_all_task_spec --
	swipl --quiet -s 'src/buildRacerScript.pl' -t build_script --



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

dotTree:
	swipl --quiet -s 'src/tools.pl' -t printTreeinDot --

clean:
	rm -f *~ src/*~ config/*~ src/autogen/* config/config.pl bbTest/* test/outputs/*

