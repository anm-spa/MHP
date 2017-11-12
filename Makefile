BBDIR='/repo/emasabu/bb'
LLVMBIN='/repo/emasabu/tools/llvm/build-all/bin'

build:
	swipl --quiet -s 'config/createConfig.pl' -t "configureBB(${BBDIR},${LLVMBIN})"
	swipl --quiet -s 'src/xml2tree.pl' -t "convertXML('experiments/graph1')"
	swipl --quiet -s 'src/tools.pl' -t validateall --
	swipl --quiet -s 'src/dataflowAnalysis.pl' -t main --  
	swipl --quiet -s 'src/tools.pl' -t build_all_task_spec --
	swipl --quiet -s 'src/buildRacerScript.pl' -t build_script --



compare:
	swipl --quiet -s 'src/analXMLStructure.pl' -t "compareXMLs('experiments/example1','experiments/graph1')"

parseStruct:
	swipl --quiet -s 'src/analXMLStructure.pl' -t "parseXMLStruct('experiments/graph1')"



tasks:
	swipl --quiet -s 'src/tools.pl' -t test1 --

setconfig:
	swipl --quiet -s 'config/createConfig.pl' -t "configureBB(${BBDIR},${LLVMBIN})"


parseXML:
	swipl --quiet -s 'src/xml2tree.pl' -t "parseXML('experiments/shdActivatorSwU.dive')"

dotTree:
	swipl --quiet -s 'src/tools.pl' -t printTreeinDot --

clean:
	rm -f *~ src/*~ config/*~ src/autogen/* config/config.pl bbTest/* test/outputs/*

cleanall:
	rm -f *~ src/*~ src/autogen/~ experiments/outputs/*