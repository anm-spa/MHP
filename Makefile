BBDIR=/repo/emasabu/bb
LLVMBIN=/repo/emasabu/tools/llvm/build-all/bin
DEBUGMODE=false

install:
	make clean
	swipl --quiet -s 'config/createConfig.pl' -t "configure_mhp_tool('${BBDIR}','${LLVMBIN}','${DEBUGMODE}')"
clean:
	rm -f *~ src/*~ config/*~ src/autogen/* config/config.pl bbTest/* outputs/*

