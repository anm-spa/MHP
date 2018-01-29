BBDIR=/repo/emasabu/bb
LLVMBIN=/repo/emasabu/tools/llvm/build-all/bin
DEBUGMODE=false

install:
	make clean
	mkdir bin
	cp setup/ranal bin/ranal
	cp setup/racer bin/racer
	chmod +x bin/ranal
	swipl --quiet -s 'config/createConfig.pl' -t "configure_mhp_tool('${BBDIR}','${LLVMBIN}','${DEBUGMODE}')"

test-install:
	make clean
	mkdir bin
	cp setup/ranal bin/ranal
	cp setup/racer bin/racer	
	chmod +x bin/ranal
	swipl --quiet -s 'config/createConfig.pl' -t "configure_mhp_tool_test_mode('test','bin','${DEBUGMODE}')"

.PHONY: clean

clean:
	rm -f *~ src/*~ config/*~ src/autogen/* config/config.pl bbTest/* outputs/*
	rm -rf bin
