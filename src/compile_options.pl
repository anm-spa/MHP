:- module(compile_options,[includeDir/1,includeSelfDir/1,isystem/1,initCommandOP/1,finalCommandOP/1,clang/1]).
:- use_module(config/config).

% All valid paths in includeDir are relative to BB dir
includeDir('node/lm/bbmContDus52LmU/build/build_output/.b/smxaos0hxa/upcLmU/obj/allinc').
includeDir('bbi/legacy/lpp/export').
includeDir('bbi/legacy/lpp/kernel/src').
includeDir('bbi/legacy/mcsLU/inc').
includeDir('bbi/iw/public/mcsIfU/inc').
includeDir('upc/dlMacCeBl/pdcchSwU/src').
includeDir('upc/dlMacCeBl/seAdminUeSwU/inc').
includeDir('upc/upcCommonBl/ifocLU/inc').
includeDir('upc/upcCommonBl/evtHandlerLU/inc').
includeDir('upc/dlMacCeBl/pdschCombinePdcchIndFoIfU/inc').
includeDir('upc/dlMacCeBl/adminSwU/src').
includeDir('elib/rdrBl/rdrIfU/inc').
includeDir('up/dlRlcPeBl/dlRlcPeIfU/inc').
includeDir('upc/dlMacCeBl/pdschSchedSpLibLU/inc').
includeDir('upc/dlMacCeBl/validIfU/inc').
includeDir('node/lm/bbmContDus52LmU/build/build_output/.b/smxaos0hxa/common_gen/traceObj').
includeDir('elib/bbBaseBl/traceStringsLU/inc'). 
includeDir('elib/bbBaseBl/traceStringsLU/src'). 
includeDir('bbi/iw/public/selectiveTraceIfU/inc'). 
includeDir('bbi/iw/public/traceIfU/inc'). 
includeDir('bbi/iw/public/traceIfU/src'). 
includeDir('bbi/iw/public/stridIfU/inc'). 
includeDir('bbi/iw/public/signalsIfU/inc'). 
includeDir('bbi/iw/public/signalsIfU/src'). 
includeDir('.baseline/g2/data/IW_LEGACY_SIGBASES_CXA1106672_1/inc'). 
includeDir('bbi/iw/private/bbiTraceObjIfU/inc'). 
includeDir('bbi/legacy/ethSwU/inc'). 
includeDir('bbi/legacy/ethSwU/src'). 
includeDir('bbi/iw/private/traceObjRegOnStringIfU/inc'). 
includeDir('bbi/iw/public/tcbbIfU/inc'). 
includeDir('bbi/fm/cbb/camSwU/inc'). 
includeDir('upc/dlMacCeBl/pdschIfU/inc'). 
includeDir('upc/dlMacCeBl/pdschPreLaFoIfU/inc'). 
includeDir('upc/dlMacCeBl/pdschSchedFoIfU/inc'). 
includeDir('upc/dlMacCeBl/pdschSelFoIfU/inc'). 
includeDir('upc/dlMacCeBl/mtdiIfU/inc'). 
includeDir('elib/bbBaseBl/commonLU/inc'). 
includeDir('elib/bbBaseBl/commonLU/src'). 
includeDir('elib/constBl/constIfU/inc'). 
includeDir('elib/constBl/constIfU/src'). 
includeDir('elib/commonBl/commonLU/inc'). 
includeDir('elib/commonBl/commonLU/src'). 
includeDir('elib/nodeCommonBl/nodeCommonLU/inc'). 
includeDir('elib/nodeCommonBl/nodeCommonLU/src').
includeDir('.baseline/g2/data/IW_LEGACY_COMMON_CXA1106678_1/inc'). 
includeDir('.baseline/g2/data/IW_LEGACY_CONSTANTS_CXA1106670_1/inc'). 
includeDir('elib/bbBaseBl/sigGenLU/inc'). 
includeDir('elib/bbBaseBl/sigGenLU/src'). 
includeDir('omf/commonBl/commonLU/inc').
includeDir('bbi/iw/public/camIfU/inc'). 
includeDir('bbi/legacy/dbcLU/inc'). 
includeDir('bbi/legacy/dbcLU/src'). 
includeDir('bbi/iw/private/commStackIfU/inc'). 
includeDir('bbi/legacy/nameServerLU/inc'). 
includeDir('bbi/legacy/pbootLU/inc'). 
includeDir('bbi/iw/public/nameLookupIfU/inc'). 
includeDir('bbi/iw/private/nameLookupIfU/inc'). 
includeDir('bbi/iw/private/simpleServicesIfU/inc'). 
includeDir('bbi/legacy/ifSwU/inc'). 
includeDir('bbi/legacy/ifSwU/src'). 
includeDir('bbi/legacy/upp/UDAI/inc'). 
includeDir('bbi/iw/private/ethDrvIfU/inc'). 
includeDir('bbi/iw/public/simpleServicesIfU/inc'). 
includeDir('bbi/iw/public/userPlaneIfU/inc'). 
includeDir('bbi/iw/private/kpcIfU/inc'). 
includeDir('bbi/iw/public/cbbIfU/inc'). 
includeDir('bbi/iw/private/commStackMsgIfU/inc'). 
includeDir('bbi/iw/private/srioDrvIfU/inc'). 
includeDir('bbi/iw/public/cbbInitIfU/inc'). 
includeDir('up/ulMacPeBl/ciIfU/inc').
includeDir('up/upCommonBl/macCommonLU/inc'). 
includeDir('bbmc/bbUeMeBl/ueMiIfU/inc').
includeDir('bbmc/bbUeMeBl/ueMiIfU/src'). 
includeDir('upc/dlMacCeBl/dlMacCeIfU/inc'). 
includeDir('upc/dlMacCeBl/dlMacCeShdIfU/inc').
includeDir('upc/dlMacCeBl/pdschLaPcSpLibLU/inc').
includeDir('bbmc/bbCellMeBl/cellMiIfU/inc'). 
includeDir('bbmc/bbCellMeBl/cellMiIfU/src'). 
includeDir('bbmc/bbUeMeBl/bbUeMeIfU/inc'). 
includeDir('bbmc/bbUeMeBl/bbUeMeIfU/src'). 
includeDir('.baseline/g2/data/SCONI_CXA1106505/inc'). 
includeDir('.baseline/g2/data/SCONI_CXA1106505/src'). 
includeDir('upc/ulCellCeBl/ulCellCeIfU/inc'). 
includeDir('up/dlMacPeBl/ciIfU/inc'). 
includeDir('upc/upcCommonBl/commonIfU/inc'). 
includeDir('upc/upcCommonBl/commonIfU/src'). 
includeDir('elib/sysConstBl/sysConstIfU/inc'). 
includeDir('upc/dlMacCeBl/pdcchIfU/inc'). 
includeDir('upc/dlMacCeBl/dlMacCeSwU/src'). 
includeDir('upc/dlMacCeBl/pdschSwU/src'). 
includeDir('upc/dlMacCeBl/pdschPreSchedFoIfU/inc'). 
includeDir('upc/upcCommonBl/timerHandlerLU/inc'). 
includeDir('upc/dlMacCeBl/shdDlRefConvLU/inc'). 
includeDir('upc/upcCommonBl/timeBudgetLU/inc'). 
includeDir('upc/dlMacCeBl/schedCtrlIfU/inc'). 
includeDir('bbi/legacy/idl2SwU/inc'). 
includeDir('upc/dlMacCeBl/remoteSchedSwU/src'). 
includeDir('upc/dlMacCeBl/externalSchedSwU/inc'). 
includeDir('elib/papBl/x2commSwU/inc'). 
includeDir('elib/palBl/ipmLU/inc'). 
includeDir('bbmc/bbCellMeBl/dciIfU/inc'). 
includeDir('bbmc/bbCellMeBl/dciIfU/src'). 
includeDir('bbmc/bbSchMeBl/dciIfU/inc'). 
includeDir('bbmc/bbSchMeBl/dciIfU/src'). 
includeDir('upc/dlMacCeBl/adminIfU/inc'). 
includeDir('upc/dlMacCeBl/cellAdminIfU/inc'). 
includeDir('upc/dlMacCeBl/seAdminIfU/inc'). 
includeDir('upc/dlMacCeBl/storageIfU/inc'). 
includeDir('elib/bbBaseBl/sysConstLU/inc'). 
includeDir('upc/dlMacCeBl/schedCtrlPucchPowMeasFoIfU/inc'). 
includeDir('upc/dlMacCeBl/validFoIfU/inc'). 
includeDir('upc/dlMacCeBl/validPostValidFoIfU/inc'). 
includeDir('upc/dlMacCeBl/cellAdminSwU/inc'). 
includeDir('upc/dlMacCeBl/shdActivatorSwU/inc'). 
includeDir('node/lm/bbmContDus52LmU/build/build_output/.b/smxaos0hxa/srcgen/cbbt/dlUpcActivators'). 
includeDir('up/dlMacPeBl/dlMacPeIfU/inc').
includeDir('bbi/legacy/ckfLU/inc'). 
includeDir('bbmc/bbCellMeBl/bbCellMeIfU/inc'). 
includeDir('elib/bbBaseBl/routerLU/inc'). 
includeDir('elib/bbBaseBl/genericIfLU/inc'). 
includeDir('elib/palBl/rbsTraceLU/inc').
includeDir('bbmc/bbMtdMeBl/mtdiIfU/inc'). 
includeDir('bbmc/bbMtdMeBl/mtdiIfU/src'). 
includeDir('.baseline/g2/data/TNRHI_CXA1106868_1/inc'). 
includeDir('elib/bbBaseBl/mtdLU/inc'). 
includeDir('bbi/fm/obs/traceSwU/inc'). 
includeDir('bbi/fm/obs/bufferedTraceSwU/inc'). 
includeDir('bbi/legacy/aomIfU/inc'). 
includeDir('bbi/legacy/aomIfU/src'). 
includeDir('bbi/legacy/duaiIfU/inc'). 
includeDir('.baseline/g2/data/IW_BOARDCONTROL_CXA1106676_1/basicIfU/inc'). 
includeDir('.baseline/g2/data/IW_BOARDCONTROL_CXA1106676_1/ethIfU/inc'). 
includeDir('bbi/legacy/xioSwU/inc'). 
includeDir('bbi/legacy/xioSwU/src'). 
includeDir('elib/papBl/ethWrapSwU/inc'). 
includeDir('bbi/iw/private/listsIfU/inc'). 
includeDir('elib/palBl/fifoLU/inc'). 
includeDir('upc/upcCommonBl/exponentTableLU/inc'). 
includeDir('upc/dlMacCeBl/validSpLibLU/inc'). 
includeDir('upc/dlMacCeBl/shdSwU/inc'). 
includeDir('upc/dlMacCeBl/seAdminCchSwU/inc'). 
includeDir('up/upCommonBl/rlcCommonLU/inc'). 
includeDir('elib/palBl/hashLU/inc'). 
includeDir('bbi/legacy/mcsLU/src'). 
includeDir('bbi/iw/public/mcsIfU/src'). 
includeDir('bbi/fm/obs/mcsSwU/inc'). 
includeSelfDir('../inc'). 
includeSelfDir('../export').  


%%% all valid paths in isystem are absolute paths.
isystem('/proj/bbi_twh/wh_bbi/x86_64-Linux2/fader2_sdk/1.56/compiler/fader2_arch/fader2_2/include').
%isystem(LPP):-
%	bbDir(BB),
%	atom_concat(BB,'/bbi/legacy/lpp/export',LPP),!.

clang('/proj/bbi_twh/wh_bbi/x86_64-Linux2/fader2_sdk/1.56/libexec/clang').

initCommandOP(' -w -mllvm -disable-llvm-optzns -fflexc -fno-inline -D LBLM4_TRINITY -D TRINITY -D PHOENIX4 -D TRINITY_TEMP_CODESTUB -D BBI_MCSCTRL_DOMAIN=BBI_MCSCTRL_DOMAIN_LTE -D BBI_MCSCTRL_NO_OF_AGENTS=4 -D ULL1PE_ULA_COLOCATED -D MAKE_WITH_LPP -D MAKE_CHECKMAGIC -D EXCLUDE_OSE -D BBI_TRACE_ENABLE_TN -D BBI_TRACE_CONTROLLER_PID=\\"baithost/traceCtrl\\" -D R2_HW_PAP -D BBI_LTE_LINX_SEG_REASSEM -D BBI_CBB_MTD_ENABLED -D LINXSWU_ALLOW_SIGNALS_GT_1KW -D XDD2_DUS52 -D TRINITY4 -D ON_TARGET_WA -D MULTI_EMCA_ITC -D TEST_PBOOT_ULMA_SNID=1024 -D PAP_IDL2_MEASURE_TX_2_RX_DELAY -D ON_TARGET_BUILD -D BBI_TRACE__HIT_MAX_NO_STRIDS=256 -D MAKE_LMPRODNO=\\"CXC2010800_1\\" -D LTE_USE_DEPRECATED_PRINTF -D MY_GIT_TOP=/repo/emasabu/bb -D PHOENIX4 -D LPP_ENABLE_HAO ').

finalCommandOP('--analyze --analyzer-output text -O3 --target=phoenix -mcpu=phoenixIV -fno-common -nostdinc -g0 -S -emit-llvm -std=c99 '). 