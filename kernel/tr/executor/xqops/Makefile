#
# Makefile for xqops (GNU make)
#

PP = ../../../..

include $(PP)/Makefile.include

OBJS =  PPAbsPath$(OBJ_EXT) \
        PPAccessors$(OBJ_EXT) \
        PPADFilter$(OBJ_EXT) \
        PPAggrFuncs$(OBJ_EXT) \
        PPAxisAncestor$(OBJ_EXT) \
        PPAxisAttribute$(OBJ_EXT) \
        PPAxisChild$(OBJ_EXT) \
        PPAxisDescendant$(OBJ_EXT) \
        PPAxisFP$(OBJ_EXT) \
        PPAxisParent$(OBJ_EXT) \
        PPAxisSelf$(OBJ_EXT) \
        PPAxisSibling$(OBJ_EXT) \
        PPBooleanOps$(OBJ_EXT) \
        PPCalculate$(OBJ_EXT) \
        PPCheckpoint$(OBJ_EXT) \
        PPConst$(OBJ_EXT) \
        PPConstructors$(OBJ_EXT) \
        PPDAFilter$(OBJ_EXT) \
        PPDDO$(OBJ_EXT) \
        PPDebug$(OBJ_EXT) \
        PPDocInCol$(OBJ_EXT) \
        PPError$(OBJ_EXT) \
        PPExcept$(OBJ_EXT) \
        PPExtFunCall$(OBJ_EXT) \
        PPFilterEL$(OBJ_EXT) \
        PPFnAccessors$(OBJ_EXT) \
        PPFnDateTimeFuncs$(OBJ_EXT) \
        PPFnDeepEqual$(OBJ_EXT) \
        PPFnDocAvailable$(OBJ_EXT) \
        PPFunCall$(OBJ_EXT) \
        PPGeneralComparison$(OBJ_EXT) \
        PPIf$(OBJ_EXT) \
        PPIndexScan$(OBJ_EXT) \
        PPIntersect$(OBJ_EXT) \
        PPLet$(OBJ_EXT) \
        PPNil$(OBJ_EXT) \
        PPNodeComparison$(OBJ_EXT) \
        PPNodeOps$(OBJ_EXT) \
        PPNumericFuncs$(OBJ_EXT) \
        PPOrderBy$(OBJ_EXT) \
        PPPatMatch$(OBJ_EXT) \
        PPPred$(OBJ_EXT) \
        PPQName$(OBJ_EXT) \
        PPRange$(OBJ_EXT) \
        PPReturn$(OBJ_EXT) \
        PPScan$(OBJ_EXT) \
        PPSelect$(OBJ_EXT) \
        PPSequence$(OBJ_EXT) \
        PPSequenceOps$(OBJ_EXT) \
        PPSequenceTypes$(OBJ_EXT) \
        PPSLStub$(OBJ_EXT) \
        PPSpaceSequence$(OBJ_EXT) \
        PPSResLStub$(OBJ_EXT) \
        PPStore$(OBJ_EXT) \
        PPStringFuncs$(OBJ_EXT) \
        PPStringsCompare$(OBJ_EXT) \
        PPSubsMatch$(OBJ_EXT) \
        PPTest$(OBJ_EXT) \
        PPTuple$(OBJ_EXT) \
        PPUnion$(OBJ_EXT) \
        PPUp$(OBJ_EXT) \
        PPUriFuncs$(OBJ_EXT) \
        PPVarDecl$(OBJ_EXT) \
        PPVariable$(OBJ_EXT) \
        PPXptr$(OBJ_EXT)

ifeq ($(ENABLE_DTSEARCH),1)
OBJS += PPFtIndexScan$(OBJ_EXT) \
        PPFtScan$(OBJ_EXT) \
        PPFtHighlight$(OBJ_EXT)
endif

ifeq ($(SQL_CONNECTION),1)
OBJS += PPSQL$(OBJ_EXT)  \
        PPSQLODBC$(OBJ_EXT)
endif
ifeq ($(ENABLE_TRIGGERS),1)
OBJS += PPCreateTrigger$(OBJ_EXT) \
        PPDropTrigger$(OBJ_EXT)	
endif


include $(PP)/Makefile.pseudolib
