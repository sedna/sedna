set(xqops_SRC PPAbsPath.cpp PPAccessors.cpp PPAggrFuncs.cpp PPAxisAncestor.cpp
        PPAxisAttribute.cpp PPAxisChild.cpp PPAxisDescendant.cpp PPAxisFP.cpp PPAxisParent.cpp
        PPAxisSelf.cpp PPAxisSibling.cpp PPBooleanOps.cpp PPCalculate.cpp PPCheckpoint.cpp
        PPConst.cpp PPConstructors.cpp PPDDO.cpp PPDocInCol.cpp
        PPError.cpp PPExcept.cpp PPExtFunCall.cpp PPFilterEL.cpp PPFnAccessors.cpp
        PPFnGetProperty.cpp PPFnDateTimeFuncs.cpp PPFnDeepEqual.cpp PPFnDocAvailable.cpp
        PPFunCall.cpp PPGeneralComparison.cpp PPIf.cpp PPIndexScan.cpp PPIntersect.cpp
        PPLast.cpp PPLet.cpp PPNil.cpp PPNodeComparison.cpp PPNodeOps.cpp PPNumericFuncs.cpp
        PPOrderBy.cpp PPOrderBy_serializer.cpp PPPatMatch.cpp PPPred.cpp PPQName.cpp PPRange.cpp PPReturn.cpp
        PPSelect.cpp PPSequence.cpp PPSeqChecker.cpp PPSequenceOps.cpp PPSequenceTypes.cpp
        PPSpaceSequence.cpp PPStore.cpp PPStringFuncs.cpp PPStringsCompare.cpp PPSubsMatch.cpp
        PPSXptr.cpp PPTest.cpp PPTuple.cpp PPUnion.cpp PPUriFuncs.cpp PPVarDecl.cpp
        PPVariable.cpp PPXptr.cpp PPExplain.cpp)

if (ENABLE_FTSEARCH)
    list(APPEND xqops_SRC PPFtIndexScan.cpp PPFtIndexDict.cpp PPFtHighlight.cpp)
endif (ENABLE_FTSEARCH)

if (ENABLE_DTSEARCH)
    list(APPEND xqops_SRC PPFtScan.cpp)
endif (ENABLE_DTSEARCH)

if (SQL_CONNECTION)
    list(APPEND xqops_SRC PPSQL.cpp PPSQLODBC.cpp)
endif (SQL_CONNECTION)
