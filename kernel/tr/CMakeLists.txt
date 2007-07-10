ADD_LIBRARY ( tr 

	# Header Files
	cl_client.h client_core.h socket_client.h tr_common_funcs.h
	tr_functions.h tr_globals.h tr_utils.h

	# Source Files
	cl_client.cpp rcv_db.cpp socket_client.cpp tr.cpp
	tr_common_funcs.cpp tr_functions.cpp tr_globals.cpp
	tr_rcv.cpp

	# Header Files, from auth
	auth/auc.h

	# Source Files, from auth
	auth/auc.cpp

	# Header Files, from crmutils
	crmutils/crmutils.h crmutils/exec_output.h
	crmutils/node_utils.h crmutils/scheme_utils.h
	crmutils/str_matcher.h

	# Source Files, from crmutils
	crmutils/debug_utils.cpp crmutils/exec_output.cpp
	crmutils/file_utils.cpp crmutils/namespace_utils.cpp
	crmutils/node_utils.cpp crmutils/print_utils.cpp
	crmutils/scheme_utils.cpp crmutils/str_matcher.cpp
	crmutils/system_tables.cpp

	# Header Files, from executor/base
	executor/base/PPBase.h executor/base/PPOperations.h
	executor/base/PPUtils.h executor/base/SequenceType.h
	executor/base/XMLDateTime.h executor/base/XPath.h
	executor/base/XPathOnSchema.h executor/base/bit_set.h
	executor/base/compare.h executor/base/dm_accessors.h
	executor/base/dynamic_context.h executor/base/ext_sort.h
	executor/base/merge.h executor/base/seq_common.h
	executor/base/sequence.h executor/base/sorted_sequence.h
	executor/base/tuple.h executor/base/xptrChanneledMerge.h
	executor/base/xptr_sequence.h executor/base/xs_binary.h
	executor/base/xs_decimal_t.h
	executor/base/xs_fp_converter.h executor/base/xs_helper.h
	executor/base/xs_names.h executor/base/xs_uri.h
	executor/base/xsd.h

	# Source Files, from executor/base
	executor/base/PPBase.cpp executor/base/PPUtils.cpp
	executor/base/SequenceType.cpp
	executor/base/XMLDateTime.cpp executor/base/XPath.cpp
	executor/base/XPathOnSchema.cpp executor/base/bit_set.cpp
	executor/base/compare.cpp executor/base/dm_accessors.cpp
	executor/base/dynamic_context.cpp
	executor/base/ext_sort.cpp executor/base/merge.cpp
	executor/base/sequence.cpp
	executor/base/sorted_sequence.cpp executor/base/tuple.cpp
	executor/base/xptrChanneledMerge.cpp
	executor/base/xptr_sequence.cpp executor/base/xs_binary.cpp
	executor/base/xs_decimal_t.cpp
	executor/base/xs_fp_converter.cpp
	executor/base/xs_helper.cpp executor/base/xs_names.cpp
	executor/base/xs_uri.cpp executor/base/xsd.cpp

	# Header Files, from executor/base/decNumber
	executor/base/decNumber/decContext.h
	executor/base/decNumber/decDPD.h
	executor/base/decNumber/decNumber.h
	executor/base/decNumber/decNumberLocal.h
	executor/base/decNumber/decimal128.h
	executor/base/decNumber/decimal64.h

	# Source Files, from executor/base/decNumber
	executor/base/decNumber/decContext.c
	executor/base/decNumber/decNumber.c
	executor/base/decNumber/decimal128.c
	executor/base/decNumber/decimal64.c

	# Header Files, from executor/base/lip
	executor/base/lip/lip.h executor/base/lip/lip_class.h

	# Source Files, from executor/base/lip
	executor/base/lip/lip.c

	# Header Files, from executor/fo
	executor/fo/boolean_operations.h
	executor/fo/casting_operations.h
	executor/fo/comparison_operations.h
	executor/fo/dateTime_operations.h executor/fo/op_map.h
	executor/fo/other_fo.h executor/fo/string_operations.h

	# Source Files, from executor/fo
	executor/fo/boolean_operations.cpp
	executor/fo/casting_operations.cpp
	executor/fo/comparison_operations.cpp
	executor/fo/dateTime_operations.cpp executor/fo/op_map.cpp
	executor/fo/other_fo.cpp executor/fo/string_operations.cpp

	# Header Files, from executor/por2qep
	executor/por2qep/ext.h executor/por2qep/por2qep.h
	executor/por2qep/scheme_tree.h executor/por2qep/tok_pp.h

	# Source Files, from executor/por2qep
	executor/por2qep/ext.cpp executor/por2qep/por2qep.cpp
	executor/por2qep/scheme_tree.cpp
	executor/por2qep/tok_pp.cpp

	# Header Files, from executor/root
	executor/root/PPBulkLoad.h executor/root/PPCreateFtIndex.h
	executor/root/PPCreateIndex.h
	executor/root/PPCreateMetadata.h
	executor/root/PPDeleteDeep.h executor/root/PPDeleteUndeep.h
	executor/root/PPDropFtIndex.h executor/root/PPDropIndex.h
	executor/root/PPDropMetadata.h executor/root/PPDropModule.h
	executor/root/PPInsertBefore.h
	executor/root/PPInsertFollowing.h
	executor/root/PPInsertTo.h executor/root/PPLoadModule.h
	executor/root/PPQueryRoot.h executor/root/PPRename.h
	executor/root/PPReplace.h executor/root/PPRetrieveDS.h
	executor/root/PPRetrieveMetadata.h

	# Source Files, from executor/root
	executor/root/PPBulkLoad.cpp
	executor/root/PPCreateFtIndex.cpp
	executor/root/PPCreateIndex.cpp
	executor/root/PPCreateMetadata.cpp
	executor/root/PPDeleteDeep.cpp
	executor/root/PPDeleteUndeep.cpp
	executor/root/PPDropFtIndex.cpp
	executor/root/PPDropIndex.cpp
	executor/root/PPDropMetadata.cpp
	executor/root/PPDropModule.cpp
	executor/root/PPInsertBefore.cpp
	executor/root/PPInsertFollowing.cpp
	executor/root/PPInsertTo.cpp executor/root/PPLoadModule.cpp
	executor/root/PPQueryRoot.cpp executor/root/PPRename.cpp
	executor/root/PPReplace.cpp executor/root/PPRetrieveDS.cpp
	executor/root/PPRetrieveMetadata.cpp

	# Header Files, from executor/xqops
	executor/xqops/PPADFilter.h executor/xqops/PPAbsPath.h
	executor/xqops/PPAccessors.h executor/xqops/PPAggrFuncs.h
	executor/xqops/PPAxisAncestor.h
	executor/xqops/PPAxisAttribute.h
	executor/xqops/PPAxisChild.h
	executor/xqops/PPAxisDescendant.h executor/xqops/PPAxisFP.h
	executor/xqops/PPAxisParent.h executor/xqops/PPAxisSelf.h
	executor/xqops/PPAxisSibling.h
	executor/xqops/PPBooleanOps.h executor/xqops/PPCalculate.h
	executor/xqops/PPCheckpoint.h executor/xqops/PPConst.h
	executor/xqops/PPConstructors.h executor/xqops/PPDAFilter.h
	executor/xqops/PPDDO.h executor/xqops/PPDebug.h
	executor/xqops/PPDocInCol.h executor/xqops/PPError.h
	executor/xqops/PPExcept.h executor/xqops/PPExtFunCall.h
	executor/xqops/PPFilterEL.h executor/xqops/PPFnAccessors.h
	executor/xqops/PPFnDateTimeFuncs.h
	executor/xqops/PPFnDeepEqual.h
	executor/xqops/PPFnDocAvailable.h
	executor/xqops/PPFtHighlight.h
	executor/xqops/PPFtIndexScan.h executor/xqops/PPFtScan.h
	executor/xqops/PPFunCall.h
	executor/xqops/PPGeneralComparison.h executor/xqops/PPIf.h
	executor/xqops/PPIndexScan.h executor/xqops/PPIntersect.h
	executor/xqops/PPLet.h executor/xqops/PPNil.h
	executor/xqops/PPNodeComparison.h
	executor/xqops/PPNodeOps.h executor/xqops/PPNumericFuncs.h
	executor/xqops/PPOrderBy.h executor/xqops/PPPatMatch.h
	executor/xqops/PPPred.h executor/xqops/PPQName.h
	executor/xqops/PPRange.h executor/xqops/PPReturn.h
	executor/xqops/PPSLStub.h executor/xqops/PPSQL.h
	executor/xqops/PPSQLODBC.h executor/xqops/PPSResLStub.h
	executor/xqops/PPScan.h executor/xqops/PPSelect.h
	executor/xqops/PPSequence.h executor/xqops/PPSequenceOps.h
	executor/xqops/PPSequenceTypes.h
	executor/xqops/PPSpaceSequence.h executor/xqops/PPStore.h
	executor/xqops/PPStringFuncs.h
	executor/xqops/PPStringsCompare.h
	executor/xqops/PPSubsMatch.h executor/xqops/PPTest.h
	executor/xqops/PPTuple.h executor/xqops/PPUnion.h
	executor/xqops/PPUp.h executor/xqops/PPUriFuncs.h
	executor/xqops/PPVarDecl.h executor/xqops/PPVariable.h
	executor/xqops/PPXptr.h

	# Source Files, from executor/xqops
	executor/xqops/PPADFilter.cpp executor/xqops/PPAbsPath.cpp
	executor/xqops/PPAccessors.cpp
	executor/xqops/PPAggrFuncs.cpp
	executor/xqops/PPAxisAncestor.cpp
	executor/xqops/PPAxisAttribute.cpp
	executor/xqops/PPAxisChild.cpp
	executor/xqops/PPAxisDescendant.cpp
	executor/xqops/PPAxisFP.cpp executor/xqops/PPAxisParent.cpp
	executor/xqops/PPAxisSelf.cpp
	executor/xqops/PPAxisSibling.cpp
	executor/xqops/PPBooleanOps.cpp
	executor/xqops/PPCalculate.cpp
	executor/xqops/PPCheckpoint.cpp executor/xqops/PPConst.cpp
	executor/xqops/PPConstructors.cpp
	executor/xqops/PPDAFilter.cpp executor/xqops/PPDDO.cpp
	executor/xqops/PPDebug.cpp executor/xqops/PPDocInCol.cpp
	executor/xqops/PPError.cpp executor/xqops/PPExcept.cpp
	executor/xqops/PPExtFunCall.cpp
	executor/xqops/PPFilterEL.cpp
	executor/xqops/PPFnAccessors.cpp
	executor/xqops/PPFnDateTimeFuncs.cpp
	executor/xqops/PPFnDeepEqual.cpp
	executor/xqops/PPFnDocAvailable.cpp
	executor/xqops/PPFtHighlight.cpp
	executor/xqops/PPFtIndexScan.cpp
	executor/xqops/PPFtScan.cpp executor/xqops/PPFunCall.cpp
	executor/xqops/PPGeneralComparison.cpp
	executor/xqops/PPIf.cpp executor/xqops/PPIndexScan.cpp
	executor/xqops/PPIntersect.cpp executor/xqops/PPLet.cpp
	executor/xqops/PPNil.cpp
	executor/xqops/PPNodeComparison.cpp
	executor/xqops/PPNodeOps.cpp
	executor/xqops/PPNumericFuncs.cpp
	executor/xqops/PPOrderBy.cpp executor/xqops/PPPatMatch.cpp
	executor/xqops/PPPred.cpp executor/xqops/PPQName.cpp
	executor/xqops/PPRange.cpp executor/xqops/PPReturn.cpp
	executor/xqops/PPSLStub.cpp executor/xqops/PPSQL.cpp
	executor/xqops/PPSQLODBC.cpp executor/xqops/PPSResLStub.cpp
	executor/xqops/PPScan.cpp executor/xqops/PPSelect.cpp
	executor/xqops/PPSequence.cpp
	executor/xqops/PPSequenceOps.cpp
	executor/xqops/PPSequenceTypes.cpp
	executor/xqops/PPSpaceSequence.cpp
	executor/xqops/PPStore.cpp executor/xqops/PPStringFuncs.cpp
	executor/xqops/PPStringsCompare.cpp
	executor/xqops/PPSubsMatch.cpp executor/xqops/PPTest.cpp
	executor/xqops/PPTuple.cpp executor/xqops/PPUnion.cpp
	executor/xqops/PPUp.cpp executor/xqops/PPUriFuncs.cpp
	executor/xqops/PPVarDecl.cpp executor/xqops/PPVariable.cpp
	executor/xqops/PPXptr.cpp

	# Header Files, from ft
	ft/FTindex.h ft/FTsearch.h ft/ft_index_data.h

	# Source Files, from ft
	ft/FTindex.cpp ft/FTsearch.cpp ft/ft_index_data.cpp

	# Header Files, from idx
	idx/index_data.h idx/indexes.h

	# Source Files, from idx
	idx/index_data.cpp idx/indexes.cpp

	# Header Files, from idx/btree
	idx/btree/btintern.h idx/btree/btpage.h idx/btree/btree.h
	idx/btree/btstruct.h idx/btree/buff.h idx/btree/test.h

	# Source Files, from idx/btree
	idx/btree/auxi.cpp idx/btree/bisection.cpp
	idx/btree/btree.cpp idx/btree/btstruct.cpp
	idx/btree/buff.cpp idx/btree/cmp.cpp idx/btree/delete.cpp
	idx/btree/find.cpp idx/btree/insert.cpp
	idx/btree/test_cmp_bisection.cpp idx/btree/test_find.cpp
	idx/btree/test_helpers.cpp idx/btree/test_insert.cpp
	idx/btree/test_overall.cpp

	# Header Files, from locks
	locks/locks.h

	# Source Files, from locks
	locks/locks.cpp

	# Header Files, from log
	log/log.h

	# Source Files, from log
	log/hl_logical_log.cpp log/hl_phys_log.cpp

	# Header Files, from mo
	mo/micro.h

	# Source Files, from mo
	mo/micro.cpp mo/mo_write_utils.cpp

	# Header Files, from nid
	nid/lex.h nid/nid.h nid/nidalloc.h nid/numb_scheme.h

	# Source Files, from nid
	nid/lex.cpp nid/nidalloc.cpp nid/numb_scheme.cpp
	nid/test_lex.cpp nid/test_nbm.cpp

	# Header Files, from pq
	pq/chicken_panic.h pq/pq.h

	# Source Files, from pq
	pq/chicken_panic.cpp pq/modules.cpp pq/pq.cpp

	# Header Files, from pstr
	pstr/hh.h pstr/pstr.h pstr/pstr_long.h pstr/pstrblk.h
	pstr/utl.h

	# Source Files, from pstr
	pstr/hh.cpp pstr/pstr.cpp pstr/pstr_long.cpp pstr/test.cpp
	pstr/utl.cpp

	# Header Files, from rcv
	rcv/rcv_funcs.h

	# Source Files, from rcv
	rcv/rcv_funcs.cpp

	# Header Files, from sqp/popt
	sqp/popt/att.h sqp/popt/att_xpath.h sqp/popt/popt_plan.h
	sqp/popt/serialize2lr.h

	# Source Files, from sqp/popt
	sqp/popt/att.cpp sqp/popt/att_xpath.cpp
	sqp/popt/cpp_popt.cpp sqp/popt/popt_plan.cpp
	sqp/popt/serialize2lr.cpp

	# Header Files, from strings
	strings/char_iterator.h strings/e_string.h
	strings/e_string_iterator.h strings/strings.h
	strings/strings_base.h strings/utf8.h

	# Source Files, from strings
	strings/e_string.cpp strings/e_string_iterator.cpp
	strings/strings.cpp strings/strings_base.cpp
	strings/utf8.cpp

	# Header Files, from structures
	structures/indirection.h structures/metadata.h
	structures/nodes.h structures/pers_map.h
	structures/schema.h

	# Source Files, from structures
	structures/indirection.cpp structures/metadata.cpp
	structures/nodes.cpp structures/schema.cpp

	# Header Files, from triggers
	triggers/triggers.h triggers/triggers_data.h
	triggers/triggers_utils.h

	# Source Files, from triggers
	triggers/triggers.cpp triggers/triggers_data.cpp
	triggers/triggers_utils.cpp

	# Header Files, from updates
	updates/updates.h

	# Source Files, from updates
	updates/deletions.cpp updates/insertions.cpp
	updates/other_updates.cpp updates/replace.cpp
	updates/updates.cpp

	# Header Files, from vmm
	vmm/os_exceptions.h vmm/vmm.h

	# Source Files, from vmm
	vmm/os_exceptions.cpp vmm/vmm.cpp vmm/vmm_test.cpp

	# Header Files, from xqp
	xqp/ANTLRToken.h xqp/AST.h xqp/ASTNodeTypes.h xqp/SORAST.h
	xqp/XQuerytoLR.h xqp/context.h xqp/flwr.h xqp/parserutils.h
	xqp/quantifier.h xqp/types.h

	# Source Files, from xqp
	xqp/AST.cpp xqp/XQuerytoLR.cpp xqp/context.cpp xqp/flwr.cpp
	xqp/parserutils.cpp xqp/print.cpp xqp/quantifier.cpp
	xqp/xqp_test.cpp
)

SOURCE_GROUP ( "Header Files 1" FILES
	cl_client.h client_core.h socket_client.h tr_common_funcs.h
	tr_functions.h tr_globals.h tr_utils.h
)

SOURCE_GROUP ( "Source Files 1" FILES
	cl_client.cpp rcv_db.cpp socket_client.cpp tr.cpp
	tr_common_funcs.cpp tr_functions.cpp tr_globals.cpp
	tr_rcv.cpp
)

SOURCE_GROUP ( "AUTH" )

SOURCE_GROUP ( "AUTH\\Header Files 2" FILES
	auth/auc.h
)

SOURCE_GROUP ( "AUTH\\Source Files 2" FILES
	auth/auc.cpp
)

SOURCE_GROUP ( "CRMUTILS" )

SOURCE_GROUP ( "CRMUTILS\\Header Files 3" FILES
	crmutils/crmutils.h crmutils/exec_output.h
	crmutils/node_utils.h crmutils/scheme_utils.h
	crmutils/str_matcher.h
)

SOURCE_GROUP ( "CRMUTILS\\Source Files 3" FILES
	crmutils/debug_utils.cpp crmutils/exec_output.cpp
	crmutils/file_utils.cpp crmutils/namespace_utils.cpp
	crmutils/node_utils.cpp crmutils/print_utils.cpp
	crmutils/scheme_utils.cpp crmutils/str_matcher.cpp
	crmutils/system_tables.cpp
)

SOURCE_GROUP ( "EXECUTOR" )

SOURCE_GROUP ( "EXECUTOR\\BASE" )

SOURCE_GROUP ( "EXECUTOR\\BASE\\Header Files 4" FILES
	executor/base/PPBase.h executor/base/PPOperations.h
	executor/base/PPUtils.h executor/base/SequenceType.h
	executor/base/XMLDateTime.h executor/base/XPath.h
	executor/base/XPathOnSchema.h executor/base/bit_set.h
	executor/base/compare.h executor/base/dm_accessors.h
	executor/base/dynamic_context.h executor/base/ext_sort.h
	executor/base/merge.h executor/base/seq_common.h
	executor/base/sequence.h executor/base/sorted_sequence.h
	executor/base/tuple.h executor/base/xptrChanneledMerge.h
	executor/base/xptr_sequence.h executor/base/xs_binary.h
	executor/base/xs_decimal_t.h
	executor/base/xs_fp_converter.h executor/base/xs_helper.h
	executor/base/xs_names.h executor/base/xs_uri.h
	executor/base/xsd.h
)

SOURCE_GROUP ( "EXECUTOR\\BASE\\Source Files 4" FILES
	executor/base/PPBase.cpp executor/base/PPUtils.cpp
	executor/base/SequenceType.cpp
	executor/base/XMLDateTime.cpp executor/base/XPath.cpp
	executor/base/XPathOnSchema.cpp executor/base/bit_set.cpp
	executor/base/compare.cpp executor/base/dm_accessors.cpp
	executor/base/dynamic_context.cpp
	executor/base/ext_sort.cpp executor/base/merge.cpp
	executor/base/sequence.cpp
	executor/base/sorted_sequence.cpp executor/base/tuple.cpp
	executor/base/xptrChanneledMerge.cpp
	executor/base/xptr_sequence.cpp executor/base/xs_binary.cpp
	executor/base/xs_decimal_t.cpp
	executor/base/xs_fp_converter.cpp
	executor/base/xs_helper.cpp executor/base/xs_names.cpp
	executor/base/xs_uri.cpp executor/base/xsd.cpp
)

SOURCE_GROUP ( "EXECUTOR\\BASE\\DECNUMBER" )

SOURCE_GROUP ( "EXECUTOR\\BASE\\DECNUMBER\\Header Files 5" FILES
	executor/base/decNumber/decContext.h
	executor/base/decNumber/decDPD.h
	executor/base/decNumber/decNumber.h
	executor/base/decNumber/decNumberLocal.h
	executor/base/decNumber/decimal128.h
	executor/base/decNumber/decimal64.h
)

SOURCE_GROUP ( "EXECUTOR\\BASE\\DECNUMBER\\Source Files 5" FILES
	executor/base/decNumber/decContext.c
	executor/base/decNumber/decNumber.c
	executor/base/decNumber/decimal128.c
	executor/base/decNumber/decimal64.c
)

SOURCE_GROUP ( "EXECUTOR\\BASE\\LIP" )

SOURCE_GROUP ( "EXECUTOR\\BASE\\LIP\\Header Files 6" FILES
	executor/base/lip/lip.h executor/base/lip/lip_class.h
)

SOURCE_GROUP ( "EXECUTOR\\BASE\\LIP\\Source Files 6" FILES
	executor/base/lip/lip.c
)

SOURCE_GROUP ( "EXECUTOR\\FO" )

SOURCE_GROUP ( "EXECUTOR\\FO\\Header Files 7" FILES
	executor/fo/boolean_operations.h
	executor/fo/casting_operations.h
	executor/fo/comparison_operations.h
	executor/fo/dateTime_operations.h executor/fo/op_map.h
	executor/fo/other_fo.h executor/fo/string_operations.h
)

SOURCE_GROUP ( "EXECUTOR\\FO\\Source Files 7" FILES
	executor/fo/boolean_operations.cpp
	executor/fo/casting_operations.cpp
	executor/fo/comparison_operations.cpp
	executor/fo/dateTime_operations.cpp executor/fo/op_map.cpp
	executor/fo/other_fo.cpp executor/fo/string_operations.cpp
)

SOURCE_GROUP ( "EXECUTOR\\POR2QEP" )

SOURCE_GROUP ( "EXECUTOR\\POR2QEP\\Header Files 8" FILES
	executor/por2qep/ext.h executor/por2qep/por2qep.h
	executor/por2qep/scheme_tree.h executor/por2qep/tok_pp.h
)

SOURCE_GROUP ( "EXECUTOR\\POR2QEP\\Source Files 8" FILES
	executor/por2qep/ext.cpp executor/por2qep/por2qep.cpp
	executor/por2qep/scheme_tree.cpp
	executor/por2qep/tok_pp.cpp
)

SOURCE_GROUP ( "EXECUTOR\\ROOT" )

SOURCE_GROUP ( "EXECUTOR\\ROOT\\Header Files 9" FILES
	executor/root/PPBulkLoad.h executor/root/PPCreateFtIndex.h
	executor/root/PPCreateIndex.h
	executor/root/PPCreateMetadata.h
	executor/root/PPDeleteDeep.h executor/root/PPDeleteUndeep.h
	executor/root/PPDropFtIndex.h executor/root/PPDropIndex.h
	executor/root/PPDropMetadata.h executor/root/PPDropModule.h
	executor/root/PPInsertBefore.h
	executor/root/PPInsertFollowing.h
	executor/root/PPInsertTo.h executor/root/PPLoadModule.h
	executor/root/PPQueryRoot.h executor/root/PPRename.h
	executor/root/PPReplace.h executor/root/PPRetrieveDS.h
	executor/root/PPRetrieveMetadata.h
)

SOURCE_GROUP ( "EXECUTOR\\ROOT\\Source Files 9" FILES
	executor/root/PPBulkLoad.cpp
	executor/root/PPCreateFtIndex.cpp
	executor/root/PPCreateIndex.cpp
	executor/root/PPCreateMetadata.cpp
	executor/root/PPDeleteDeep.cpp
	executor/root/PPDeleteUndeep.cpp
	executor/root/PPDropFtIndex.cpp
	executor/root/PPDropIndex.cpp
	executor/root/PPDropMetadata.cpp
	executor/root/PPDropModule.cpp
	executor/root/PPInsertBefore.cpp
	executor/root/PPInsertFollowing.cpp
	executor/root/PPInsertTo.cpp executor/root/PPLoadModule.cpp
	executor/root/PPQueryRoot.cpp executor/root/PPRename.cpp
	executor/root/PPReplace.cpp executor/root/PPRetrieveDS.cpp
	executor/root/PPRetrieveMetadata.cpp
)

SOURCE_GROUP ( "EXECUTOR\\XQOPS" )

SOURCE_GROUP ( "EXECUTOR\\XQOPS\\Header Files 10" FILES
	executor/xqops/PPADFilter.h executor/xqops/PPAbsPath.h
	executor/xqops/PPAccessors.h executor/xqops/PPAggrFuncs.h
	executor/xqops/PPAxisAncestor.h
	executor/xqops/PPAxisAttribute.h
	executor/xqops/PPAxisChild.h
	executor/xqops/PPAxisDescendant.h executor/xqops/PPAxisFP.h
	executor/xqops/PPAxisParent.h executor/xqops/PPAxisSelf.h
	executor/xqops/PPAxisSibling.h
	executor/xqops/PPBooleanOps.h executor/xqops/PPCalculate.h
	executor/xqops/PPCheckpoint.h executor/xqops/PPConst.h
	executor/xqops/PPConstructors.h executor/xqops/PPDAFilter.h
	executor/xqops/PPDDO.h executor/xqops/PPDebug.h
	executor/xqops/PPDocInCol.h executor/xqops/PPError.h
	executor/xqops/PPExcept.h executor/xqops/PPExtFunCall.h
	executor/xqops/PPFilterEL.h executor/xqops/PPFnAccessors.h
	executor/xqops/PPFnDateTimeFuncs.h
	executor/xqops/PPFnDeepEqual.h
	executor/xqops/PPFnDocAvailable.h
	executor/xqops/PPFtHighlight.h
	executor/xqops/PPFtIndexScan.h executor/xqops/PPFtScan.h
	executor/xqops/PPFunCall.h
	executor/xqops/PPGeneralComparison.h executor/xqops/PPIf.h
	executor/xqops/PPIndexScan.h executor/xqops/PPIntersect.h
	executor/xqops/PPLet.h executor/xqops/PPNil.h
	executor/xqops/PPNodeComparison.h
	executor/xqops/PPNodeOps.h executor/xqops/PPNumericFuncs.h
	executor/xqops/PPOrderBy.h executor/xqops/PPPatMatch.h
	executor/xqops/PPPred.h executor/xqops/PPQName.h
	executor/xqops/PPRange.h executor/xqops/PPReturn.h
	executor/xqops/PPSLStub.h executor/xqops/PPSQL.h
	executor/xqops/PPSQLODBC.h executor/xqops/PPSResLStub.h
	executor/xqops/PPScan.h executor/xqops/PPSelect.h
	executor/xqops/PPSequence.h executor/xqops/PPSequenceOps.h
	executor/xqops/PPSequenceTypes.h
	executor/xqops/PPSpaceSequence.h executor/xqops/PPStore.h
	executor/xqops/PPStringFuncs.h
	executor/xqops/PPStringsCompare.h
	executor/xqops/PPSubsMatch.h executor/xqops/PPTest.h
	executor/xqops/PPTuple.h executor/xqops/PPUnion.h
	executor/xqops/PPUp.h executor/xqops/PPUriFuncs.h
	executor/xqops/PPVarDecl.h executor/xqops/PPVariable.h
	executor/xqops/PPXptr.h
)

SOURCE_GROUP ( "EXECUTOR\\XQOPS\\Source Files 10" FILES
	executor/xqops/PPADFilter.cpp executor/xqops/PPAbsPath.cpp
	executor/xqops/PPAccessors.cpp
	executor/xqops/PPAggrFuncs.cpp
	executor/xqops/PPAxisAncestor.cpp
	executor/xqops/PPAxisAttribute.cpp
	executor/xqops/PPAxisChild.cpp
	executor/xqops/PPAxisDescendant.cpp
	executor/xqops/PPAxisFP.cpp executor/xqops/PPAxisParent.cpp
	executor/xqops/PPAxisSelf.cpp
	executor/xqops/PPAxisSibling.cpp
	executor/xqops/PPBooleanOps.cpp
	executor/xqops/PPCalculate.cpp
	executor/xqops/PPCheckpoint.cpp executor/xqops/PPConst.cpp
	executor/xqops/PPConstructors.cpp
	executor/xqops/PPDAFilter.cpp executor/xqops/PPDDO.cpp
	executor/xqops/PPDebug.cpp executor/xqops/PPDocInCol.cpp
	executor/xqops/PPError.cpp executor/xqops/PPExcept.cpp
	executor/xqops/PPExtFunCall.cpp
	executor/xqops/PPFilterEL.cpp
	executor/xqops/PPFnAccessors.cpp
	executor/xqops/PPFnDateTimeFuncs.cpp
	executor/xqops/PPFnDeepEqual.cpp
	executor/xqops/PPFnDocAvailable.cpp
	executor/xqops/PPFtHighlight.cpp
	executor/xqops/PPFtIndexScan.cpp
	executor/xqops/PPFtScan.cpp executor/xqops/PPFunCall.cpp
	executor/xqops/PPGeneralComparison.cpp
	executor/xqops/PPIf.cpp executor/xqops/PPIndexScan.cpp
	executor/xqops/PPIntersect.cpp executor/xqops/PPLet.cpp
	executor/xqops/PPNil.cpp
	executor/xqops/PPNodeComparison.cpp
	executor/xqops/PPNodeOps.cpp
	executor/xqops/PPNumericFuncs.cpp
	executor/xqops/PPOrderBy.cpp executor/xqops/PPPatMatch.cpp
	executor/xqops/PPPred.cpp executor/xqops/PPQName.cpp
	executor/xqops/PPRange.cpp executor/xqops/PPReturn.cpp
	executor/xqops/PPSLStub.cpp executor/xqops/PPSQL.cpp
	executor/xqops/PPSQLODBC.cpp executor/xqops/PPSResLStub.cpp
	executor/xqops/PPScan.cpp executor/xqops/PPSelect.cpp
	executor/xqops/PPSequence.cpp
	executor/xqops/PPSequenceOps.cpp
	executor/xqops/PPSequenceTypes.cpp
	executor/xqops/PPSpaceSequence.cpp
	executor/xqops/PPStore.cpp executor/xqops/PPStringFuncs.cpp
	executor/xqops/PPStringsCompare.cpp
	executor/xqops/PPSubsMatch.cpp executor/xqops/PPTest.cpp
	executor/xqops/PPTuple.cpp executor/xqops/PPUnion.cpp
	executor/xqops/PPUp.cpp executor/xqops/PPUriFuncs.cpp
	executor/xqops/PPVarDecl.cpp executor/xqops/PPVariable.cpp
	executor/xqops/PPXptr.cpp
)

SOURCE_GROUP ( "FT" )

SOURCE_GROUP ( "FT\\Header Files 11" FILES
	ft/FTindex.h ft/FTsearch.h ft/ft_index_data.h
)

SOURCE_GROUP ( "FT\\Source Files 11" FILES
	ft/FTindex.cpp ft/FTsearch.cpp ft/ft_index_data.cpp
)

SOURCE_GROUP ( "IDX" )

SOURCE_GROUP ( "IDX\\Header Files 12" FILES
	idx/index_data.h idx/indexes.h
)

SOURCE_GROUP ( "IDX\\Source Files 12" FILES
	idx/index_data.cpp idx/indexes.cpp
)

SOURCE_GROUP ( "IDX\\BTREE" )

SOURCE_GROUP ( "IDX\\BTREE\\Header Files 13" FILES
	idx/btree/btintern.h idx/btree/btpage.h idx/btree/btree.h
	idx/btree/btstruct.h idx/btree/buff.h idx/btree/test.h
)

SOURCE_GROUP ( "IDX\\BTREE\\Source Files 13" FILES
	idx/btree/auxi.cpp idx/btree/bisection.cpp
	idx/btree/btree.cpp idx/btree/btstruct.cpp
	idx/btree/buff.cpp idx/btree/cmp.cpp idx/btree/delete.cpp
	idx/btree/find.cpp idx/btree/insert.cpp
	idx/btree/test_cmp_bisection.cpp idx/btree/test_find.cpp
	idx/btree/test_helpers.cpp idx/btree/test_insert.cpp
	idx/btree/test_overall.cpp
)

SOURCE_GROUP ( "LOCKS" )

SOURCE_GROUP ( "LOCKS\\Header Files 14" FILES
	locks/locks.h
)

SOURCE_GROUP ( "LOCKS\\Source Files 14" FILES
	locks/locks.cpp
)

SOURCE_GROUP ( "LOG" )

SOURCE_GROUP ( "LOG\\Header Files 15" FILES
	log/log.h
)

SOURCE_GROUP ( "LOG\\Source Files 15" FILES
	log/hl_logical_log.cpp log/hl_phys_log.cpp
)

SOURCE_GROUP ( "MO" )

SOURCE_GROUP ( "MO\\Header Files 16" FILES
	mo/micro.h
)

SOURCE_GROUP ( "MO\\Source Files 16" FILES
	mo/micro.cpp mo/mo_write_utils.cpp
)

SOURCE_GROUP ( "NID" )

SOURCE_GROUP ( "NID\\Header Files 17" FILES
	nid/lex.h nid/nid.h nid/nidalloc.h nid/numb_scheme.h
)

SOURCE_GROUP ( "NID\\Source Files 17" FILES
	nid/lex.cpp nid/nidalloc.cpp nid/numb_scheme.cpp
	nid/test_lex.cpp nid/test_nbm.cpp
)

SOURCE_GROUP ( "PQ" )

SOURCE_GROUP ( "PQ\\Header Files 18" FILES
	pq/chicken_panic.h pq/pq.h
)

SOURCE_GROUP ( "PQ\\Source Files 18" FILES
	pq/chicken_panic.cpp pq/modules.cpp pq/pq.cpp
)

SOURCE_GROUP ( "PSTR" )

SOURCE_GROUP ( "PSTR\\Header Files 19" FILES
	pstr/hh.h pstr/pstr.h pstr/pstr_long.h pstr/pstrblk.h
	pstr/utl.h
)

SOURCE_GROUP ( "PSTR\\Source Files 19" FILES
	pstr/hh.cpp pstr/pstr.cpp pstr/pstr_long.cpp pstr/test.cpp
	pstr/utl.cpp
)

SOURCE_GROUP ( "RCV" )

SOURCE_GROUP ( "RCV\\Header Files 20" FILES
	rcv/rcv_funcs.h
)

SOURCE_GROUP ( "RCV\\Source Files 20" FILES
	rcv/rcv_funcs.cpp
)

SOURCE_GROUP ( "SQP" )

SOURCE_GROUP ( "SQP\\POPT" )

SOURCE_GROUP ( "SQP\\POPT\\Header Files 21" FILES
	sqp/popt/att.h sqp/popt/att_xpath.h sqp/popt/popt_plan.h
	sqp/popt/serialize2lr.h
)

SOURCE_GROUP ( "SQP\\POPT\\Source Files 21" FILES
	sqp/popt/att.cpp sqp/popt/att_xpath.cpp
	sqp/popt/cpp_popt.cpp sqp/popt/popt_plan.cpp
	sqp/popt/serialize2lr.cpp
)

SOURCE_GROUP ( "STRINGS" )

SOURCE_GROUP ( "STRINGS\\Header Files 22" FILES
	strings/char_iterator.h strings/e_string.h
	strings/e_string_iterator.h strings/strings.h
	strings/strings_base.h strings/utf8.h
)

SOURCE_GROUP ( "STRINGS\\Source Files 22" FILES
	strings/e_string.cpp strings/e_string_iterator.cpp
	strings/strings.cpp strings/strings_base.cpp
	strings/utf8.cpp
)

SOURCE_GROUP ( "STRUCTURES" )

SOURCE_GROUP ( "STRUCTURES\\Header Files 23" FILES
	structures/indirection.h structures/metadata.h
	structures/nodes.h structures/pers_map.h
	structures/schema.h
)

SOURCE_GROUP ( "STRUCTURES\\Source Files 23" FILES
	structures/indirection.cpp structures/metadata.cpp
	structures/nodes.cpp structures/schema.cpp
)

SOURCE_GROUP ( "TRIGGERS" )

SOURCE_GROUP ( "TRIGGERS\\Header Files 24" FILES
	triggers/triggers.h triggers/triggers_data.h
	triggers/triggers_utils.h
)

SOURCE_GROUP ( "TRIGGERS\\Source Files 24" FILES
	triggers/triggers.cpp triggers/triggers_data.cpp
	triggers/triggers_utils.cpp
)

SOURCE_GROUP ( "UPDATES" )

SOURCE_GROUP ( "UPDATES\\Header Files 25" FILES
	updates/updates.h
)

SOURCE_GROUP ( "UPDATES\\Source Files 25" FILES
	updates/deletions.cpp updates/insertions.cpp
	updates/other_updates.cpp updates/replace.cpp
	updates/updates.cpp
)

SOURCE_GROUP ( "VMM" )

SOURCE_GROUP ( "VMM\\Header Files 26" FILES
	vmm/os_exceptions.h vmm/vmm.h
)

SOURCE_GROUP ( "VMM\\Source Files 26" FILES
	vmm/os_exceptions.cpp vmm/vmm.cpp vmm/vmm_test.cpp
)

SOURCE_GROUP ( "XQP" )

SOURCE_GROUP ( "XQP\\Header Files 27" FILES
	xqp/ANTLRToken.h xqp/AST.h xqp/ASTNodeTypes.h xqp/SORAST.h
	xqp/XQuerytoLR.h xqp/context.h xqp/flwr.h xqp/parserutils.h
	xqp/quantifier.h xqp/types.h
)

SOURCE_GROUP ( "XQP\\Source Files 27" FILES
	xqp/AST.cpp xqp/XQuerytoLR.cpp xqp/context.cpp xqp/flwr.cpp
	xqp/parserutils.cpp xqp/print.cpp xqp/quantifier.cpp
	xqp/xqp_test.cpp
)

