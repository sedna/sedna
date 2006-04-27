#
# Makefile for XQuery Executor
#

PP = ../../..

VPATH = base fo por2qep root xqops $(PP)/kernel/tr


include $(PP)/Makefile.include

EXECUTOR_OBJECTS   =   tuple$(OBJ_EXT)					\
                       sequence$(OBJ_EXT)				\
                       xptr_sequence$(OBJ_EXT)			\
                       sorted_sequence$(OBJ_EXT)		\
                       dm_accessors$(OBJ_EXT)			\
                       e_string$(OBJ_EXT)				\
                       ext_sort$(OBJ_EXT)				\
                       compare$(OBJ_EXT)				\
                       merge$(OBJ_EXT)					\
                       xptrChanneledMerge$(OBJ_EXT)		\
                       PPStaticContext$(OBJ_EXT)		\
                       PPBase$(OBJ_EXT)					\
                       PPUtils$(OBJ_EXT)				\
                       XPath$(OBJ_EXT)					\
                       XPathOnSchema$(OBJ_EXT)			\
                       SequenceType$(OBJ_EXT)			\
                       PPSLStub$(OBJ_EXT)				\
                       PPSResLStub$(OBJ_EXT)			\
                       PPIf$(OBJ_EXT)					\
                       PPAbsPath$(OBJ_EXT)				\
                       PPIndexScan$(OBJ_EXT)			\
                       PPReturn$(OBJ_EXT)				\
                       PPSelect$(OBJ_EXT)				\
                       PPLet$(OBJ_EXT)					\
                       PPVariable$(OBJ_EXT)				\
                       PPConst$(OBJ_EXT)				\
                       PPNil$(OBJ_EXT)					\
                       PPCast$(OBJ_EXT)					\
                       PPDDO$(OBJ_EXT)					\
                       PPSequence$(OBJ_EXT)				\
                       PPRange$(OBJ_EXT)				\
                       PPSequenceOps$(OBJ_EXT)			\
                       PPNodeOps$(OBJ_EXT)				\
                       PPBooleanOps$(OBJ_EXT)			\
                       PPConstructors$(OBJ_EXT)			\
                       PPSpaceSequence$(OBJ_EXT)		\
                       PPGeneralComparison$(OBJ_EXT)	\
                       PPAccessors$(OBJ_EXT)			\
                       PPFnAccessors$(OBJ_EXT)			\
                       PPNodeComparison$(OBJ_EXT)		\
                       PPStore$(OBJ_EXT)				\
                       PPAxisSelf$(OBJ_EXT)				\
                       PPAxisChild$(OBJ_EXT)			\
                       PPAxisFP$(OBJ_EXT)				\
                       PPAxisSibling$(OBJ_EXT)			\
                       PPAxisAttribute$(OBJ_EXT)		\
                       PPAxisDescendant$(OBJ_EXT)		\
                       PPAxisAncestor$(OBJ_EXT)			\
                       PPAxisParent$(OBJ_EXT)			\
                       PPCalculate$(OBJ_EXT)			\
                       PPConGen$(OBJ_EXT)				\
                       PPInstanceOf$(OBJ_EXT)			\
                       PPFunCall$(OBJ_EXT)				\
                       PPExtFunCall$(OBJ_EXT)			\
                       PPSubsMatch$(OBJ_EXT)			\
                       PPPatMatch$(OBJ_EXT)				\
                       PPTest$(OBJ_EXT)					\
                       PPError$(OBJ_EXT)				\
                       PPDocInCol$(OBJ_EXT)				\
                       PPTuple$(OBJ_EXT)				\
                       PPPred$(OBJ_EXT)					\
                       PPScan$(OBJ_EXT)					\
                       PPUnion$(OBJ_EXT)				\
                       PPIntersect$(OBJ_EXT)			\
                       PPExcept$(OBJ_EXT)				\
                       PPADFilter$(OBJ_EXT)				\
                       PPDAFilter$(OBJ_EXT)				\
                       PPUp$(OBJ_EXT)					\
                       PPStringFuncs$(OBJ_EXT)			\
                       casting_operations$(OBJ_EXT)		\
                       numeric_operations$(OBJ_EXT)		\
                       boolean_operations$(OBJ_EXT)		\
                       comparison_operations$(OBJ_EXT)	\
                       string_operations$(OBJ_EXT)		\
                       PPQueryRoot$(OBJ_EXT)			\
                       PPInsertTo$(OBJ_EXT)				\
                       PPInsertFollowing$(OBJ_EXT)		\
                       PPInsertBefore$(OBJ_EXT)			\
                       PPDeleteDeep$(OBJ_EXT)			\
                       PPReplace$(OBJ_EXT)				\
                       PPDeleteUndeep$(OBJ_EXT)			\
                       PPRename$(OBJ_EXT)				\
                       PPBulkLoad$(OBJ_EXT)				\
                       PPCreateIndex$(OBJ_EXT)			\
                       PPCreateMetadata$(OBJ_EXT)		\
                       PPDropMetadata$(OBJ_EXT)			\
                       PPDropIndex$(OBJ_EXT)			\
                       PPRetrieveDS$(OBJ_EXT)			\
                       PPRetrieveMetadata$(OBJ_EXT)		\
                       por2qep$(OBJ_EXT)				\
                       ext$(OBJ_EXT)					\
                       calc_altern$(OBJ_EXT)			\
                       scheme_tree$(OBJ_EXT)			

ifeq ($(SQL_CONNECTION),1)
EXECUTOR_OBJECTS  +=   PPSQL$(OBJ_EXT)					\
                       PPSQLODBC$(OBJ_EXT)
endif
ifeq ($(ENABLE_DTSEARCH),1)
EXECUTOR_OBJECTS  +=   PPFtIndexScan$(OBJ_EXT) \
					   PPFtScan$(OBJ_EXT) \
					   PPFtHighlight$(OBJ_EXT) \
					   PPCreateFtIndex$(OBJ_EXT) \
					   PPDropFtIndex$(OBJ_EXT)
endif


all: libexecutor$(LIB_EXT)
	@echo ===================================================================
	@echo EXECUTOR DONE
	@echo ===================================================================



ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(PP)/kernel/tr/depend
endif


gen_num_ops$(OBJ_EXT): gen_num_ops.c
	$(CC) $(CFLAGS_CHICKEN_EXE) $<

gen_num_ops$(EXE_EXT): gen_num_ops$(OBJ_EXT)
	$(LD) $(LFLAGS_CHICKEN_EXE) $(LDOUT)$@ $^ $(LSLIBS_CHICKEN_EXE)

numeric_operations.h numeric_operations.cpp: gen_num_ops$(EXE_EXT)
	./gen_num_ops$(EXE_EXT)

gen_compar_ops$(OBJ_EXT): gen_compar_ops.c
	$(CC) $(CFLAGS_CHICKEN_EXE) $<

gen_compar_ops$(EXE_EXT): gen_compar_ops$(OBJ_EXT)
	$(LD) $(LFLAGS_CHICKEN_EXE) $(LDOUT)$@ $^ $(LSLIBS_CHICKEN_EXE)

comparison_operations.h comparison_operations.cpp: gen_compar_ops$(EXE_EXT)
	./gen_compar_ops$(EXE_EXT)

gen_calc_altern$(OBJ_EXT): gen_calc_altern.c
	$(CC) $(CFLAGS_CHICKEN_EXE) $<

gen_calc_altern$(EXE_EXT): gen_calc_altern$(OBJ_EXT)
	$(LD) $(LFLAGS_CHICKEN_EXE) $(LDOUT)$@ $^ $(LSLIBS_CHICKEN_EXE)

calc_altern.cpp: gen_calc_altern$(EXE_EXT)
	./gen_calc_altern$(EXE_EXT)


calc_altern$(OBJ_EXT): calc_altern.cpp \
  ../../kernel/tr/executor/fo/boolean_operations.h \
  ../../kernel/tr/executor/base/PPBase.h \
  ../../kernel/tr/executor/base/sequence.h \
  ../../kernel/tr/executor/base/compare.h \
  ../../kernel/tr/executor/base/tuple.h ../../kernel/common/counted_ptr.h \
  ../../kernel/tr/structures/nodes.h ../../kernel/common/base.h \
  ../../kernel/common/sedna.h ../../kernel/common/u/u.h \
  ../../driver/c/sp_defs.h \
  ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../kernel/common/utils.h \
  ../../kernel/common/u/uprocess.h ../../kernel/common/u/u.h \
  ../../kernel/common/u/uthread.h ../../kernel/common/u/usecurity.h \
  ../../kernel/common/sm_vmm_data.h \
  ../../kernel/common/xptr.h ../../kernel/common/base.h \
  ../../kernel/common/errdbg/exceptions.h ../../kernel/common/utils.h \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h \
  ../../kernel/tr/nid/numb_scheme.h ../../kernel/common/xptr.h \
  ../../kernel/tr/crmutils/exec_output.h \
  ../../kernel/common/u/usocket.h ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/sp.h ../../kernel/common/u/usocket.h \
  ../../kernel/common/u/uutils.h ../../kernel/common/base.h \
  ../../kernel/tr/nid/nid.h ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/tr/executor/base/decimal.h \
  ../../kernel/tr/executor/base/date.h \
  ../../kernel/common/errdbg/exceptions.h ../../kernel/tr/pstr/pstr.h \
  ../../kernel/tr/executor/base/SequenceType.h \
  ../../kernel/tr/executor/base/XPath.h \
  ../../kernel/tr/executor/por2qep/scheme_tree.h \
  ../../kernel/tr/vmm/vmm.h ../../kernel/common/xptr.h \
  ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/tr/vmm/os_exceptions.h ../../kernel/common/u/u.h \
  ../../kernel/common/SSMMsg.h ../../kernel/common/u/usem.h \
  ../../kernel/common/u/uthread.h ../../kernel/common/u/ushm.h \
  ../../kernel/common/persistent_db_data.h \
  ../../kernel/common/u/uthread.h \
  ../../kernel/tr/executor/base/PPStaticContext.h \
  ../../kernel/tr/structures/schema.h ../../kernel/tr/structures/nodes.h \
  ../../kernel/tr/structures/pers_map.h \
  ../../kernel/common/ph/pers_heap.h ../../kernel/common/u/u.h \
  ../../kernel/common/u/usecurity.h numeric_operations.h \
  ../../kernel/tr/executor/base/PPBase.h \
  comparison_operations.h \
  ../../kernel/tr/executor/fo/string_operations.h \
  ../../kernel/tr/executor/xqops/PPCalculate.h \
  ../../kernel/tr/executor/base/PPBase.h \
  ../../kernel/tr/executor/base/PPUtils.h \
  ../../kernel/tr/executor/base/PPBase.h \
  ../../kernel/tr/crmutils/node_utils.h ../../kernel/common/xptr.h \
  ../../kernel/tr/structures/nodes.h ../../kernel/tr/vmm/vmm.h \
  ../../kernel/tr/structures/schema.h \
  ../../kernel/tr/executor/base/dm_accessors.h \
  ../../kernel/tr/executor/fo/boolean_operations.h \
  ../../kernel/tr/executor/por2qep/scheme_tree.h

comparison_operations$(OBJ_EXT): comparison_operations.cpp \
  comparison_operations.h \
  ../../kernel/tr/executor/base/PPBase.h \
  ../../kernel/tr/executor/base/sequence.h \
  ../../kernel/tr/executor/base/compare.h \
  ../../kernel/tr/executor/base/tuple.h ../../kernel/common/counted_ptr.h \
  ../../kernel/tr/structures/nodes.h ../../kernel/common/base.h \
  ../../kernel/common/sedna.h ../../kernel/common/u/u.h \
  \
  ../../driver/c/sp_defs.h \
  ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../kernel/common/utils.h \
  ../../kernel/common/u/uprocess.h ../../kernel/common/u/u.h \
  ../../kernel/common/u/uthread.h ../../kernel/common/u/usecurity.h \
  ../../kernel/common/sm_vmm_data.h \
  ../../kernel/common/xptr.h ../../kernel/common/base.h \
  ../../kernel/common/errdbg/exceptions.h ../../kernel/common/utils.h \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h \
  ../../kernel/tr/nid/numb_scheme.h ../../kernel/common/xptr.h \
  ../../kernel/tr/crmutils/exec_output.h \
  ../../kernel/common/u/usocket.h ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/sp.h ../../kernel/common/u/usocket.h \
  ../../kernel/common/u/uutils.h ../../kernel/common/base.h \
  ../../kernel/tr/nid/nid.h ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/tr/executor/base/decimal.h \
  ../../kernel/tr/executor/base/date.h \
  ../../kernel/common/errdbg/exceptions.h ../../kernel/tr/pstr/pstr.h \
  ../../kernel/tr/executor/base/SequenceType.h \
  ../../kernel/tr/executor/base/XPath.h \
  ../../kernel/tr/executor/por2qep/scheme_tree.h \
  ../../kernel/tr/vmm/vmm.h ../../kernel/common/xptr.h \
  ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/tr/vmm/os_exceptions.h ../../kernel/common/u/u.h \
  ../../kernel/common/SSMMsg.h ../../kernel/common/u/usem.h \
  ../../kernel/common/u/uthread.h ../../kernel/common/u/ushm.h \
  ../../kernel/common/persistent_db_data.h \
  ../../kernel/common/u/uthread.h \
  ../../kernel/tr/executor/base/PPStaticContext.h \
  ../../kernel/tr/structures/schema.h ../../kernel/tr/structures/nodes.h \
  ../../kernel/tr/structures/pers_map.h \
  ../../kernel/common/ph/pers_heap.h ../../kernel/common/u/u.h \
  ../../kernel/common/u/usecurity.h numeric_operations.h \
  ../../kernel/tr/executor/fo/boolean_operations.h \
  ../../kernel/tr/executor/base/PPBase.h \
  ../../kernel/tr/executor/fo/string_operations.h \
  ../../kernel/tr/executor/fo/casting_operations.h \
  ../../kernel/tr/nid/numb_scheme.h

numeric_operations$(OBJ_EXT): numeric_operations.cpp \
  numeric_operations.h ../../kernel/tr/executor/base/PPBase.h \
  ../../kernel/tr/executor/base/sequence.h \
  ../../kernel/tr/executor/base/compare.h \
  ../../kernel/tr/executor/base/tuple.h ../../kernel/common/counted_ptr.h \
  ../../kernel/tr/structures/nodes.h ../../kernel/common/base.h \
  ../../kernel/common/sedna.h ../../kernel/common/u/u.h \
  \
  ../../driver/c/sp_defs.h \
  ../../kernel/common/mmgr/se_alloc.h \
  ../../kernel/common/errdbg/event_log.h ../../kernel/common/sedna.h \
  ../../kernel/common/utils.h \
  ../../kernel/common/u/uprocess.h ../../kernel/common/u/u.h \
  ../../kernel/common/u/uthread.h ../../kernel/common/u/usecurity.h \
  ../../kernel/common/sm_vmm_data.h \
  ../../kernel/common/xptr.h ../../kernel/common/base.h \
  ../../kernel/common/errdbg/exceptions.h ../../kernel/common/utils.h \
  ../../kernel/common/errdbg/d_printf.h ../../kernel/common/u/u.h \
  ../../kernel/common/error_codes.h \
  ../../kernel/common/errdbg/event_log.h \
  ../../kernel/tr/nid/numb_scheme.h ../../kernel/common/xptr.h \
  ../../kernel/tr/crmutils/exec_output.h \
  ../../kernel/common/u/usocket.h ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/common/sp.h ../../kernel/common/u/usocket.h \
  ../../kernel/common/u/uutils.h ../../kernel/common/base.h \
  ../../kernel/tr/nid/nid.h ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/tr/executor/base/decimal.h \
  ../../kernel/tr/executor/base/date.h \
  ../../kernel/common/errdbg/exceptions.h ../../kernel/tr/pstr/pstr.h \
  ../../kernel/tr/executor/base/SequenceType.h \
  ../../kernel/tr/executor/base/XPath.h \
  ../../kernel/tr/executor/por2qep/scheme_tree.h \
  ../../kernel/tr/vmm/vmm.h ../../kernel/common/xptr.h \
  ../../kernel/common/errdbg/exceptions.h \
  ../../kernel/tr/vmm/os_exceptions.h ../../kernel/common/u/u.h \
  ../../kernel/common/SSMMsg.h ../../kernel/common/u/usem.h \
  ../../kernel/common/u/uthread.h ../../kernel/common/u/ushm.h \
  ../../kernel/common/persistent_db_data.h \
  ../../kernel/common/u/uthread.h \
  ../../kernel/tr/executor/base/PPStaticContext.h \
  ../../kernel/tr/structures/schema.h ../../kernel/tr/structures/nodes.h \
  ../../kernel/tr/structures/pers_map.h \
  ../../kernel/common/ph/pers_heap.h ../../kernel/common/u/u.h \
  ../../kernel/common/u/usecurity.h \
  ../../kernel/tr/executor/fo/casting_operations.h \
  ../../kernel/tr/executor/base/PPBase.h \
  ../../kernel/tr/executor/base/PPUtils.h \
  ../../kernel/tr/executor/base/PPBase.h \
  ../../kernel/tr/crmutils/node_utils.h ../../kernel/common/xptr.h \
  ../../kernel/tr/structures/nodes.h ../../kernel/tr/vmm/vmm.h \
  ../../kernel/tr/structures/schema.h \
  ../../kernel/tr/executor/base/dm_accessors.h



################################################################################
# EXECUTOR                                                                     #
################################################################################
libexecutor$(LIB_EXT): $(EXECUTOR_OBJECTS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) *$(LIB_EXT)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) gen_num_ops$(EXE_EXT)
	-$(REMOVE) gen_compar_ops$(EXE_EXT)
	-$(REMOVE) gen_calc_altern$(EXE_EXT)
	-$(REMOVE) numeric_operations.h 
	-$(REMOVE) numeric_operations.cpp
	-$(REMOVE) comparison_operations.h
	-$(REMOVE) comparison_operations.cpp
	-$(REMOVE) calc_altern.cpp
	-$(REMOVE) gen_calc_altern.c
	-$(REMOVE) gen_calc_altern.exp
	-$(REMOVE) gen_compar_ops.c
	-$(REMOVE) gen_compar_ops.exp
	-$(REMOVE) gen_num_ops.c
	-$(REMOVE) gen_num_ops.exp

