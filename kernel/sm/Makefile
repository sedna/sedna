#
# Makefile for SM
#

PP = ../..

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifeq ($(words $(wildcard depend)), 0)
ifneq ($(findstring depend, $(MAKECMDGOALS)), depend)
$(error Type "make depend" first)
endif
endif
endif


INCLUDE_DIRS = . $(PP)/kernel/common $(PP)/kernel/common/u $(PP)/driver/c	\
               $(PP)/kernel/common/ph $(PP)/libs/expat bufmgr plmgr	llmgr lm

VPATH = . bufmgr


include $(PP)/Makefile.include

all: $(PP)/bin/se_sm$(EXE_EXT) $(PP)/bin/se_cdb$(EXE_EXT) $(PP)/bin/se_ddb$(EXE_EXT) $(PP)/bin/se_smsd$(EXE_EXT)
	@echo ===================================================================
	@echo SM DONE
	@echo ===================================================================

test: sm_register_test$(EXE_EXT)
	@echo ===================================================================
	@echo SM_REGISTER_TEST DONE
	@echo ===================================================================

include $(PP)/Makefile.depend

depend:
	$(DEPACT)
	$(MAKE) -C plmgr depend
	$(MAKE) -C llmgr depend
	$(MAKE) -C lm depend


ifneq ($(words $(wildcard depend)), 0)
include depend
endif

SYSTEM_LIBS = Advapi32$(LIB_EXT) 

COMMON_FILES_LIB = $(PP)/kernel/common/libcommon_files$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
COMMON_FILES_S = $(COMMON_FILES_LIB)
else
COMMON_FILES_S = -L$(PP)/kernel/common -lcommon_files
endif

PHYS_LOG_MGR_LIB = plmgr/libsm_phys_log_mgr$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
PHYS_LOG_MGR_S = $(PHYS_LOG_MGR_LIB)
else
PHYS_LOG_MGR_S = -Lplmgr -lsm_phys_log_mgr
endif


LOGICAL_LOG_MGR_LIB = llmgr/liblogical_log_mgr$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
LOGICAL_LOG_MGR_S = $(LOGICAL_LOG_MGR_LIB)
else
LOGICAL_LOG_MGR_S = -Lllmgr -llogical_log_mgr
endif


LOCK_MGR_LIB = lm/liblock_mgr$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
LOCK_MGR_S = $(LOCK_MGR_LIB)
else
LOCK_MGR_S = -Llm -llock_mgr
endif


################################################################################
# sm                                                                           #
################################################################################

$(PHYS_LOG_MGR_LIB): libraries
$(LOGICAL_LOG_MGR_LIB): libraries
$(LOCK_MGR_LIB): libraries

SM_OBJS =  sm$(OBJ_EXT)				\
           sm_functions$(OBJ_EXT)	\
           sm_globals$(OBJ_EXT)		\
           blk_mngmt$(OBJ_EXT)		\
           bm_core$(OBJ_EXT)		\
           bm_functions$(OBJ_EXT)	\
           bm_rcv$(OBJ_EXT)			\
           trmgr$(OBJ_EXT)

$(PP)/bin/se_sm$(EXE_EXT): $(SM_OBJS)				\
                           $(COMMON_FILES_LIB)		\
                           $(PHYS_LOG_MGR_LIB)		\
                           $(LOGICAL_LOG_MGR_LIB)	\
                           $(LOCK_MGR_LIB)
	$(LD) $(LFLAGS) $(LDOUT)$@ $(SM_OBJS) $(PHYS_LOG_MGR_S) $(LOGICAL_LOG_MGR_S) $(LOCK_MGR_S) $(COMMON_FILES_S) $(LSLIBS)

################################################################################
# cdb                                                                          #
################################################################################
CDB_OBJS = cdb$(OBJ_EXT)			\
           sm_functions$(OBJ_EXT)	\
           cdb_globals$(OBJ_EXT)	\
           sm_globals$(OBJ_EXT)		\
           db_utils$(OBJ_EXT)		\
           blk_mngmt$(OBJ_EXT)		\
           bm_core$(OBJ_EXT)		\
           bm_functions$(OBJ_EXT)	\
           trmgr$(OBJ_EXT)			\
           bm_rcv$(OBJ_EXT)

$(PP)/bin/se_cdb$(EXE_EXT): $(CDB_OBJS)			\
                            $(COMMON_FILES_LIB)	\
                            $(PHYS_LOG_MGR_LIB)	\
                            $(LOGICAL_LOG_MGR_LIB)
	$(LD) $(LFLAGS) $(LDOUT)$@ $(CDB_OBJS) $(COMMON_FILES_S) $(PHYS_LOG_MGR_S) $(LOGICAL_LOG_MGR_S) $(LSLIBS)

################################################################################
# ddb                                                                          #
################################################################################
DDB_OBJS = ddb$(OBJ_EXT)			\
           db_utils$(OBJ_EXT)


$(PP)/bin/se_ddb$(EXE_EXT): $(DDB_OBJS)			\
                            $(COMMON_FILES_LIB)
	$(LD) $(LFLAGS) $(LDOUT)$@ $(DDB_OBJS) $(COMMON_FILES_S) $(LSLIBS)

################################################################################
# smsd                                                                         #
################################################################################
SMSD_OBJS = smsd$(OBJ_EXT)

$(PP)/bin/se_smsd$(EXE_EXT): $(SMSD_OBJS) $(COMMON_FILES_LIB)
	$(LD) $(LFLAGS) $(LDOUT)$@ $(SMSD_OBJS) $(COMMON_FILES_S) $(LSLIBS)


################################################################################
# libraries
################################################################################
libraries:
	$(MAKE) -C plmgr
	$(MAKE) -C llmgr
	$(MAKE) -C lm

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) sm_register_test$(EXE_EXT)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) *.map
	-$(REMOVE) *.pdb
	-$(REMOVE) sm.SUP
	-$(REMOVE) cdb.map
	-$(REMOVE) cdb.pdb
	-$(REMOVE) cdb.SUP
	-$(REMOVE) vc60.idb
	-$(REMOVE) vc60.pdb
	-$(REMOVE) vc70.idb
	-$(REMOVE) vc70.pdb
	$(MAKE) -C plmgr clean
	$(MAKE) -C llmgr clean
	$(MAKE) -C lm clean
