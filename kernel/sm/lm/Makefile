#
# Makefile for llmgr (GNU make)
#

PP = ../../..

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifeq ($(words $(wildcard depend)), 0)
ifneq ($(findstring depend, $(MAKECMDGOALS)), depend)
$(error Type "make depend" first)
endif
endif
endif


INCLUDE_DIRS = $(PP)/kernel/common $(PP)/kernel/common/u $(PP)/driver/c .. $(PP)/kernel/sm \

VPATH = .


include $(PP)/Makefile.include

all: liblock_mgr$(LIB_EXT) 
	@echo ===================================================================
	@echo Lock Manager has been Done
	@echo ===================================================================



include $(PP)/Makefile.depend

depend:
	$(DEPACT)


ifneq ($(words $(wildcard depend)), 0)
include depend
endif


COMMON_FILES_LIB = $(PP)/kernel/common/libcommon_files$(LIB_EXT)


################################################################################
# LL_MGR_LIB                                                                          #
################################################################################
liblock_mgr$(LIB_EXT): lm_globals$(OBJ_EXT) 	\
                       lock_table$(OBJ_EXT)		\
                       trans_table$(OBJ_EXT)	\
                       TransCB$(OBJ_EXT)
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
	-$(REMOVE) vc60.idb
	-$(REMOVE) vc60.pdb
	-$(REMOVE) vc70.idb
	-$(REMOVE) vc70.pdb
	-$(REMOVE) *.map
	-$(REMOVE) *.pdb
	-$(REMOVE) *.SUP
