#
# Makefile for llmgr (GNU make)
#

PP = ../../..


VPATH = $(PP)/kernel/sm


include $(PP)/Makefile.include

all: liblock_mgr$(LIB_EXT) 
	@echo ===================================================================
	@echo Lock Manager has been Done
	@echo ===================================================================



ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(PP)/kernel/sm/depend
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
